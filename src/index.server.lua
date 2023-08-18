local luauParser = if script then require(script.Parent.luauParser) else require("./luauParser/init")
local lexer = luauParser.lexer
local parser = luauParser.parser
local ast = luauParser.ast

local eventScript = [[
local idkLib = game.Workspace.Multiplayer:GetMapVals()
local l = workspace.Multiplayer:GetMapVals()
local test = idkLib
local Lib = l
local acallingfunction = workspace:Help()  -- mostly testing the parser
acallingfunction.call() -- mostly testing the parser
test.Script.MoveWater(Vector3.new(1, 2, 3), 10)
idkLib.Script.MovePart()
local testinter = `hi`
local testInterExpr = `hi{1 + 2}`
l.Script.MovePart()

local function Test(i)  -- mostly testing the parser
    wait(1)
    Lib.Script.MoveWater()
    task.wait(2)
end

Lib.btnFuncs[1] = function()
    print()
end

test()

Lib.Script.MovePart(Lib.Map.hentai, Vector3.new(1, 2, 3), 2, true)
]]

local luau_globalMaps = {
    ["workspace"] = "game.Workspace"
}
local parser_keywords = { -- Used to identify specific variables
    FE2_LIB = "$FE2_LIB"
}
local parser_functions_allocate = {}

local scope_variables: {[string]: any} = {}
local timelinesData: {[string]: {any}} = {main = {}}
local timelinesOutput: {any} = {}

-- EventScript->Timelines related functions
function addTimelineData(data: {[string]: string})
    local function countTable(tbl)
        local i = 0
        for _ in tbl do
            i += 1
        end
        return i
    end
    if countTable(data) == 0 then
        return
    end

    table.insert(timelinesOutput, data)
end

local tokenized = lexer.new(eventScript):scan()
local parseToAst = parser.new(tokenized)
parseToAst:parseChunk()
local astRoot = parseToAst.result

local currentScope: number = 0 -- main scope is 0
local delayTimePerScope: {[number]: number} = {}

local function isSelfCall(functionName: string, firstArgument: string?): boolean
    if not firstArgument then
        return false
    end

    local nameSplit = string.split(functionName, ".")
    local argumentSplit = string.split(firstArgument, ".")
    if #nameSplit - #argumentSplit ~= 1 then
        return false
    end

    for i = 1, #argumentSplit do
        if nameSplit[i] ~= argumentSplit[i] then
            return false
        end
    end

    return true
end

local function visitNode(astNode)
    if astNode.kind == ast.Kind.Local then
        local toBeAssigned = visitNode(astNode.value[1][1])
        local assignedValue = visitNode(astNode.value[2][1])
        scope_variables[toBeAssigned] = assignedValue
        if type(assignedValue) == "table" then
            if assignedValue.functionName == "game.Workspace.Multiplayer.GetMapVals" and isSelfCall(assignedValue.functionName, assignedValue.functionArguments[1]) then
                print("Detected FE2's Lib declaration, assigning keyword")
                scope_variables[toBeAssigned] = parser_keywords.FE2_LIB
            end
        end

        return
    elseif astNode.kind == ast.Kind.Binding then
        local binding = table.create(#astNode.value)
        for i, nameValue in astNode.value do
            if i == "n" then
                continue
            end
            binding[i] = visitNode(nameValue)
        end
        return table.concat(binding, ".")
    elseif astNode.kind == ast.Kind.Name then
        local replacedVar = scope_variables[astNode.value] or luau_globalMaps[astNode.value]
        if replacedVar then
            return replacedVar
        end
        return astNode.value
    elseif astNode.kind == ast.Kind.FunctionCall then
        local callingFunctionName = visitNode(astNode.value[1])
        local functionArguments = table.create(#astNode.value[2])
        for i, argumentNode in astNode.value[2] do
            functionArguments[i] = visitNode(argumentNode)
        end

        if callingFunctionName == "Vector3.new" then
            -- Lune doesn't have Vector3.new() but it has vector()
            print("Transforming to native Vector3.new")
            return (if Vector3 then Vector3.new else getfenv().vector)(unpack(functionArguments))
        elseif callingFunctionName == "CFrame.new" then

        end
        print(callingFunctionName)

        if callingFunctionName == `{parser_keywords.FE2_LIB}.Script.MovePart` then
            print("doing move part with", functionArguments)
            addTimelineData({
                XFrame_Function = "MovePart",
                XFrame_Timestamp = delayTimePerScope[currentScope] or 0,
                Object = functionArguments[1],
                Translate = functionArguments[2],
                XFrame_Length = functionArguments[3],
                UseLocalSpace = functionArguments[4],
                EasingStyle = functionArguments[5],
                EasingDirection = functionArguments[6]
            })
        elseif callingFunctionName == `{parser_keywords.FE2_LIB}.Script.MoveWater` then
            print("doing move water with", functionArguments)
            addTimelineData({
                XFrame_Function = "MovePart",
                XFrame_Timestamp = delayTimePerScope[currentScope] or 0,
                Object = functionArguments[1],
                Translate = functionArguments[2],
                XFrame_Length = functionArguments[3],
                UseLocalSpace = functionArguments[4],
            })
        elseif callingFunctionName == "task.wait" or callingFunctionName == "wait" then
            print(`delaying time in scope {currentScope} by {functionArguments[1]}`)
            if not delayTimePerScope[currentScope] then
                delayTimePerScope[currentScope] = 0
            end
            delayTimePerScope[currentScope] += tonumber(functionArguments[1]) or 0
        end

        return {functionName = callingFunctionName, functionArguments = functionArguments}
    elseif astNode.kind == ast.Kind.IndexName then
        local name = table.create(#astNode.value)
        for i, nameValue in astNode.value do
            if i == "n" then
                continue
            end
            name[i] = visitNode(nameValue)
            if type(name[i]) == "table" then
                parser_functions_allocate[name[i].functionName] = name[i]
                name[i] = `$FUNC_ALLOCATE({name[i].functionName})`
            end

            if i == 1 then
                local replacedVar = scope_variables[name[i]] or luau_globalMaps[name[i]]
                if replacedVar then
                    name[i] = `$VAR_{name[i]}`
                end
            end
        end
        return table.concat(name, ".")
    elseif astNode.kind == ast.Kind.SelfIndexName then
        return `{visitNode(astNode.value[1])}.{visitNode(astNode.value[2])}`
    elseif astNode.kind == ast.Kind.LocalFunction then
        print(astNode)
        local functionDeclareName = visitNode(astNode.value[1])
        if not timelinesData[functionDeclareName] then
            timelinesData[functionDeclareName] = {}
        end
    -- Simple types
    elseif table.find({ast.Kind.True, ast.Kind.False, ast.Kind.Nil, ast.Kind.Number, ast.Kind.String}, astNode.kind) then
        if astNode.kind == ast.Kind.True then
            return true
        elseif astNode.kind == ast.Kind.False then
            return false
        elseif astNode.kind == ast.Kind.Nil then
            return nil
        end
        return astNode.value
    elseif astNode.kind == ast.Kind.InterpolatedString then
        -- ignore
        return
    else
        print(astNode.kind, "not implemented")
        return
    end
end
for _, astNode in astRoot.children do
    visitNode(astNode)
end

print('results dump\n- scope vars:', scope_variables, "-timeline data:", timelinesOutput)
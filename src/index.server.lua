local luau = require("./luau")
local lexer = if script then require(script.Parent.lexer) else require("./lexer/init")
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

test()

Lib.Script.MovePart(Lib.Map.hentai, Vector3.new(1, 2, 3), 2, true)
]]

local luau_reserveds = {"if", "then", "end", "for", "in", "local"}
local luau_globalMaps = {
    ["workspace"] = "game.Workspace"
}
local parser_keywords = { -- Used to identify specific variables
    FE2_LIB = "$FE2_LIB"
}
local parser_functions_allocate = {}
local lex_errors = {
    INVALID_SYNTAX = "Invalid syntax",
    LUAU_RESERVED = "Reserved keyword"
}

local variables = {
    lib_name = nil, -- Holds the current name of "Lib" in the scope
    scope_variables = {}
}
local isLocalAssignMark = false
local isAssigning = false
local isIndexing = false
local isCalling = false
local ignorePotentialNewLine = false
local indexLayer = 1
local argumentsCount = 0
local bracketsCount = 0

local holdDatas = {}
local indexing = {}
local arguments = {}
local timelinesOutput = {}

local USE_PROPER_PARSING = true

function dump(o)
    if type(o) == 'table' then
       local s = '{ '
       for k,v in pairs(o) do
          if type(k) ~= 'number' then k = '"'..k..'"' end
          s = s .. '['..k..'] = ' .. dump(v) .. ','
       end
       return s .. '} '
    else
       return tostring(o)
    end
end

-- Lexer-related functions
function cleanLexContent(content: string): string
    return string.gsub(content, "%s", "")
end

function resolveVariables(entries: {string}): {string}
    local resolved: {string} = {}
    for _, content in entries do
        table.insert(resolved, variables.scope_variables[content] or content)
    end

    return resolved
end

function addIndex(indexName: string)
    if not indexing[indexLayer] then
        indexing[indexLayer] = {}
    end
    table.insert(indexing[indexLayer], indexName)
end

function getLastIndex()
    if not indexing[indexLayer] then
        indexing[indexLayer] = {}
    end
    return indexing[indexLayer][#indexing[indexLayer]]
end

function pushIndexes()
    local indexes = table.clone(indexing[indexLayer])
    table.clear(indexing[indexLayer])
    return indexes
end

function pushArgument(arg: string)
    if not arguments[argumentsCount] then
        arguments[argumentsCount] = ""
    end
    arguments[argumentsCount] ..= arg
end

function pullAllArguments()
    local pulledArguments = table.clone(arguments)
    table.clear(arguments)
    return pulledArguments
end

function resolveArguments(arguments: {string}): {string}
    local resolvedArgs: {string} = table.create(#arguments) -- Since we know the size, we only allocate the necessary size to store
    for i, argumentString in arguments do
        --[[local marks = {
            Vector3 = false,
            CFrame = false,
            Color3 = false
        }
        for token, content in lexer.scan(argumentString) do
            -- in case they do custom variables naming or stuffs
            content = variables.scope_variables[content] or content

            if content == "CFrame" then
                marks.CFrame = true
            elseif content == "Vector3" then
                marks.Vector3 = true
            end

            if token == "operator" and content == "." then
                
            end
        end]]
        local success, gotData = pcall(function()
            local loadedBytecode = luau.compile(`return {argumentString}`)
            if loadedBytecode then
                return loadedBytecode()
            end
        end)
        print(i, success, gotData)
        resolvedArgs[i] = if success then gotData else "nil"
    end

    return resolvedArgs
end

function throwLexError(errorContent: string, type: string)
    error(`Lex error: {type} - {errorContent}`)
end

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

if USE_PROPER_PARSING then
    local luau_lexer = require("luaul/lexer")
    local luau_parser = require("luaul/parser")
    local luau_ast = require("luaul/ast")
    local luau_tokens = luau_lexer.new(eventScript):scan()
    local parser = luau_parser.new(luau_tokens)
    parser:parseChunk()
    local astRoot = parser.result

    local currentScope = 0 -- main scope is 0
    local delayTimePerScope = {}

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
        if astNode.kind == luau_ast.Kind.Local then
            local toBeAssigned = visitNode(astNode.value[1][1])
            local assignedValue = visitNode(astNode.value[2][1])
            variables.scope_variables[toBeAssigned] = assignedValue
            if type(assignedValue) == "table" then
                if assignedValue.functionName == "game.Workspace.Multiplayer.GetMapVals" and isSelfCall(assignedValue.functionName, assignedValue.functionArguments[1]) then
                    print("Detected FE2's Lib declaration, assigning keyword")
                    variables.scope_variables[toBeAssigned] = parser_keywords.FE2_LIB
                end
            end

            return
        elseif astNode.kind == luau_ast.Kind.Binding then
            local binding = table.create(#astNode.value)
            for i, nameValue in astNode.value do
                if i == "n" then
                    continue
                end
                binding[i] = visitNode(nameValue)
            end
            return table.concat(binding, ".")
        elseif astNode.kind == luau_ast.Kind.Name then
            local replacedVar = variables.scope_variables[astNode.value] or luau_globalMaps[astNode.value]
            if replacedVar then
                return replacedVar
            end
            return astNode.value
        elseif astNode.kind == luau_ast.Kind.FunctionCall then
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
                    XFrame_Timestamp = delayTimePerScope[currentScope],
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
                    XFrame_Timestamp = delayTimePerScope[currentScope],
                    Object = functionArguments[1],
                    Translate = functionArguments[2],
                    XFrame_Length = functionArguments[3],
                    UseLocalSpace = functionArguments[4],
                })
            elseif callingFunctionName == "task.wait" or callingFunctionName == "wait" then
                print(`delaying time in scope {currentScope} by {functionArguments[1]}`)
                delayTimePerScope[currentScope] += tonumber(functionArguments[1]) or 0
            end

            return {functionName = callingFunctionName, functionArguments = functionArguments}
        elseif astNode.kind == luau_ast.Kind.IndexName then
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
                    local replacedVar = variables.scope_variables[name[i]] or luau_globalMaps[name[i]]
                    if replacedVar then
                        name[i] = `$VAR_{name[i]}`
                    end
                end
            end
            return table.concat(name, ".")
        elseif astNode.kind == luau_ast.Kind.SelfIndexName then
            return `{visitNode(astNode.value[1])}.{visitNode(astNode.value[2])}`
        -- Simple types
        elseif table.find({luau_ast.Kind.True, luau_ast.Kind.False, luau_ast.Kind.Nil, luau_ast.Kind.Number, luau_ast.Kind.String}, astNode.kind) then
            if astNode.kind == luau_ast.Kind.True then
                return true
            elseif astNode.kind == luau_ast.Kind.False then
                return false
            elseif astNode.kind == luau_ast.Kind.Nil then
                return nil
            end
            return astNode.value
        elseif astNode.kind == luau_ast.Kind.InterpolatedString then
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

    print('results dump\n- scope vars:', variables.scope_variables, "-timeline data:", timelinesOutput)
else
    for token, content in lexer.scan(eventScript) do
        content = cleanLexContent(content)

        print(`-content: {content}\n token: {token}`)
        if not isLocalAssignMark and content == "local" then
            isLocalAssignMark = true
        end
        if not isIndexing and not isCalling and (token == "iden" or token == "builtin") then
            if table.find(luau_reserveds, content) and (isLocalAssignMark and content ~= "local") then
                throwLexError(`Cannot assign reserved keyword {content}`, lex_errors.LUAU_RESERVED)
            end

            if isLocalAssignMark then
                isAssigning = true
                isLocalAssignMark = false
                holdDatas[1] = "assign" -- We are assigning a variable
                holdDatas[2] = content -- The variable name
                ignorePotentialNewLine = true
            else
                holdDatas[1] = "keyword"
                holdDatas[2] = content
            end
        end
        if holdDatas[1] == "keyword" and token == "operator" then
            if content == "." or content == ":" then
                isIndexing = true
                holdDatas[1] = "indexing"
                addIndex(holdDatas[2])
            elseif content ~= "=" then
                throwLexError(`Incomplete statement`, lex_errors.INVALID_SYNTAX)
            else
                holdDatas[1] = "assign" -- Now we know we are assigning a variable
            end
        end
        if isAssigning then
            if token == "operator" then
                if content ~= "=" then
                    throwLexError(`Cannot use {content} in an assignment`, lex_errors.INVALID_SYNTAX)
                end

                isIndexing = true
                isAssigning = false
            elseif token == "number" then
                throwLexError(`Error idk`, lex_errors.INVALID_SYNTAX)
            elseif token == "builtin" or token == "iden" then
                if holdDatas[2] == "local" then
                    holdDatas[2] = content
                else
                    if ignorePotentialNewLine then
                        ignorePotentialNewLine = false
                    else
                        isAssigning = false
                    end
                end
            end
    elseif isIndexing then
            local lastIndex = getLastIndex()
            if lastIndex then
                if (lastIndex == "." or lastIndex == ":") then
                    if token ~= "builtin" and token ~= "iden" then
                        throwLexError(`Cannot use {content} in an assignment`, lex_errors.INVALID_SYNTAX)
                    elseif table.find(luau_reserveds, content) then
                        throwLexError(`Cannot use reserved keyword ({content}) as index`, lex_errors.LUAU_RESERVED)
                    end
                else
                    if token == "builtin" or token == "iden" then
                        isIndexing = false
                        holdDatas[3] = pushIndexes()
                    elseif token ~= "operator" then
                        throwLexError(`Cannot use {content} in an assignment`, lex_errors.INVALID_SYNTAX)
                    end
                end
            end
            addIndex(content)

            if content == ")" or content == "()" then
                isIndexing = false
                holdDatas[3] = pushIndexes()
                print("Finished indexing")
            elseif content == "(" then
                isIndexing = false
                isCalling = true -- Only used when a function is called with arguments

                print("Entering arguments")
                local currentIndexes = pushIndexes()
                holdDatas[1] = "calling"
                holdDatas[2] = table.move(currentIndexes, 1, #currentIndexes - 1, 1)
                holdDatas[3] = nil
                argumentsCount = 1
                bracketsCount = 1
            end
        elseif isCalling then
            if content == "," then
                print('split', bracketsCount)
                if bracketsCount == 1 then
                    argumentsCount += 1
                else
                    pushArgument(content)
                end
            elseif token == "operator" then
                if content == "(" then
                    bracketsCount += 1
                    print(content, bracketsCount)
                    pushArgument(content)
                elseif content == ")" then
                    bracketsCount -= 1
                    print(content, bracketsCount)
                    if bracketsCount == 0 then
                        isCalling = false
                        holdDatas[3] = pullAllArguments()
                    else
                        pushArgument(content)
                    end
                else
                    pushArgument(content)
                end
            else
                pushArgument(content)
            end
        end

        if holdDatas[1] == "assign" and holdDatas[3] then
            if holdDatas[#holdDatas] == "()" then
                -- Lib detection
                local indexes = resolveVariables(holdDatas[3])
                if table.concat(indexes, "") == "workspace.Multiplayer.GetMapVals:Invoke()" then
                    variables.lib_name = holdDatas[2]
                end
            end
            variables.scope_variables[holdDatas[2]] = table.concat(holdDatas[3], "")

            table.clear(holdDatas)
            isIndexing = false
        elseif holdDatas[1] == "calling" and holdDatas[2] and holdDatas[3] then
            -- boi this is "fun"
            local indexes = resolveVariables(holdDatas[2])
            print(dump(holdDatas[2]),' resolved vars to: ', dump(indexes))

            -- The reason for 1, 3, 5 check and stuffs is because the lexer also
            -- Add "." and ":" to the indexes, so we need to ignore them
            -- So Lib.Map.Script would be {"Lib", ".", "Map", ".", "Script"}
            -- Hence the odd numbers

            -- Handles Lib.Script.
            if indexes[1] == "Lib" and indexes[3] == "Script" then
                if indexes[5] == "MoveWater" and indexes[6] == "(" then
                    local resolvedArgs = resolveArguments(holdDatas[3])
                    print('resolved args', dump(resolvedArgs))
                end
            end
        end
    end
    print(`results dump:\n-hold datas:{dump(holdDatas)}\n-indexing datas:{dump(indexing)}\n-scope vars:{dump(variables.scope_variables)}\n-argument datas:{dump(arguments)}`)
end
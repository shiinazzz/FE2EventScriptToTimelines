local luau = require("./luau")
local tokenizer = require("./luauAst/Tokenizer")
local parser = require("./luauAst/Parser")
local eventScript = [[
local idkLib = game.Workspace.Multiplayer:GetMapVals()
local l = workspace.Multiplayer:GetMapVals()
local test = idkLib
local Lib = l
local acallingfunction = workspace:Help()
acallingfunction.call()
test.Script.MoveWater(Vector3.new(1, 2, 3), 10)
idkLib.Script.MovePart()
l.Script.MovePart()

local function Test(i)
    wait(1)
    Lib.Script.MoveWater()
    task.wait(2)
end

test()
]]

local luau_reserveds = { "if", "then", "end", "for", "in", "local" }
local luau_globalMaps = {
	["workspace"] = "game.Workspace",
}
local parser_keywords = { -- Used to identify specific variables
	FE2_LIB = "$FE2_LIB",
}
local parser_functions_allocate = {}
local lex_errors = {
	INVALID_SYNTAX = "Invalid syntax",
	LUAU_RESERVED = "Reserved keyword",
}

local variables = {
	lib_name = nil, -- Holds the current name of "Lib" in the scope
	scope_variables = {},
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
	if type(o) == "table" then
		local s = "{ "
		for k, v in pairs(o) do
			if type(k) ~= "number" then
				k = '"' .. k .. '"'
			end
			s = s .. "[" .. k .. "] = " .. dump(v) .. ","
		end
		return s .. "} "
	else
		return tostring(o)
	end
end
-- EventScript->Timelines related functions
function addTimelineData(data: { [string]: string })
	table.insert(timelinesOutput, data)
end

local parser = parser.new(eventScript)
local astRoot = parser:parse().root

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
			if
				assignedValue.functionName == "game.Workspace.Multiplayer.GetMapVals"
				and isSelfCall(assignedValue.functionName, assignedValue.functionArguments[1])
			then
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
		end
		print(callingFunctionName)

		if callingFunctionName == `{parser_keywords.FE2_LIB}.Script.MovePart` then
			print("doing move part with", functionArguments)
		elseif callingFunctionName == `{parser_keywords.FE2_LIB}.Script.MoveWater` then
			print("doing move part with", functionArguments)
		elseif callingFunctionName == "task.wait" or callingFunctionName == "wait" then
		end

		return { functionName = callingFunctionName, functionArguments = functionArguments }
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
	elseif
		table.find(
			{ luau_ast.Kind.True, luau_ast.Kind.False, luau_ast.Kind.Nil, luau_ast.Kind.Number, luau_ast.Kind.String },
			astNode.kind
		)
	then
		return astNode.value
	else
		print(astNode.kind, "not implemented")
		return
	end
end

local AstVisitor = setmetatable({}, {
    __index = {
        visit = function(self, node)
            print(node.class_name)
            if node.class_name == "AstStatBlock" then
                local statBody = node.body
                for _, statElement in statBody.elements do
                    statElement:visit(self)
                end
            elseif node.class_name == "AstStatLocal" then
                --print(node)
                local locals = {}
                for i = 1, math.min(#node.vars.elements, #node.values.elements) do
                    locals[node.vars.elements[i]] = node.values.elements[i]
                end
                print(locals)
            elseif node.class_name == "AstStatExpression" then
                --print("expr", node.expression)
            end
        end
    }
})
--print(astRoot)
print(astRoot:visit(AstVisitor))

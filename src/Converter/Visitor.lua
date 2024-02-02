-- Visitor
-- Helper class for Converter
-- cutymeo (ari)

local DEBUG = false
local SyntaxNode = require(script.Parent.LuauAstBuilder.SyntaxNode)

-- Nodes
export type BinaryNode = SyntaxNode.SyntaxNode & {
	Children: {ExpressionNode}
}
export type IdentifierNode = SyntaxNode.SyntaxNode & {
	Value: string
}
export type IndexIdentifierNode = SyntaxNode.SyntaxNode & {
	Children: {IdentifierNode}
}
export type ExpressionListNode = SyntaxNode.SyntaxNode & {
	Children: {ExpressionNode}
}
export type ExpressionNode = BinaryNode | IdentifierNode | IndexIdentifierNode | ExpressionListNode

export type Identifier = string
export type IndexIdentifier = {Identifier}

export type FunctionDefinitionData = {
	type: "functionDefinition",
	requiredArguments: {any},
	scopeId: number
}
export type FunctionDeclarationData = {
	type: "functionDeclaration",
	identifier: Identifier | IndexIdentifier,
	definition: FunctionDefinitionData,
	arguments: {any},
}

-- Utility class
local EqualTable = {
	__eq = function(Table1, Table2)
		for Index, Value in Table1 do
			if Table1[Index] ~= Table2[Index] then
				return
			end
		end

		for Index, Value in Table2 do
			if Table2[Index] ~= Table1[Index] then
				return
			end
		end

		return true
	end
}

-- Helper classes
type Variable = {
	name: string | {string},
	indexing: {string}?,
	shadowed: number?,
	isSelf: boolean,
	scope: number,
	value: any,
}

type Scope = {
	data: {any}
}

-- Main class
local Visitor = {}
Visitor.__index = Visitor

function Visitor.createFromTree(Tree: SyntaxNode.SyntaxNode): Visitor
	local self = setmetatable({
		tree = Tree,
		scopes = {} :: {Scope},
		variables = {} :: {Variable},
		references = {} :: {VariableReference},
		capturedReferences = {} :: {{number}},
		scopesStack = {} :: {number},
		startingScope = 1
	}, Visitor)
	
	-- Globals
	self:newScope()
	self:newVariable("game", "game", false)
	self:newVariable("Instance", "Instance", false)
	self:newVariable("workspace", "workspace", false)
	
	return self
end

-- Getters
function Visitor:currentScopeId(): number
	return self.scopesStack[#self.scopesStack]
end

function Visitor:currentScope(): Scope
	return self.scopes[self:currentScopeId()]
end

-- Variables handler
function Visitor:variableInScope(scope: number, name: string | string): number?
	local names: {string} = type(name) == "table" and name or {name}
	local indexing: {string} = #names > 1 and setmetatable(table.move(names, 2, #names, 1, {}), EqualTable) or nil
	
	for i = #self.variables, 1, -1 do
		local variable = self.variables[i]
		if variable.name == names[1] and variable.indexing == indexing then
			return i
		end
	end
	
	return nil
end

function Visitor:getVariable(name: string | {string}): {variable: number, scope: number}?
	for stackIndex = #self.scopesStack, 1, -1 do
		local scope = self.scopesStack[stackIndex]
		local variable = self:variableInScope(scope, name)
		if variable then
			return {
				variable = variable,
				scope = scope
			}
		end
	end
	
	return nil
end

function Visitor:newVariable(name: string | {string}, value: any, isSelf: boolean?): number
	if DEBUG then
		print('Visitor: new variable', name, 'with value', value)
	end
	
	local names: {string} = type(name) == "table" and name or {name}
	local indexing: {string} = #names > 1 and table.move(names, 2, #names, 1, {}) or nil
	
	local id = #self.variables + 1
	self.variables[id] = {
		name = names[1],
		indexing = indexing,
		isSelf = isSelf == true,
		scope = self:currentScopeId(),
		value = value,
	}
	
	return id
end

-- Names-related
function Visitor:resolveIndexes(indexes: {string}): {string}
	if #indexes < 1 then
		return indexes
	end
	
	local indexesWithMeta = setmetatable(table.clone(indexes), EqualTable)
	local removedIndexes = table.create(#indexes)
	
	for i = 1, #indexesWithMeta do
		local foundVariable = self:getVariable(indexesWithMeta)
		if foundVariable then
			local varValue = self.variables[foundVariable.variable].value
			if type(varValue) == "table" then
				for i = #varValue, 1, -1 do
					table.insert(removedIndexes, 1, varValue[i])
				end
			else
				table.insert(removedIndexes, 1, varValue)
			end
			
			return removedIndexes
		else
			table.insert(removedIndexes, 1, table.remove(indexesWithMeta, #indexesWithMeta))
		end
	end
	
	return indexes
end

-- Scopes handler
function Visitor:newScope()
	local scopeId = #self.scopes + 1
	self.scopes[scopeId] = {
		data = {}
	}
	
	if DEBUG then
		print("Visitor: new scope id", scopeId)
	end
	
	table.insert(self.scopesStack, scopeId)
end

function Visitor:exitScope()
	if DEBUG then
		print("Visitor: exiting scope id", self.scopesStack[#self.scopesStack])
	end
	table.remove(self.scopesStack, #self.scopesStack)
end

-- Visitors

-- User-defined functions
function Visitor:visitFunctionCall(identifier: Identifier | IndexIdentifier, arguments: {Identifier | IndexIdentifier | FunctionDefinitionData}, isSelfCall: boolean)
end
function Visitor:visitNumericalLoop(expressions: {any}?, scopeId: number)
end
function Visitor:visitWhileLoop(conditions: {any}?, scopeId: number)
end
function Visitor:visitIf(conditions: {any}?, thenScopeId: number, elseScopeId: number?)
end
function Visitor:onFunctionReassignment(name: Identifier | IndexIdentifier, value: FunctionDeclarationData)
end
function Visitor:onNewScope(scopeId: number)
end

-- Core functions
function Visitor:visitScope(scope: SyntaxNode.SyntaxNode): number
	self:newScope()
	
	local scopeId = self:currentScopeId()
	self:onNewScope(scopeId)
	
	if scope.Children ~= nil then
		for _, node in scope.Children do
			local data = self:node(node)
			
			if data then
				table.insert(self.scopes[scopeId].data, data)
			end
		end
	end
	
	self:exitScope()
	
	return scopeId
end

function Visitor:nodeIs(node: SyntaxNode.SyntaxNode, kind: SyntaxNode.SyntaxNodeKind | {SyntaxNode.SyntaxNodeKind})
	if type(kind) == "table" then
		return table.find(kind, node.Kind) ~= nil
	end
	
	return node.Kind == kind
end

function Visitor:node(node: SyntaxNode.SyntaxNode?): any
	if not node then
		return nil
	end
	
	if self:nodeIs(node, SyntaxNode.SyntaxNodeKind.Scope) then
		return self:visitScope(node)
	elseif self:nodeIs(node, SyntaxNode.SyntaxNodeKind.FunctionCall) then
        if not node.Children then
            return
        end

		local identifier = self:node(node.Children[1])
		local passedArguments = self:node(node.Children[2]) or {}
		
		return self:visitFunctionCall(identifier, passedArguments, self:nodeIs(node.Children[1], SyntaxNode.SyntaxNodeKind.SelfIndexIndentifier))
	elseif self:nodeIs(node, SyntaxNode.SyntaxNodeKind.NumericalLoop) then
        if not node.Children then
            return
        end

		local expressions = self:node(node.Children[1])
		local scopeId = self:node(node.Children[2])
		
		return self:visitNumericalLoop(expressions, scopeId)
	elseif self:nodeIs(node, SyntaxNode.SyntaxNodeKind.WhileLoop) then
        if not node.Children then
            return
        end

		local conditions = self:node(node.Children[1])
		local scopeId = self:node(node.Children[2])

		return self:visitWhileLoop(conditions, scopeId)
	elseif self:nodeIs(node, SyntaxNode.SyntaxNodeKind.IfStatement) then
        if not node.Children then
            return
        end

		local conditions = self:node(node.Children[1])
		local thenScopeId = self:node(node.Children[2])
		local elseScopeId = self:node(node.Children[3])
		
		return self:visitIf(conditions, thenScopeId, elseScopeId)
		
	-- Dependency handling
	elseif self:nodeIs(node, SyntaxNode.SyntaxNodeKind.FunctionDefinition) then
        if not node.Children then
            return
        end

		local requiredArguments = self:node(node.Children[1])
		local scopeId = self:node(node.Children[2])

		return {
			type = "functionDefinition",
			requiredArguments = requiredArguments,
			scopeId = scopeId
		}
	elseif self:nodeIs(node, {SyntaxNode.SyntaxNodeKind.Function, SyntaxNode.SyntaxNodeKind.LocalFunction}) then
		if not node.Children then
            return
        end
        
        local identifier = self:node(node.Children[1])
		local definition = self:node(node.Children[2])

		return {
			type = "functionDeclaration",
			identifier = identifier,
			definition = definition,
			isLocal = self:nodeIs(node, SyntaxNode.SyntaxNodeKind.LocalFunction)
		}
		
	-- Variables handler
	-- local a, b, c = d, e, f
	elseif self:nodeIs(node, SyntaxNode.SyntaxNodeKind.Local) then
        if not node.Children then
            return
        end

		local names = self:node(node.Children[1])
		local values = self:node(node.Children[2]) or {}

		for i = 1, #names do
			local name = names[i]
			local value = values[i] or nil
			
			self:newVariable(name, value, false)
		end
        return
	-- a (doesn't exist before) = b (the Parser split up a, b = c, d to child nodes)
	elseif self:nodeIs(node, SyntaxNode.SyntaxNodeKind.Global) then
        if not node.Children then
            return
        end

		local name = self:node(node.Children[1])
		local value = self:node(node.Children[2]) or {}

		self:newVariable(name, value, false)
        return
	-- a = c
	elseif self:nodeIs(node, {SyntaxNode.SyntaxNodeKind.LocalReassignment, SyntaxNode.SyntaxNodeKind.GlobalReassignment}) then
		if not node.Children then
            return
        end
        
        local name = self:node(node.Children[1])
		local value: FunctionDeclarationData = self:node(node.Children[2])

		if type(value) == "table" then
			if value.type == "functionDeclaration" then
				self:onFunctionReassignment(name, value)
			end
		end
		
	-- Basic types
	elseif self:nodeIs(node, {SyntaxNode.SyntaxNodeKind.Identifier, SyntaxNode.SyntaxNodeKind.True, SyntaxNode.SyntaxNodeKind.False, SyntaxNode.SyntaxNodeKind.Nil, SyntaxNode.SyntaxNodeKind.String, SyntaxNode.SyntaxNodeKind.Number}) then
		if self:nodeIs(node, SyntaxNode.SyntaxNodeKind.True) then
			return true
		elseif self:nodeIs(node, SyntaxNode.SyntaxNodeKind.False) then
			return false
		elseif self:nodeIs(node, SyntaxNode.SyntaxNodeKind.Nil) then
			return nil
		elseif self:nodeIs(node, SyntaxNode.SyntaxNodeKind.Number) then
			return tonumber(node.Value)
		end
		
		return node.Value
	elseif self:nodeIs(node, {SyntaxNode.SyntaxNodeKind.IndexIndentifier, SyntaxNode.SyntaxNodeKind.SelfIndexIndentifier}) then
		if not node.Children then
            return
        end
        
        local namePath = table.create(#node.Children)
		for _, childNode in node.Children do
			if self:nodeIs(childNode, {SyntaxNode.SyntaxNodeKind.Identifier, SyntaxNode.SyntaxNodeKind.String, SyntaxNode.SyntaxNodeKind.Number}) then
				table.insert(namePath, self:node(childNode))
			elseif self:nodeIs(childNode, SyntaxNode.SyntaxNodeKind.IndexIndentifier) then
				local names = self:node(childNode)
				if names then
					for _, name in names do
						table.insert(namePath, name)
					end
				else
					warn("Visitor: Indexing contains unresolved values.", childNode)
					return
				end
			elseif self:nodeIs(childNode, SyntaxNode.SyntaxNodeKind.FunctionCall) then
				warn("Visitor: Cannot resolve function call: ", childNode)
				return
			else
				warn('Visitor: Unhandled namecall kind', childNode)
			end
		end
		namePath = self:resolveIndexes(namePath)

		return namePath
	elseif self:nodeIs(node, SyntaxNode.SyntaxNodeKind.FunctionArguments) then
		if not node.Children then
			return
		end

		return self:node(node.Children)
	elseif self:nodeIs(node, SyntaxNode.SyntaxNodeKind.Binding) then
		return self:node(node.Children and node.Children[1])
	elseif self:nodeIs(node, {SyntaxNode.SyntaxNodeKind.BindingList, SyntaxNode.SyntaxNodeKind.ExpressionList}) then
		if not node.Children then
            return
        end
        
        local expressionList = table.create(#node.Children)
		for _, child in node.Children do
			local expression = self:node(child)
			table.insert(expressionList, expression)
		end

		return expressionList
	end

    return
end

function Visitor:visit()
	self:node(self.tree)
end

type Visitor = typeof(Visitor.createFromTree(SyntaxNode.new(-1, {})))

return Visitor
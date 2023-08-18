--[[
	Abstract-syntax-tree node implementation.
]]
local AstNode = {}
AstNode.__index = AstNode

AstNode.Kind = {
	-- Simple types
	True = "True",
	Nil = "Nil",
	Dot3 = "Dot3",
	Name = "Name",
	False = "False",
	String = "String",
	InterpolatedString = "InterpolatedString",
	Number = "Number",

	-- Unary operators
	Not = "Not",
	Len = "Len",
	Neg = "Neg",

	-- Binary Operators
	Div = "Div",
	Add = "Add",
	Or = "Or",
	And = "And",
	CompareGe = "CompareGe",
	Concat = "Concat",
	Mod = "Mod",
	CompareLe = "CompareLe",
	CompareEq = "CompareEq",
	Pow = "Pow",
	CompareLt = "CompareLt",
	CompareNe = "CompareNe",
	CompareGt = "CompareGt",
	Sub = "Sub",
	Mul = "Mul",

	-- Control-related operators
	Continue = "Continue",
	Break = "Break",
	Return = "Return",

	-- Expression related things
	IndexExpr = "IndexExpr",
	TableConstructor = "TableConstructor",
	SelfIndexName = "SelfIndexName",
	FunctionCall = "FunctionCall",
	IndexName = "IndexName",
	Binding = "Binding",

	-- Statements
	FunctionStat = "FunctionStat",
	Assign = "Assign",
	CompoundAssign = "CompoundAssign",
	IfStat = "IfStat",

	-- Control structures
	LocalFunction = "LocalFunction",
	Block = "Block",
	ForInLoop = "ForInLoop",
	ForLoop = "ForLoop",
	Global = "Global",
	WhileLoop = "WhileLoop",
	RepeatLoop = "RepeatLoop",
	Local = "Local",
	DoBlock = "DoBlock",

	-- Type-related things
	VariadicTypePack = "VariadicTypePack",
	TypeTable = "TypeTable",
	TypeReference = "TypeReference",
	TypeUnion = "TypeUnion",
	TypeFunction = "TypeFunction",
	TypeTableIndexer = "TypeTableIndexer",
	GenericTypePack = "GenericTypePack",
	SingletonString = "SingletonString",
	TypeTableProp = "TypeTableProp",
	SingletonBool = "SingletonBool",
	TypeAssertion = "TypeAssertion",
	TypeIntersection = "TypeIntersection",
	TypeTypeOf = "TypeTypeOf",
}

function AstNode.fromArray(nodeKind, children, value)
	local self = {}
	setmetatable(self, AstNode)

	self.value = value
	self.kind = nodeKind
	self.children = children

	return self
end

function AstNode.new(nodeKind, ...)
	return AstNode.fromArray(nodeKind, nil, table.pack(...))
end

function AstNode.fromValue(nodeKind, value, ...)
	return AstNode.fromArray(nodeKind, table.pack(...), value)
end

function AstNode.is(object)
	return type(object) == "table" and getmetatable(object) == AstNode
end

return AstNode
--!strict
type Array<T> = { [number]: T }

local SyntaxNode = {}
SyntaxNode.__index = SyntaxNode

export type SyntaxNode = typeof(setmetatable({} :: {
	Kind: SyntaxNodeKind,
	Children: Array<SyntaxNode>?,
	Value: any,
    ScopeName: string?,
}, SyntaxNode))
export type SyntaxNodeKind = number

SyntaxNode.SyntaxNodeKind = { -- TODO reorder this
	Return = 1,
	Break = 2,
	Continue = 3,

	Scope = 4,

	Identifier = 5,

	Global = 6,
	Local = 7,
	GlobalReassignment = 8,
	LocalReassignment = 9,

	IndexIndentifier = 10,
	IndexExpression = 11,
	SelfIndexIndentifier = 12,

	FunctionCall = 13,

	Table = 14,

	Add = 15,
	Subtract = 16,
	Multiply = 17,
	Divide = 18,
	Modulo = 19,
	Power = 20,
	Concaternate = 21,
	CompareNotEqual = 22,
	CompareEqual = 23,
	CompareLessThan = 24,
	CompareLessEqual = 25,
	CompareGreaterThan = 26,
	CompareGreaterEqual = 27,
	And = 28,
	Or = 29,

	True = 30,
	False = 31,
	Nil = 32,
	Dot3 = 33,

	String = 34,
	Number = 35,

	Length = 36,

	Not = 37,

	Negate = 38,

	TableField = 39,

	IfElseExpression = 40,

	LocalFunction = 41,
	Function = 42,

	FunctionArguments = 43,

	ExpressionList = 44,

	BindingList = 45,
	Binding = 46,

	Do = 47,

	FunctionDefinition = 48,

	IfStatement = 49,
	WhileLoop = 50,
	RepeatLoop = 51,

	CompoundAssign = 52,

	InterpolatedString = 53,

	NumericalLoop = 54,
	GenericLoop = 55
}

--function SyntaxNode.__tostring(self)
--	local KindName = "Unknown"
--	for Kind, Identifier in SyntaxNode.SyntaxNodeKind do
--		if Identifier == self.Kind then
--			KindName = Kind
--			break
--		end
--	end

--	return `SyntaxNode.SyntaxNodeKind.{KindName}`
--end

function SyntaxNode.new(kind: SyntaxNodeKind, children: Array<SyntaxNode>, value: any): SyntaxNode
	return setmetatable({
		Kind = kind,
		Children = children,
		Value = value
	}, SyntaxNode)
end

function SyntaxNode.fromValue(kind: SyntaxNodeKind, value: any): SyntaxNode
	return setmetatable({
		Kind = kind,
		Value = value
	}, SyntaxNode)
end

return SyntaxNode
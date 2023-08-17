--[[
	Lexical token implementation. Each token contains a value and a kind,
	as well as debugging information such as where the token begins and ends.
]]
local Token = {}
Token.__index = Token

Token.Kind = {
	-- Reserved
	ReservedElseIf = "ReservedElseIf",
	ReservedRepeat = "ReservedRepeat",
	ReservedEnd = "ReservedEnd",
	ReservedUntil = "ReservedUntil",
	ReservedIf = "ReservedIf",
	ReservedOr = "ReservedOr",
	ReservedBreak = "ReservedBreak",
	ReservedLocal = "ReservedLocal",
	ReservedTrue = "ReservedTrue",
	ReservedElse = "ReservedElse",
	ReservedFunction = "ReservedFunction",
	ReservedDo = "ReservedDo",
	ReservedThen = "ReservedThen",
	ReservedFalse = "ReservedFalse",
	ReservedIn = "ReservedIn",
	ReservedAnd = "ReservedAnd",
	ReservedReturn = "ReservedReturn",
	ReservedWhile = "ReservedWhile",
	ReservedNil = "ReservedNil",
	ReservedFor = "ReservedFor",
	ReservedNot = "ReservedNot",

	-- Operators
	GreaterEqual = "GreaterEqual",
	Plus = "Plus",
	SemiColon = "SemiColon",
	RightParen = "RightParen",
	RightBrace = "RightBrace",
	LessEqual = "LessEqual",
	Minus = "Minus",
	Dot = "Dot",
	LeftBrace = "LeftBrace",
	Modulo = "Modulo",
	GreaterThan = "GreaterThan",
	RightBracket = "RightBracket",
	Caret = "Caret",
	Equal = "Equal",
	EqualTo = "EqualTo",
	LeftBracket = "LeftBracket",
	NotEqual = "NotEqual",
	Star = "Star",
	LeftParen = "LeftParen",
	LessThan = "LessThan",
	Dot2 = "Dot2",
	Slash = "Slash",
	Colon = "Colon",
	Hashtag = "Hashtag",
	Dot3 = "Dot3",

	-- Delimiters
	Comma = "Comma",
	SemiColon = "SemiColon",

	-- Type-related operators
	DoubleColon = "DoubleColon",
	QuestionMark = "QuestionMark",
	Ampersand = "Ampersand",
	SkinnyArrow = "SkinnyArrow",
	Pipe = "Pipe",

	-- Compound operators
	StarEqual = "StarEqual",
	MinusEqual = "MinusEqual",
	SlashEqual = "SlashEqual",
	PlusEqual = "PlusEqual",

	-- Other things
	Number = "Number",
	QuotedString = "QuotedString",
	Comment = "Comment",
	Name = "Name",
	LongString = "LongString",
	EndOfFile = "EndOfFile",
}

function Token.new(tokenKind, startPosition, endPosition, value)
	local self = {}
	setmetatable(self, Token)

	self.startPosition = startPosition
	self.endPosition = endPosition
	self.kind = tokenKind
	self.value = value

	return self
end

function Token.is(object)
	return type(object) == "table" and getmetatable(object) == Token
end

return Token
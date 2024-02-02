local Token = {}
Token.__index = Token

export type Token = typeof(setmetatable({} :: {
	Kind: TokenSyntaxKind,
	Position: number,
	Characters: string
}, Token))
export type TokenSyntaxKind = number

Token.SyntaxKind = {
	Invalid = 0,
	
	-- Reserved
	And = 1,
	Or = 2,
	Not = 3,
	True = 4,
	False = 5,
	Nil = 6,
	Local = 7,
	Do = 8,
	Function = 9,
	In = 10,
	For = 11,
	While = 12,
	Repeat = 13,
	Until = 14,
	If = 15,
	Then = 16,
	Else = 17,
	ElseIf = 18,
	End = 19,
	Break = 20,
	Return = 21,
	Continue = 22,
	
	-- Operators
	Plus = 23,
	Minus = 24,
	Star = 25,
	Slash = 26,
	Modulo = 27,
	Hashtag = 28,
	Equal = 29,
	Caret = 30,

	PlusEqual = 31,
	MinusEqual = 32,
	StarEqual = 33,
	SlashEqual = 34,
	ModuloEqual = 35,
	NotEqual = 36,
	EqualTo = 37,
	LessThan = 38,
	LessEqual = 39,
	GreaterThan = 40,
	GreaterEqual = 41,
	CaretEqual = 42,

	Comma = 43,
	SemiColon = 44,
	Colon = 45,
	Dot = 46,
	Dot2 = 47,
	Dot2Equal = 48,
	Dot3 = 49,

	DoubleColon = 50,
	SkinnyArrow = 51,
	Pipe = 52,
	Ampersand = 53,
	QuestionMark = 54,

	LeftParen = 55,
	RightParen = 56,
	LeftBracket = 57,
	RightBracket = 58,
	LeftBrace = 59,
	RightBrace = 60,
	
	-- Misc
	String = 61,
	LongString = 62,
	InterpolatedStringStart = 63,
	InterpolatedStringMiddle = 64,
	InterpolatedStringEnd = 65,
	InterpolatedStringSimple = 66,
	Comment = 67,
	
	Number = 68,
	
	Identifier = 69,
	
	EndOfFile = 70
}

function Token.__tostring(self)
	local KindName = "Unknown"
	for Kind, Identifier in Token.SyntaxKind do
		if Identifier == self.Kind then
			KindName = Kind
			break
		end
	end

	return `Token.SyntaxKind.{KindName}`
end

function Token.new(syntaxKind: TokenSyntaxKind, position: number, characters: string): Token
	return setmetatable({
		Kind = syntaxKind,
		Position = position,
		Characters = characters
	}, Token)
end

Token.Invalid = Token.new(Token.SyntaxKind.Invalid, 0, "")

return Token
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
	NotEqual = 35,
	EqualTo = 36,
	LessThan = 37,
	LessEqual = 38,
	GreaterThan = 39,
	GreaterEqual = 40,

	Comma = 41,
	SemiColon = 42,
	Colon = 43,
	Dot = 44,
	Dot2 = 45,
	Dot3 = 46,

	DoubleColon = 47,
	SkinnyArrow = 48,
	Pipe = 49,
	Ampersand = 50,
	QuestionMark = 51,

	LeftParen = 52,
	RightParen = 53,
	LeftBracket = 54,
	RightBracket = 55,
	LeftBrace = 56,
	RightBrace = 57,
	
	-- Misc
	String = 58,
	LongString = 59,
	InterpolatedStringStart = 60,
	InterpolatedStringMiddle = 61,
	InterpolatedStringEnd = 62,
	InterpolatedStringSimple = 63,
	Comment = 64,
	
	Number = 65,
	
	Identifier = 66,
	
	EndOfFile = 67
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
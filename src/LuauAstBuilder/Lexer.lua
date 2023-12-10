--!strict
type Array<T> = { [number]: T }

local Token = require(script.Parent.Token)

local Lexer = {}
Lexer.__index = Lexer

export type Lexer = typeof(setmetatable({}, Lexer))

Lexer.WhitespaceCharacters = " \t\n\r\f"
Lexer.ValidCharacters = "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
Lexer.AlphabetCharacters = "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
Lexer.DigitCharacters = "0123456789"
Lexer.ValidNumberCharacters = "0123456789abcdefABCDEF"
Lexer.ReservedKeywords = {
	["and"] = Token.SyntaxKind.And,
	["or"] = Token.SyntaxKind.Or,
	["not"] = Token.SyntaxKind.Not,
	["true"] = Token.SyntaxKind.True,
	["false"] = Token.SyntaxKind.False,
	["nil"] = Token.SyntaxKind.Nil,
	["local"] = Token.SyntaxKind.Local,
	["do"] = Token.SyntaxKind.Do,
	["function"] = Token.SyntaxKind.Function,
	["in"] = Token.SyntaxKind.In,
	["for"] = Token.SyntaxKind.For,
	["while"] = Token.SyntaxKind.While,
	["repeat"] = Token.SyntaxKind.Repeat,
	["until"] = Token.SyntaxKind.Until,
	["if"] = Token.SyntaxKind.If,
	["then"] = Token.SyntaxKind.Then,
	["else"] = Token.SyntaxKind.Else,
	["elseif"] = Token.SyntaxKind.ElseIf,
	["end"] = Token.SyntaxKind.End,
	["break"] = Token.SyntaxKind.Break,
	["return"] = Token.SyntaxKind.Return,
	["continue"] = Token.SyntaxKind.Continue
}
Lexer.Operators = {
	[1] = {
		["+"] = Token.SyntaxKind.Plus,
		["-"] = Token.SyntaxKind.Minus,
		["*"] = Token.SyntaxKind.Star,
		["/"] = Token.SyntaxKind.Slash,
		["%"] = Token.SyntaxKind.Modulo,
		["#"] = Token.SyntaxKind.Hashtag,
		["="] = Token.SyntaxKind.Equal,
		["^"] = Token.SyntaxKind.Caret,
		
		["<"] = Token.SyntaxKind.LessThan,
		[">"] = Token.SyntaxKind.GreaterThan,
		
		[","] = Token.SyntaxKind.Comma,
		[";"] = Token.SyntaxKind.SemiColon,
		[":"] = Token.SyntaxKind.Colon,
		["."] = Token.SyntaxKind.Dot,
		
		["|"] = Token.SyntaxKind.Pipe,
		["&"] = Token.SyntaxKind.Ampersand,
		["?"] = Token.SyntaxKind.QuestionMark,

		["("] = Token.SyntaxKind.LeftParen,
		[")"] = Token.SyntaxKind.RightParen,
		["["] = Token.SyntaxKind.LeftBracket,
		["]"] = Token.SyntaxKind.RightBracket,
		["{"] = Token.SyntaxKind.LeftBrace,
		["}"] = Token.SyntaxKind.RightBrace
	},
	[2] = {
		["+="] = Token.SyntaxKind.PlusEqual,
		["-="] = Token.SyntaxKind.MinusEqual,
		["*="] = Token.SyntaxKind.StarEqual,
		["/="] = Token.SyntaxKind.SlashEqual,
		["~="] = Token.SyntaxKind.NotEqual,
		["=="] = Token.SyntaxKind.EqualTo,
		["<="] = Token.SyntaxKind.LessEqual,
		[">="] = Token.SyntaxKind.GreaterEqual,
		
		[".."] = Token.SyntaxKind.Dot2,
		
		["::"] = Token.SyntaxKind.DoubleColon,
		["->"] = Token.SyntaxKind.SkinnyArrow
	},
	[3] = {
		["..."] = Token.SyntaxKind.Dot3
	}
}

function Lexer.new(source: string): Lexer
	return setmetatable({
		m_Source = source,
		m_Position = 1,
		
		m_BraceStack = {} :: Array<boolean>
	}, Lexer)
end

function Lexer:m_NextCharacter(count: number): string
	local characters = self:m_Peek(count - 1)
	
	local position: number = self.m_Position
	position += count
	self.m_Position = position
	
	return characters
end

function Lexer:m_Match(characters: string): boolean
	local position: number = self.m_Position
	return self.m_Source:sub(position, position + #characters - 1) == characters
end

function Lexer:m_Validate(characters: string): string?
	if self:m_Match(characters) then
		local position: number = self.m_Position
		position += #characters
		self.m_Position = position
		
		return characters
	end
	
	return nil
end

function Lexer:m_Expect(characters: string): string
	local match = self:m_Validate(characters)
	if match == nil then
		error(`Expected {characters}`)
	end

	return match
end

function Lexer:m_Peek(offset: number): string
	local position: number = self.m_Position
	return self.m_Source:sub(position, position + offset)
end

function Lexer:m_PeekAt(startPosition: number, offset: number): string
	return self.m_Source:sub(startPosition, startPosition + offset)
end

function Lexer:m_NextNumber(start: number): Token.Token
	local prefix = self:m_Validate("0b") or self:m_Validate("0B") or self:m_Validate("0x") or self:m_Validate("0X") or ""
	local characters: Array<string> = {}
	local nextCharacter
	repeat
		table.insert(characters, self:m_NextCharacter(1))
		if self:m_Validate(".") ~= nil then
			table.insert(characters, ".")
		end
		
		nextCharacter = self:m_Peek(0)
	until not Lexer.ValidNumberCharacters:find(nextCharacter, 1, true)
	
	return Token.new(Token.SyntaxKind.Number, start, prefix .. table.concat(characters))
end

function Lexer:m_NextIdentifier(start: number): Token.Token
	local characters: Array<string> = {}
	while Lexer.ValidCharacters:find(self:m_Peek(0), 1, true) do
		table.insert(characters, self:m_NextCharacter(1))
	end
	
	return Token.new(Token.SyntaxKind.Identifier, start, table.concat(characters))
end

function Lexer:m_ReadLongString(): string
	self:m_Expect("[")
	local startCount = 0
	while self:m_Validate("=") ~= nil do
		startCount += 1
	end
	self:m_Expect("[")

	local suffix = `]{("="):rep(startCount)}]`

	local characters: Array<string> = {}
	while self:m_Validate(suffix) == nil do
		table.insert(characters, self:m_NextCharacter(1))
	end
	
	return table.concat(characters)
end

function Lexer:m_NextComment(start: number): Token.Token
	if self:m_Match("[") then
		return Token.new(Token.SyntaxKind.Comment, start, self:m_ReadLongString())
	end
	local characters: Array<string> = {}
	while self:m_Validate("\n") == nil do
		table.insert(characters, self:m_NextCharacter(1))
	end

	return Token.new(Token.SyntaxKind.Comment, start, table.concat(characters))
end

function Lexer:m_NextLongString(start: number): Token.Token
	return Token.new(Token.SyntaxKind.LongString, start, self:m_ReadLongString())
end

function Lexer:m_NextString(start: number): Token.Token
	local quote = self:m_Validate("'") or self:m_Validate('"')
	local characters: Array<string> = {}
	while self:m_Validate(quote) == nil do
		local character = self:m_NextCharacter(1)
		if character == "\\" then
			character = self:m_NextCharacter(1)
		end
		
		table.insert(characters, character)
	end
	
	return Token.new(Token.SyntaxKind.String, start, table.concat(characters))
end

function Lexer:m_NextInterpolatedStringSection(start: number, formatType: Token.TokenSyntaxKind, endType: Token.TokenSyntaxKind): Token.Token
	local characters: Array<string> = {}
	while self:m_Validate("`") == nil do
		local character = self:m_Peek(0)
		if character == "\r" or character == "\n" then
			error("Malformed interpolated string, did you forget to add a '`'?")
		elseif character == "\\" then
			if self:m_Peek(1) == "u{" then
				table.insert(characters, character)
				table.insert(characters, self:m_NextCharacter(1))
				table.insert(characters, self:m_NextCharacter(1))
				character = ""
			else
				character = self:m_NextCharacter(1)
			end
		elseif character == "{" then
			table.insert(self.m_BraceStack, true)
			if self:m_PeekAt(start + 1, 0) == "{" then
				error("Double braces are not permitted within interpolated strings. Did you mean '\\{'?")
			end
			
			return Token.new(formatType, start, table.concat(characters))
		end
		
		table.insert(characters, self:m_NextCharacter(1))
	end

	return Token.new(endType, start, table.concat(characters))
end

function Lexer:m_NextInterpolatedString(start: number): Token.Token
	self:m_Expect("`")
	return self:m_NextInterpolatedStringSection(start, Token.SyntaxKind.InterpolatedStringStart, Token.SyntaxKind.InterpolatedStringSimple)
end

function Lexer:m_NextToken(): Token.Token?
	local start: number = self.m_Position
	
	if self:m_Match("--") then
		return self:m_NextComment(start)
	end
	if self:m_Match("'") or self:m_Match('"') then
		return self:m_NextString(start)
	end
	if self:m_Match("[[") or self:m_Match("[=") then
		return self:m_NextLongString(start)
	end
	if self:m_Match("`") then
		return self:m_NextInterpolatedString(start)
	end
	if self:m_Match(".") and Lexer.DigitCharacters:find(self:m_PeekAt(start + 1, 0), 1, true) then
		return self:m_NextNumber(start)
	end
	
	for i = #Lexer.Operators, 1, -1 do
		for operator, syntaxKind in pairs(Lexer.Operators[i]) do
			if self:m_Validate(operator) then
				if syntaxKind == Token.SyntaxKind.LeftBrace then
					if #self.m_BraceStack == 0 then
						table.insert(self.m_BraceStack, false)
					end
				elseif syntaxKind == Token.SyntaxKind.RightBrace then
					if #self.m_BraceStack > 0 then
						if table.remove(self.m_BraceStack, #self.m_BraceStack) == true then
							return self:m_NextInterpolatedStringSection(start, Token.SyntaxKind.InterpolatedStringMiddle, Token.SyntaxKind.InterpolatedStringEnd)
						end
					end
				end
				
				return Token.new(syntaxKind, start, operator)
			end
		end
	end
	for keyword, syntaxKind in pairs(Lexer.ReservedKeywords) do
		if not Lexer.ValidCharacters:find(self:m_PeekAt(start + #keyword, 0), 1, true) and self:m_Validate(keyword) ~= nil then -- TODO optimise but this is necessary, it can't be removed
			return Token.new(syntaxKind, start, keyword)
		end
	end
	
	do
		local character = self:m_Peek(0)
		if Lexer.WhitespaceCharacters:find(character, 1, true) then
			self:m_NextCharacter(1)
			return Token.Invalid
		end
		if Lexer.DigitCharacters:find(character, 1, true) then
			return self:m_NextNumber(start)
		end
		if Lexer.AlphabetCharacters:find(character, 1, true) then
			return self:m_NextIdentifier(start)
		end
	end
	
	return nil
end

function Lexer:Scan(): Array<Token.Token>
	local tokens = {}
	while self.m_Position < #self.m_Source do
		local token = self:m_NextToken()
		if token == nil then break end
		
		if token ~= Token.Invalid then
			table.insert(tokens, token)
		end
	end
	
	table.insert(tokens, Token.new(Token.SyntaxKind.EndOfFile, self.m_Position, "\0"))
	
	return tokens
end

return Lexer
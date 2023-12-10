--!strict
type Array<T> = { [number]: T }

local Token = require(script.Parent.Token)
local SyntaxNode = require(script.Parent.SyntaxNode)

local Parser = {}
Parser.__index = Parser

Parser.ExitScopeTokens = {
	Token.SyntaxKind.EndOfFile,
	Token.SyntaxKind.End,
	Token.SyntaxKind.Until,
	Token.SyntaxKind.Else,
	Token.SyntaxKind.ElseIf
}
Parser.BuiltInTypesTokens = {
	[Token.SyntaxKind.True] = SyntaxNode.SyntaxNodeKind.True,
	[Token.SyntaxKind.False] = SyntaxNode.SyntaxNodeKind.False,
	[Token.SyntaxKind.Nil] = SyntaxNode.SyntaxNodeKind.Nil,
	[Token.SyntaxKind.Dot3] = SyntaxNode.SyntaxNodeKind.Dot3
}
Parser.CompoundOperators = {
	[Token.SyntaxKind.PlusEqual] = SyntaxNode.SyntaxNodeKind.Add,
	[Token.SyntaxKind.MinusEqual] = SyntaxNode.SyntaxNodeKind.Subtract,
	[Token.SyntaxKind.StarEqual] = SyntaxNode.SyntaxNodeKind.Multiply,
	[Token.SyntaxKind.SlashEqual] = SyntaxNode.SyntaxNodeKind.Divide,
}
Parser.UnaryOperators = {
	[Token.SyntaxKind.Hashtag] = SyntaxNode.SyntaxNodeKind.Length,
	[Token.SyntaxKind.Not] = SyntaxNode.SyntaxNodeKind.Not,
	[Token.SyntaxKind.Minus] = SyntaxNode.SyntaxNodeKind.Negate,
}
Parser.BinaryOperators = {
	[Token.SyntaxKind.Plus] = SyntaxNode.SyntaxNodeKind.Add,
	[Token.SyntaxKind.Minus] = SyntaxNode.SyntaxNodeKind.Subtract,
	[Token.SyntaxKind.Star] = SyntaxNode.SyntaxNodeKind.Multiply,
	[Token.SyntaxKind.Slash] = SyntaxNode.SyntaxNodeKind.Divide,
	[Token.SyntaxKind.Modulo] = SyntaxNode.SyntaxNodeKind.Modulo,
	[Token.SyntaxKind.Caret] = SyntaxNode.SyntaxNodeKind.Power,
	[Token.SyntaxKind.Dot2] = SyntaxNode.SyntaxNodeKind.Concaternate,
	[Token.SyntaxKind.NotEqual] = SyntaxNode.SyntaxNodeKind.CompareNotEqual,
	[Token.SyntaxKind.EqualTo] = SyntaxNode.SyntaxNodeKind.CompareEqual,
	[Token.SyntaxKind.LessThan] = SyntaxNode.SyntaxNodeKind.CompareLessThan,
	[Token.SyntaxKind.LessEqual] = SyntaxNode.SyntaxNodeKind.CompareLessEqual,
	[Token.SyntaxKind.GreaterThan] = SyntaxNode.SyntaxNodeKind.CompareGreaterThan,
	[Token.SyntaxKind.GreaterEqual] = SyntaxNode.SyntaxNodeKind.CompareGreaterEqual,
	[Token.SyntaxKind.And] = SyntaxNode.SyntaxNodeKind.And,
	[Token.SyntaxKind.Or] = SyntaxNode.SyntaxNodeKind.Or,
}

Parser.LValueSyntaxNodeKinds = {
	SyntaxNode.SyntaxNodeKind.Identifier,
	SyntaxNode.SyntaxNodeKind.IndexExpression,
	SyntaxNode.SyntaxNodeKind.IndexIndentifier
}

function Parser.new(source: string, tokens: Array<Token.Token>): Parser
	return setmetatable({
		m_Source = source,
		m_Tokens = tokens :: Array<Token.Token>,
		m_CurrentToken = nil :: Token.Token?,
		m_Index = 0,
		
		m_ScopeLevel = 0,
		m_LocalVariablesStack = {} :: Array<Array<string>>,
		m_GlobalVariablesStack = {} :: Array<string>
	}, Parser)
end

export type Parser = typeof(Parser.new("", {}))

function Parser:m_TokenFormatError(ErrorText: string): ()
	local token: Token.Token? = self.m_CurrentToken
	if not token then
		error(ErrorText)
	end
	
	local lineTexts = string.split(self.m_Source, "\n")
	
	local currentLine = -1
	local linePosition = -1
	
	local totalIndex = 0
	local lineStartIndex = 0
	for line = 1, #lineTexts do
		totalIndex += #lineTexts[line] + 1
		if token.Position <= totalIndex then
			currentLine = line
			linePosition = token.Position - lineStartIndex
			break	
		end
		lineStartIndex = totalIndex
	end
	
	local currentLineText = lineTexts[currentLine]
	error(`\n{currentLineText}\n{string.rep(" ", linePosition - 1)}^\nLine {currentLine}:{linePosition}: {ErrorText}`)
end

function Parser:m_NextToken(): ()
	local index: number = self.m_Index
	index += 1
	self.m_CurrentToken = self.m_Tokens[index]
	self.m_Index = index
end

function Parser:m_Validate(syntaxKind: Token.TokenSyntaxKind): Token.Token?
	local token: Token.Token? = self.m_CurrentToken
	if token ~= nil and token.Kind == syntaxKind then
		self:m_NextToken()
		return token
	end
	
	return nil
end

function Parser:m_Expect(syntaxKind: Token.TokenSyntaxKind, suggestionString: string?): Token.Token
	local token: Token.Token? = self.m_CurrentToken
	if token == nil or token.Kind ~= syntaxKind then
		return self:m_TokenFormatError(`Expected {Token.new(syntaxKind, -1, "")}, got {token or Token.Invalid}{suggestionString or ""}`)
	end
	
	self:m_NextToken()
	return token
end

function Parser:m_Peek(syntaxKind: Token.TokenSyntaxKind): Token.Token?
	local token: Token.Token? = self.m_CurrentToken
	if token ~= nil and token.Kind == syntaxKind then
		return token
	end
	
	return nil
end

function Parser:m_ParseLValueBindingList(): SyntaxNode.SyntaxNode
	local bindings: Array<SyntaxNode.SyntaxNode> = {}
	repeat
		local expression
		if self:m_Validate(Token.SyntaxKind.LeftParen) then
			expression = self:m_ParseExpression()
			self:m_Expect(Token.SyntaxKind.RightParen)
		elseif self:m_Peek(Token.SyntaxKind.Identifier) then
			expression = self:m_ParseIdentifier()
		end
		
		while true do
			if self:m_Validate(Token.SyntaxKind.Dot) then
				expression = SyntaxNode.new(SyntaxNode.SyntaxNodeKind.IndexIndentifier, { expression, self:m_ParseIdentifier() })

			elseif self:m_Validate(Token.SyntaxKind.LeftBracket) then
				expression = SyntaxNode.new(SyntaxNode.SyntaxNodeKind.IndexExpression, { expression, self:m_ParseExpression() })
				self:m_Expect(Token.SyntaxKind.RightBracket)
				
				break
			else
				break
			end
		end
		
		table.insert(bindings, expression)
	until self:m_Validate(Token.SyntaxKind.Comma) == nil
	
	return SyntaxNode.new(SyntaxNode.SyntaxNodeKind.ExpressionList, bindings)
end

function Parser:m_ParsePrefixExpression(): SyntaxNode.SyntaxNode
	if self:m_Validate(Token.SyntaxKind.LeftParen) then
		local expression = self:m_ParseExpression()
		if not self:m_Validate(Token.SyntaxKind.RightParen) then
			local suggestion = self:m_Peek(Token.SyntaxKind.Equal) and "; did you mean to use '{' when defining a table?"
			self:m_Expect(Token.SyntaxKind.RightParen, suggestion)
		end

		return expression
	else
		local expression = self:m_ParseIdentifier()
		while self:m_Validate(Token.SyntaxKind.Dot) ~= nil do
			expression = SyntaxNode.new(SyntaxNode.SyntaxNodeKind.IndexIndentifier, { expression, self:m_ParseIdentifier() })
		end
		if self:m_Validate(Token.SyntaxKind.Colon) ~= nil then
			expression = SyntaxNode.new(SyntaxNode.SyntaxNodeKind.SelfIndexIndentifier, { expression, self:m_ParseIdentifier() })
		end
		return expression
	end
end

function Parser:m_ParsePrimaryExpression(): SyntaxNode.SyntaxNode?
	local expression = self:m_ParsePrefixExpression() --[[self:m_ParseLValueBindingList()
	if self:m_Validate(Token.SyntaxKind.Equal) then
		return self:m_ParseAssignment(expression)
	else
		expression = expression.Children[1]
	end]]
	while true do
		if self:m_Validate(Token.SyntaxKind.Dot) then
			expression = SyntaxNode.new(SyntaxNode.SyntaxNodeKind.IndexIndentifier, { expression, self:m_ParseIdentifier() })
		elseif self:m_Validate(Token.SyntaxKind.LeftBracket) then
			expression = SyntaxNode.new(SyntaxNode.SyntaxNodeKind.IndexExpression, { expression, self:m_ParseExpression() })
			self:m_Expect(Token.SyntaxKind.RightBracket)
		elseif self:m_Validate(Token.SyntaxKind.Colon) then
			local fn = SyntaxNode.new(SyntaxNode.SyntaxNodeKind.SelfIndexIndentifier, { expression, self:m_ParseIdentifier() })
			expression = SyntaxNode.new(SyntaxNode.SyntaxNodeKind.FunctionCall, { fn, self:m_ParseFunctionArguments(expression :: SyntaxNode.SyntaxNode?) })
		elseif self:m_Peek(Token.SyntaxKind.LeftParen) or self:m_Peek(Token.SyntaxKind.LeftBrace) or self:m_Peek(Token.SyntaxKind.String) or self:m_Peek(Token.SyntaxKind.LongString) then
			expression = SyntaxNode.new(SyntaxNode.SyntaxNodeKind.FunctionCall, { expression, self:m_ParseFunctionArguments() })
		else
			break
		end
	end

	return expression
end

function Parser:m_ParseSimpleExpression(): SyntaxNode.SyntaxNode
	local syntaxNodeKind = Parser.BuiltInTypesTokens[self.m_CurrentToken.Kind]
	if syntaxNodeKind ~= nil then
		self:m_NextToken()
		return SyntaxNode.fromValue(syntaxNodeKind)
	end
	if self:m_Peek(Token.SyntaxKind.LeftBrace) then
		return self:m_ParseTableConstructor()
	end
	if self:m_Validate(Token.SyntaxKind.If) then
		return self:m_ParseIfElseExpression()
	end
	if self:m_Validate(Token.SyntaxKind.Function) then
		return self:m_ParseFunctionBody()
	end
	
	local str = self:m_Validate(Token.SyntaxKind.String) or self:m_Validate(Token.SyntaxKind.LongString) or self:m_Validate(Token.SyntaxKind.InterpolatedStringSimple)
	if str ~= nil then
		return SyntaxNode.fromValue(SyntaxNode.SyntaxNodeKind.String, str.Characters)
	end
	local interpolatedStringStart = self:m_Peek(Token.SyntaxKind.InterpolatedStringStart)
	if interpolatedStringStart ~= nil then
		return self:m_ParseInterpolatedString()
	end
	local number = self:m_Validate(Token.SyntaxKind.Number)
	if number ~= nil then
		return SyntaxNode.fromValue(SyntaxNode.SyntaxNodeKind.Number, number.Characters)
	end
		
	return self:m_ParsePrimaryExpression()
end

function Parser:m_GenericBinary(tokens: Array<Token.TokenSyntaxKind>, subParser: (self: any) -> (SyntaxNode.SyntaxNode?)): SyntaxNode.SyntaxNode?
	local left = subParser(self)
	if self.m_CurrentToken == nil then
		return left
	end
	
	while true do
		local token = nil
		for _, possibleToken in ipairs(tokens) do
			if self:m_Validate(possibleToken) then
				token = possibleToken
				break
			end
		end
		
		if token == nil then break end

		local right = subParser(self)
		left = SyntaxNode.new(Parser.BinaryOperators[token], { left :: SyntaxNode.SyntaxNode, right :: SyntaxNode.SyntaxNode })
	end
	
	return left
end

function Parser:m_GenericPrefix(tokens: Array<Token.TokenSyntaxKind>, subParser: (self: any) -> (SyntaxNode.SyntaxNode?)): SyntaxNode.SyntaxNode?
	local left: SyntaxNode.SyntaxNode? = subParser(self)
	
	local stack = {}
	while true do
		local token = nil
		for _, possibleToken in ipairs(tokens) do
			if self:m_Validate(possibleToken) then
				token = possibleToken
				break
			end
		end
		
		if token == nil then break end
		
		table.insert(stack, token)
	end
	
	for i = #stack, 1, -1 do
		local right = subParser(self)
		left = SyntaxNode.new(Parser.UnaryOperators[stack[i]], { right :: SyntaxNode.SyntaxNode })
	end
	
	return left
end

function Parser:m_ParseAssertionExpression(): SyntaxNode.SyntaxNode
	local expression = self:m_ParseSimpleExpression()

	-- TODO typechecking type cast
	
	return expression
end

function Parser:m_ParsePowerExpression(): SyntaxNode.SyntaxNode?
	local tokens = { Token.SyntaxKind.Caret }
	return self:m_GenericBinary(tokens, Parser.m_ParseAssertionExpression)
end

function Parser:m_ParseUnaryExpression(): SyntaxNode.SyntaxNode?
	local tokens = { Token.SyntaxKind.Minus, Token.SyntaxKind.Not }
	return self:m_GenericPrefix(tokens, Parser.m_ParsePowerExpression)
end

function Parser:m_ParseFactorExpression(): SyntaxNode.SyntaxNode?
	local tokens = { Token.SyntaxKind.Modulo }
	return self:m_GenericBinary(tokens, Parser.m_ParseUnaryExpression)
end

function Parser:m_ParseMultiplicationExpression(): SyntaxNode.SyntaxNode?
	local tokens = { Token.SyntaxKind.Star, Token.SyntaxKind.Slash, Token.SyntaxKind.Modulo }
	return self:m_GenericBinary(tokens, Parser.m_ParseFactorExpression)
end

function Parser:m_ParseSumExpression(): SyntaxNode.SyntaxNode?
	local tokens = { Token.SyntaxKind.Plus, Token.SyntaxKind.Minus }
	return self:m_GenericBinary(tokens, Parser.m_ParseMultiplicationExpression)
end

function Parser:m_ParseConcatExpression(): SyntaxNode.SyntaxNode?
	local tokens = { Token.SyntaxKind.Dot2 }
	return self:m_GenericBinary(tokens, Parser.m_ParseSumExpression)
end

function Parser:m_ParseCompareExpression(): SyntaxNode.SyntaxNode?
	local tokens = { Token.SyntaxKind.LessThan, Token.SyntaxKind.LessEqual, Token.SyntaxKind.GreaterThan, Token.SyntaxKind.GreaterEqual, Token.SyntaxKind.EqualTo, Token.SyntaxKind.NotEqual }
	return self:m_GenericBinary(tokens, Parser.m_ParseConcatExpression)
end

function Parser:m_ParseAndExpression(): SyntaxNode.SyntaxNode?
	local tokens = { Token.SyntaxKind.And }
	return self:m_GenericBinary(tokens, Parser.m_ParseCompareExpression)
end

function Parser:m_ParseOrExpression(): SyntaxNode.SyntaxNode?
	local tokens = { Token.SyntaxKind.Or }
	return self:m_GenericBinary(tokens, Parser.m_ParseAndExpression)
end

function Parser:m_ParseExpression(): SyntaxNode.SyntaxNode?
	return self:m_ParseOrExpression()
end

function Parser:m_ParseIfElseExpression(): SyntaxNode.SyntaxNode
	local condition = self:m_ParseExpression()
	self:m_Expect(Token.SyntaxKind.Then)
	
	local thenExpression = self:m_ParseExpression()
	local elseExpression
	if self:m_Validate(Token.SyntaxKind.ElseIf) then
		elseExpression = self:m_ParseIfElseExpression()
	else
		self:m_Expect(Token.SyntaxKind.Else)
		elseExpression = self:m_ParseExpression()
	end
	
	return SyntaxNode.new(SyntaxNode.SyntaxNodeKind.IfElseExpression, { condition, thenExpression, elseExpression })
end

function Parser:m_ParseExpressionList(): SyntaxNode.SyntaxNode
	local expressions = {self:m_ParseExpression()}
	while self:m_Validate(Token.SyntaxKind.Comma) do
		if self:m_Peek(Token.SyntaxKind.RightParen) then
			self:m_TokenFormatError("Expected expression after ',' but got ')' instead")
		end
		
		table.insert(expressions, self:m_ParseExpression())
	end
	
	local expressionList = SyntaxNode.new(SyntaxNode.SyntaxNodeKind.ExpressionList, expressions)
	
	return expressionList
end

type TableField = { Key: SyntaxNode.SyntaxNode, Value: SyntaxNode.SyntaxNode? }
function Parser:m_ParseTableConstructor(): SyntaxNode.SyntaxNode
	self:m_Expect(Token.SyntaxKind.LeftBrace)
	
	local fields: Array<SyntaxNode.SyntaxNode> = {}
	
	if self:m_Peek(Token.SyntaxKind.RightBrace) == nil then
		local currentIndexArray = 1
		repeat
			if self:m_Validate(Token.SyntaxKind.LeftBracket) then
				local key = self:m_ParseExpression()
				self:m_Expect(Token.SyntaxKind.RightBracket)
				self:m_Expect(Token.SyntaxKind.Equal)
				
				table.insert(fields, SyntaxNode.fromValue(SyntaxNode.SyntaxNodeKind.TableField, {
					Key = key, Value = self:m_ParseExpression()
				}))
				
			elseif self:m_Peek(Token.SyntaxKind.Identifier) == nil then
				table.insert(fields, SyntaxNode.fromValue(SyntaxNode.SyntaxNodeKind.TableField, {
					Key = SyntaxNode.fromValue(SyntaxNode.SyntaxNodeKind.Number, tostring(currentIndexArray)), Value = self:m_ParseExpression()
				}))
				currentIndexArray += 1
				
			else
				local identifier = self:m_ParseIdentifier()
				self:m_Expect(Token.SyntaxKind.Equal)
				
				table.insert(fields, SyntaxNode.fromValue(SyntaxNode.SyntaxNodeKind.TableField, {
					Key = identifier, Value = self:m_ParseExpression()
				}))
			end
			
		until self:m_Validate(Token.SyntaxKind.Comma) == nil and self:m_Validate(Token.SyntaxKind.SemiColon) == nil
	end
	
	self:m_Expect(Token.SyntaxKind.RightBrace)
	
	return SyntaxNode.new(SyntaxNode.SyntaxNodeKind.Table, fields)
end

function Parser:m_ParseFunctionArguments(selfExpression: SyntaxNode.SyntaxNode?): SyntaxNode.SyntaxNode
	local arguments = {}
	if self:m_Peek(Token.SyntaxKind.LeftBrace) then
		arguments = { self:m_ParseTableConstructor() }
	elseif self:m_Peek(Token.SyntaxKind.String) or self:m_Peek(Token.SyntaxKind.LongString) then
		local str = self:m_Validate(Token.SyntaxKind.String) or self:m_Validate(Token.SyntaxKind.LongString)
		arguments = { SyntaxNode.fromValue(SyntaxNode.SyntaxNodeKind.String, str.Characters) }
	else
		self:m_Expect(Token.SyntaxKind.LeftParen)
		if self:m_Validate(Token.SyntaxKind.RightParen) == nil then
			arguments = self:m_ParseExpressionList()
			self:m_Expect(Token.SyntaxKind.RightParen)
		end
	end
	
	if selfExpression ~= nil then
		table.insert(arguments, 1, selfExpression)
	end
	
	return SyntaxNode.new(SyntaxNode.SyntaxNodeKind.FunctionArguments, arguments)
end

function Parser:m_ParseIdentifier(): SyntaxNode.SyntaxNode
	return SyntaxNode.fromValue(SyntaxNode.SyntaxNodeKind.Identifier, self:m_Expect(Token.SyntaxKind.Identifier).Characters)
end

function Parser:m_ParseBinding(): SyntaxNode.SyntaxNode
	local identifier = self:m_ParseIdentifier()
	-- TODO type annotation
	
	return SyntaxNode.new(SyntaxNode.SyntaxNodeKind.Binding, { identifier })
end

function Parser:m_ParseBindingList(): SyntaxNode.SyntaxNode
	local bindings: Array<SyntaxNode.SyntaxNode> = {}
	repeat
		table.insert(bindings, self:m_ParseBinding())
	until self:m_Validate(Token.SyntaxKind.Comma) == nil
	
	return SyntaxNode.new(SyntaxNode.SyntaxNodeKind.BindingList, bindings)
end

function Parser:m_ParseInterpolatedString(): SyntaxNode.SyntaxNode
	local expressions: Array<SyntaxNode.SyntaxNode> = {}
	
	while true do
		local canContinue = self:m_Peek(Token.SyntaxKind.InterpolatedStringEnd) == nil and self:m_Peek(Token.SyntaxKind.InterpolatedStringSimple) == nil
		
		local str = self:m_Validate(Token.SyntaxKind.InterpolatedStringStart) or self:m_Validate(Token.SyntaxKind.InterpolatedStringMiddle)
			or self:m_Validate(Token.SyntaxKind.InterpolatedStringEnd) or self:m_Validate(Token.SyntaxKind.InterpolatedStringSimple)
		table.insert(expressions, SyntaxNode.fromValue(SyntaxNode.SyntaxNodeKind.String, str.Characters))
		
		if not canContinue then break end
		
		self:m_Expect(Token.SyntaxKind.LeftBrace)
		if self:m_Peek(Token.SyntaxKind.InterpolatedStringMiddle) or self:m_Peek(Token.SyntaxKind.InterpolatedStringEnd) then
			self:m_TokenFormatError("Malformed interpolated string, expected an expression in {}")
		end
		
		table.insert(expressions, self:m_ParseExpression())
	end
	
	return SyntaxNode.new(SyntaxNode.SyntaxNodeKind.InterpolatedString, expressions)
end

function Parser:m_ParseFunctionBody(): SyntaxNode.SyntaxNode
	-- TODO generic types
	self:m_Expect(Token.SyntaxKind.LeftParen)
	local bindings
	if self:m_Validate(Token.SyntaxKind.RightParen) == nil then
		bindings = self:m_ParseBindingList()
		self:m_Expect(Token.SyntaxKind.RightParen)
	else
		bindings = SyntaxNode.new(SyntaxNode.SyntaxNodeKind.BindingList, {})
	end
	
	for _, binding in ipairs(bindings.Children :: Array<SyntaxNode.SyntaxNode>) do
		self:m_AddLocalVariableToScope(binding, 1)
	end
	
	-- TODO return type annotation
	
	local body = self:m_ParseScope()
	self:m_Expect(Token.SyntaxKind.End)
	
	return SyntaxNode.new(SyntaxNode.SyntaxNodeKind.FunctionDefinition, { bindings, body })
end

function Parser:m_ParseIfStatement(): SyntaxNode.SyntaxNode
	local condition = self:m_ParseExpression()
	self:m_Expect(Token.SyntaxKind.Then)
	
	local thenScope = self:m_ParseScope()
	local elseScope = nil
	if self:m_Validate(Token.SyntaxKind.ElseIf) then
		elseScope = self:m_ParseIfStatement()
	else
		if self:m_Validate(Token.SyntaxKind.Else) then
			elseScope = self:m_ParseScope()
		end
		self:m_Expect(Token.SyntaxKind.End)
	end
	
	return SyntaxNode.new(SyntaxNode.SyntaxNodeKind.IfStatement, { condition, thenScope, elseScope })
end

function Parser:m_CheckIsLValue(left: SyntaxNode.SyntaxNode): boolean
	return table.find(Parser.LValueSyntaxNodeKinds, left.Kind) ~= nil
end

function Parser:m_CheckIfVariableExists(left: SyntaxNode.SyntaxNode): boolean
	local identifier = left.Children == nil and left.Value or (left.Children :: Array<SyntaxNode.SyntaxNode>)[1].Value
	
	if table.find(self.m_GlobalVariablesStack, identifier) ~= nil then return true end
	
	local exists = false
	for i = self.m_ScopeLevel, 1, -1 do
		if table.find(self.m_LocalVariablesStack[i], identifier) ~= nil then
			exists = true
			break
		end
	end
	
	return exists
end

function Parser:m_ParseCompoundAssignment(left: SyntaxNode.SyntaxNode, tokenKind: Token.TokenSyntaxKind, operator: SyntaxNode.SyntaxNodeKind): SyntaxNode.SyntaxNode
	if not self:m_CheckIsLValue(left) then
		self:m_TokenFormatError(`Assigned expression is not a valid lvalue: {left}`)
	end
	if not self:m_CheckIfVariableExists(left) then
		self:m_TokenFormatError("Assigned expression is not a valid lvalue: lvalue is nil")
	end
	self:m_Expect(tokenKind)
	
	return SyntaxNode.new(SyntaxNode.SyntaxNodeKind.CompoundAssign, { SyntaxNode.new(operator, { left, self:m_ParseExpression() }) })
end

function Parser:m_ParseAssignment(left: SyntaxNode.SyntaxNode): SyntaxNode.SyntaxNode
	--local expressions = self:m_ParseExpressionList().Children
	if not self:m_CheckIsLValue(left) then
		self:m_TokenFormatError(`Assigned expression is not a valid lvalue: {left}`)
	end
	
	local variables: Array<SyntaxNode.SyntaxNode> = {left}
	while self:m_Validate(Token.SyntaxKind.Comma) do
		local expression = self:m_ParsePrimaryExpression()
		if not self:m_CheckIsLValue(left) then
			self:m_TokenFormatError(`Assigned expression is not a valid lvalue: {left}`)
		end
		
		table.insert(variables, expression)
	end
	
	self:m_Expect(Token.SyntaxKind.Equal)
	
	local values = self:m_ParseExpressionList().Children
	
	local bindings = {}
	for index, binding in variables :: Array<SyntaxNode.SyntaxNode> do
		if not self:m_CheckIsLValue(binding) then
			self:m_TokenFormatError(`Assigned expression is not a valid lvalue: {binding}`)
		end

		local identifier = binding.Kind == SyntaxNode.SyntaxNodeKind.Identifier and binding.Value or (binding.Children :: Array<SyntaxNode.SyntaxNode>)[1].Value

		for i = self.m_ScopeLevel, 1, -1 do
			if table.find(self.m_LocalVariablesStack[i], identifier) ~= nil then
				-- Lobal variable reassignment
				table.insert(bindings, SyntaxNode.new(SyntaxNode.SyntaxNodeKind.LocalReassignment, { binding, values[index] }))
			else
				if table.find(self.m_GlobalVariablesStack, identifier) == nil then
					-- Global variable declaration
					table.insert(self.m_GlobalVariablesStack, identifier)
					table.insert(bindings, SyntaxNode.new(SyntaxNode.SyntaxNodeKind.Global, { binding, values[index] }))
				else
					-- Global variable reassignment
					table.insert(bindings, SyntaxNode.new(SyntaxNode.SyntaxNodeKind.GlobalReassignment, { binding, values[index] }))
				end
			end
		end
	end
	
	--[[for index, binding in ipairs(left.Children :: Array<SyntaxNode.SyntaxNode>) do
		if not self:m_CheckIsLValue(binding) then
			self:m_TokenFormatError(`Assigned expression is not a valid lvalue: {binding}`)
		end
		
		local identifier = binding.Kind == SyntaxNode.SyntaxNodeKind.Identifier and binding.Value or (binding.Children :: Array<SyntaxNode.SyntaxNode>)[1].Value
		
		for i = self.m_ScopeLevel, 1, -1 do
			if table.find(self.m_LocalVariablesStack[i], identifier) ~= nil then
				-- Lobal variable reassignment
				table.insert(bindings, SyntaxNode.new(SyntaxNode.SyntaxNodeKind.LocalReassignment, { binding, expressions[index] }))
			else
				if table.find(self.m_GlobalVariablesStack, identifier) == nil then
					-- Global variable declaration
					table.insert(self.m_GlobalVariablesStack, identifier)
					table.insert(bindings, SyntaxNode.new(SyntaxNode.SyntaxNodeKind.Global, { binding, expressions[index] }))
				else
					-- Global variable reassignment
					table.insert(bindings, SyntaxNode.new(SyntaxNode.SyntaxNodeKind.GlobalReassignment, { binding, expressions[index] }))
				end
			end
		end
	end]]

	return SyntaxNode.new(SyntaxNode.SyntaxNodeKind.BindingList, bindings)
end

function Parser:m_AddLocalVariableToScope(binding: SyntaxNode.SyntaxNode, levelOffset: number): ()
	if binding.Children == nil then return end
	
	local scopeLevel: number = self.m_ScopeLevel
	local stack = self.m_LocalVariablesStack[scopeLevel + levelOffset]
	if stack == nil then
		stack = {}
		table.insert(self.m_LocalVariablesStack, stack)
	end
	
	local identifier = binding.Kind == SyntaxNode.SyntaxNodeKind.Identifier and binding.Value or binding.Children[1].Value
	if table.find(stack, identifier) == nil then
		table.insert(stack, identifier)
	end
end

function Parser:m_ParseNextStatement(): SyntaxNode.SyntaxNode?
	self:m_Validate(Token.SyntaxKind.Comment)
	
	if self:m_Validate(Token.SyntaxKind.If) ~= nil then
		-- If statement
		return self:m_ParseIfStatement()
	end
	
	if self:m_Validate(Token.SyntaxKind.Local) ~= nil then
		if self:m_Validate(Token.SyntaxKind.Function) ~= nil then
			-- Local function
			local identifier = self:m_ParseIdentifier()
			self:m_AddLocalVariableToScope(identifier, 0)
			local body = self:m_ParseFunctionBody()
			
			return SyntaxNode.new(SyntaxNode.SyntaxNodeKind.LocalFunction, { identifier, body })
		else
			-- Local variable
			local bindings = self:m_ParseBindingList()
			for _, binding in pairs(bindings.Children) do
				self:m_AddLocalVariableToScope(binding, 0)
			end
			local expression
			if self:m_Validate(Token.SyntaxKind.Equal) ~= nil then
				expression = self:m_ParseExpressionList()
			end
			
			return SyntaxNode.new(SyntaxNode.SyntaxNodeKind.Local, { bindings, expression })
		end
	end
	if self:m_Validate(Token.SyntaxKind.Function) ~= nil then
		-- Global Function
		-- TODO add to global scope only if it's not a function from a local variable?
		local expression = self:m_ParseIdentifier()
		while self:m_Validate(Token.SyntaxKind.Dot) ~= nil do
			expression = SyntaxNode.new(SyntaxNode.SyntaxNodeKind.IndexIndentifier, { expression, self:m_ParseIdentifier() })
		end
		if self:m_Validate(Token.SyntaxKind.Colon) ~= nil then
			expression = SyntaxNode.new(SyntaxNode.SyntaxNodeKind.SelfIndexIndentifier, { expression, self:m_ParseIdentifier() })
		end
		
		return SyntaxNode.new(SyntaxNode.SyntaxNodeKind.Function, { expression, self:m_ParseFunctionBody() })
	end
	
	if self:m_Validate(Token.SyntaxKind.For) ~= nil then
		local bindings = self:m_ParseBindingList()
		if self:m_Validate(Token.SyntaxKind.Equal) then
			-- Numerical loop
			local binding = (bindings.Children :: Array<SyntaxNode.SyntaxNode>)[1]
			self:m_AddLocalVariableToScope(binding, 1)
			
			local expressions = self:m_ParseExpressionList()
			self:m_Expect(Token.SyntaxKind.Do)
			local scope = self:m_ParseScope()
			self:m_Expect(Token.SyntaxKind.End)
			
			return SyntaxNode.new(SyntaxNode.SyntaxNodeKind.NumericalLoop, { binding, expressions, scope })
		else
			-- Generic loop
			for _, binding in ipairs(bindings.Children :: Array<SyntaxNode.SyntaxNode>) do
				self:m_AddLocalVariableToScope(binding, 1)
			end
			
			self:m_Expect(Token.SyntaxKind.In)
			local iteratorExpression = self:m_ParseExpression()
			self:m_Expect(Token.SyntaxKind.Do)
			local scope = self:m_ParseScope()
			self:m_Expect(Token.SyntaxKind.End)
			
			return SyntaxNode.new(SyntaxNode.SyntaxNodeKind.GenericLoop, { bindings, iteratorExpression, scope })
		end
	end
	if self:m_Validate(Token.SyntaxKind.While) ~= nil then
		-- While loop
		local condition = self:m_ParseExpression()
		self:m_Expect(Token.SyntaxKind.Do)
		local body = self:m_ParseScope()
		self:m_Expect(Token.SyntaxKind.End)
		
		return SyntaxNode.new(SyntaxNode.SyntaxNodeKind.WhileLoop, { condition, body })
	end
	if self:m_Validate(Token.SyntaxKind.Do) ~= nil then
		local body = self:m_ParseScope()
		self:m_Expect(Token.SyntaxKind.End)
		
		return SyntaxNode.new(SyntaxNode.SyntaxNodeKind.Do, { body })
	end
	
	if self:m_Validate(Token.SyntaxKind.Repeat) ~= nil then
		-- Repeat loop
		local body = self:m_ParseScope()
		self:m_Expect(Token.SyntaxKind.Until)
		local condition = self:m_ParseExpression()
		
		return SyntaxNode.new(SyntaxNode.SyntaxNodeKind.RepeatLoop, { body, condition })
	end
	
	if self:m_Validate(Token.SyntaxKind.Return) ~= nil then
		local expressions
		if table.find(Parser.ExitScopeTokens, self.m_CurrentToken.Kind) == nil and not self:m_Peek(Token.SyntaxKind.SemiColon) then
			expressions = self:m_ParseExpressionList()
		end
		
		return SyntaxNode.new(SyntaxNode.SyntaxNodeKind.Return, { expressions })
	end
	if self:m_Validate(Token.SyntaxKind.Break) ~= nil then
		return SyntaxNode.fromValue(SyntaxNode.SyntaxNodeKind.Break)
	end
	
	local expression = self:m_ParsePrimaryExpression()
	if expression.Kind == SyntaxNode.SyntaxNodeKind.FunctionCall then -- We can return early
		return expression
	end
	
	-- The next token is , or =, it's an assignment
	if self:m_Peek(Token.SyntaxKind.Comma) or self:m_Peek(Token.SyntaxKind.Equal) then
		return self:m_ParseAssignment(expression)
	end
	
	local tokenKind = self.m_CurrentToken.Kind
	local operator = Parser.CompoundOperators[tokenKind]
	if operator ~= nil then
		return self:m_ParseCompoundAssignment(expression, tokenKind, operator)
	end
	
	return expression
end

function Parser:m_IncrementScopeLevel()
	local scopeLevel: number = self.m_ScopeLevel
	scopeLevel += 1
	self.m_ScopeLevel = scopeLevel
	
	if self.m_LocalVariablesStack[scopeLevel] == nil then
		table.insert(self.m_LocalVariablesStack, {})
	end
end

function Parser:m_DecrementScopeLevel()
	local scopeLevel: number = self.m_ScopeLevel
	table.clear(self.m_LocalVariablesStack[scopeLevel])
	
	self.m_ScopeLevel = scopeLevel - 1
end

function Parser:m_ParseScope(): SyntaxNode.SyntaxNode
	self:m_IncrementScopeLevel()
	
	local children: Array<SyntaxNode.SyntaxNode> = {}
	while table.find(Parser.ExitScopeTokens, self.m_CurrentToken.Kind) == nil do
		local node = self:m_ParseNextStatement()
		if node == nil then break end
		
		self:m_Validate(Token.SyntaxKind.SemiColon)
		table.insert(children, node)
		
		if node.Kind == SyntaxNode.SyntaxNodeKind.Break or node.Kind == SyntaxNode.SyntaxNodeKind.Return or node.Kind == SyntaxNode.SyntaxNodeKind.Continue then break end
	end
	
	self:m_DecrementScopeLevel()
	
	return SyntaxNode.new(SyntaxNode.SyntaxNodeKind.Scope, children)
end

function Parser:Parse(): SyntaxNode.SyntaxNode
	self:m_NextToken()
	local root = self:m_ParseScope()
	self:m_Validate(Token.SyntaxKind.EndOfFile)
	
	table.clear(self.m_GlobalVariablesStack)
	
	return root
end

return Parser
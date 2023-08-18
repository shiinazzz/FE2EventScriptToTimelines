-- based off of luau's Parser (https://github.com/Roblox/luau/blob/master/Ast/src/Parser.cpp)

local shared = if script then require(script.Parent.shared) else require("./shared")
local binaryToInt = shared.binaryToInt
local isInstance, class, newBuffer =
	shared.isInstance, shared.class, shared.newBuffer
local Tokenizer = if script then require(script.Parent.Tokenizer) else require("./Tokenizer")

local getIdentifier = shared.getIdentifier

local ERROR_INVALID_INTERP_DOUBLE_BRACE =
	"Double braces are not permitted within interpolated strings. Did you mean '\\{'?"

local AstNameTable = shared.AstNameTable
local Vector = shared.Vector
local TokenType = shared.TokenType
local Location = shared.Location
local AstName = shared.AstName
local Token = shared.Token
local Position = shared.Position
local isSpace = shared.isSpace

local AstLocal = shared.AstLocal
local AstArray = shared.AstArray
local AstTypeList = shared.AstTypeList
local AstArgumentName = shared.AstArgumentName
local AstGenericType = shared.AstGenericType
local AstGenericTypePack = shared.AstGenericTypePack
--[[local AstNode = shared.AstNode
local AstExpression = shared.AstExpression
local AstStat = shared.AstStat]]
local AstExpressionGroup = shared.AstExpressionGroup
local AstExpressionConstantNil = shared.AstExpressionConstantNil
local AstExpressionConstantBool = shared.AstExpressionConstantBool
local ConstantNumberParseResult = shared.ConstantNumberParseResult
local AstExpressionConstantNumber = shared.AstExpressionConstantNumber
local AstExpressionConstantString = shared.AstExpressionConstantString
local AstExpressionLocal = shared.AstExpressionLocal
local AstExpressionGlobal = shared.AstExpressionGlobal
local AstExpressionVarargs = shared.AstExpressionVarargs
local AstExpressionCall = shared.AstExpressionCall
local AstExpressionIndexName = shared.AstExpressionIndexName
local AstExpressionIndexExpression = shared.AstExpressionIndexExpression
local AstExpressionFunction = shared.AstExpressionFunction
local AstExpressionTable = shared.AstExpressionTable
local AstExpressionUnary = shared.AstExpressionUnary
local AstExpressionBinary = shared.AstExpressionBinary
local AstExpressionTypeAssertion = shared.AstExpressionTypeAssertion
local AstExpressionIfElse = shared.AstExpressionIfElse
local AstExpressionInterpolatedString = shared.AstExpressionInterpolatedString
local AstStatBlock = shared.AstStatBlock
local AstStatIf = shared.AstStatIf
local AstStatWhile = shared.AstStatWhile
local AstStatRepeat = shared.AstStatRepeat
local AstStatBreak = shared.AstStatBreak
local AstStatContinue = shared.AstStatContinue
local AstStatReturn = shared.AstStatReturn
local AstStatExpression = shared.AstStatExpression
local AstStatLocal = shared.AstStatLocal
local AstStatFor = shared.AstStatFor
local AstStatForIn = shared.AstStatForIn
local AstStatAssign = shared.AstStatAssign
local AstStatCompoundAssign = shared.AstStatCompoundAssign
local AstStatFunction = shared.AstStatFunction
local AstStatLocalFunction = shared.AstStatLocalFunction
local AstStatTypeAlias = shared.AstStatTypeAlias
local AstStatDeclareGlobal = shared.AstStatDeclareGlobal
local AstStatDeclareFunction = shared.AstStatDeclareFunction
local AstDeclaredClassProp = shared.AstDeclaredClassProp
local AstTableIndexer = shared.AstTableIndexer
local AstStatDeclareClass = shared.AstStatDeclareClass
local AstType = shared.AstType
local AstTypeOrPack = shared.AstTypeOrPack
local AstTypeReference = shared.AstTypeReference
local AstTableProperty = shared.AstTableProperty
local AstTypeTable = shared.AstTypeTable
local AstTypeFunction = shared.AstTypeFunction
local AstTypeTypeof = shared.AstTypeTypeof
local AstTypeUnion = shared.AstTypeUnion
local AstTypeIntersection = shared.AstTypeIntersection
local AstExpressionError = shared.AstExpressionError
local AstStatError = shared.AstStatError
local AstTypeError = shared.AstTypeError
local AstTypeSingletonBool = shared.AstTypeSingletonBool
local AstTypeSingletonString = shared.AstTypeSingletonString
local AstTypePackExplicit = shared.AstTypePackExplicit
local AstTypePackVariadic = shared.AstTypePackVariadic
local AstTypePackGeneric = shared.AstTypePackGeneric

local Function = class(function(self, has_var_arg, loop_depth)
	self.has_var_arg = has_var_arg or false
	self.loop_depth = loop_depth or 0
end, {
	class_name = "Function",
})

--local Local = shared.Local
local Name = shared.Name
local Binding = shared.Binding
local HotComment = shared.HotComment
local Comment = shared.Comment

local ParseResult = class(function(self, root, lines, hot_comments, parse_errors, comment_locations)
	self.root = root
	self.lines = lines or 0
	self.hot_comments = hot_comments
	self.parse_errors = parse_errors
	self.comment_locations = comment_locations
end, {
	class_name = "ParseResult",
})
local ParseError = class(function(self, location, message)
	self.location = location
	self.message = message
end, {
	class_name = "ParseError",
	getLocation = function(self)
		return self.location
	end,
	getMessage = function(self)
		return self.message
	end,

	raise = function(location, format_string, ...)
		return error(format_string:format(...), 2)
	end,
})

local MatchToken = class(function(self, token)
	self.ttype = token.token_type
	self.position = token.location.begin
end, {
	class_name = "MatchToken",
})

-- :<
-- NOTE: INDEXES START AT 0
local function createBinaryPriority(priority)
	local result = {}

	for k, v in next, priority do
		result[k - 1] = { left = v[1], right = v[2] }
	end

	return result
end

local function isExprLValue(expression)
	return isInstance(expression, AstExpressionLocal)
		or isInstance(expression, AstExpressionGlobal)
		or isInstance(expression, AstExpressionIndexExpression)
		or isInstance(expression, AstExpressionIndexName)
end

local function shouldParseTypePack(tokenizer)
	if tokenizer:getCurrent().token_type == TokenType.Dot3 then
		return true
	elseif
		tokenizer:getCurrent().token_type == TokenType.Name and tokenizer:lookAhead().token_type == TokenType.Dot3
	then
		return true
	end

	return false
end

local function parseInteger(data)
	local result = tonumber(data)
	if result then
		return ConstantNumberParseResult.Ok, result
	end
	return ConstantNumberParseResult.Maliformed, data
end

local function parseDouble(data)
	local buffer = newBuffer(data)

	if buffer[1] == "0" and (buffer[2] == "b" or buffer[2] == "B") and buffer[3] then
		return binaryToInt(data:sub(3))
	end

	-- if buffer[1] == '0' and (buffer[2] == 'x' or buffer[2] == 'X') and buffer[3] then
	--     return parseInteger(data);
	-- end;

	return parseInteger(data)
end

local function isStatLast(stat)
	return stat:is(AstStatBreak) or stat:is(AstStatContinue) or stat:is(AstStatReturn)
end

local Parser = class(function(self, content, names, options)
	-- ignore the ordering for these properties
	self.parse_errors = Vector.new()
	self.function_stack = Vector.new()
	self.match_recovery_stop_on_token = Vector.new()
	self.hot_comment_header = true
	self.recursion_counter = 0
	self.end_mismatch_suspect = MatchToken.new(Token.new(Location.new(), TokenType.Eof))
	self.comment_locations = Vector.new()
	self.hot_comments = Vector.new()

	self.local_stack = Vector.new()
	self.local_map = {
		findKey = function(self, query)
			for k, v in next, self do
				if k == query then
					return v
				end
			end
            return
		end,
	}

	self.scratch_stat = Vector.new()
	self.scratch_string = Vector.new()
	self.scratch_expression = Vector.new()
	self.scratch_expression_aux = Vector.new()
	self.scratch_local = Vector.new()
	self.scratch_binding = Vector.new()
	self.scratch_optional_arg_name = Vector.new()
	self.scratch_declared_class_props = Vector.new()
	self.scratch_table_type_props = Vector.new()
	self.scratch_item = Vector.new()
	self.scratch_generic_types = Vector.new()
	self.scratch_generic_type_packs = Vector.new()
	self.scratch_type_or_pack = Vector.new()

	self.content = content
	self.buffer, self.length = newBuffer(content)
	names = names or AstNameTable.new()
	self.names = names
	self.options = options

	self.tokenizer = Tokenizer.new(content, names)

	local top = Function.new(true)

	self.function_stack:pushBack(top)

	self.name_self = names:addStatic("self")
	self.name_number = names:addStatic("number")
	self.name_error = names:addStatic("%error-id%")
	self.name_nil = names:getOrAdd("nil") -- nil is a reserved keyword

	self.match_recovery_stop_on_token:assign(TokenType.Reserved_END.value, 0)
	self.match_recovery_stop_on_token[TokenType.Eof.value] = 1

	assert(self.hot_comment_header, "hot_comment_header!")
	self:nextToken()

	self.hot_comment_header = false
end, {
	class_name = "Parser",
	parse = function(self)
		local _, result = xpcall(function()
			local root = self:parseChunk()
			local lines = self.tokenizer:getCurrent().location.endd.line
				+ ((self.length > 0 and self.buffer[self.length - 1] ~= "\n" and 0) or 1)

			return ParseResult.new(root, lines, self.hot_comments, self.parse_errors, self.comment_locations)
		end, function(err)
			self.parse_errors:pushBack(err)

			return ParseResult.new(nil, 0, nil, self.parse_errors)
		end)

		return result
	end,

	blockFollow = function(self, token)
		return token.token_type == TokenType.Eof
			or token.token_type == TokenType.ReservedElse
			or token.token_type == TokenType.ReservedElseif
			or token.token_type == TokenType.ReservedEnd
			or token.token_type == TokenType.ReservedUntil
	end,

	parseChunk = function(self)
		local result = self:parseBlock()

		if self.tokenizer:getCurrent().token_type ~= TokenType.Eof then
			self:expectAndConsumeFail(TokenType.Eof, nil)
		end

		return result
	end,

	parseBlock = function(self)
		local locals_begin = self:saveLocals()
		local result = self:parseBlockNoScope()
		self:restoreLocals(locals_begin)
		return result
	end,

	parseBlockNoScope = function(self)
		local body = Vector.new()

		local previous_position = self.tokenizer:getPreviousLocation().endd

		while not self:blockFollow(self.tokenizer:getCurrent()) do
			local recursion_counter_old = self.recursion_counter

			self:incrementRecursionCounter("block")

			local stat = self:parseStat()

			self.recursion_counter = recursion_counter_old

			if self.tokenizer:getCurrent().token_type == TokenType:getChar(";") then
				self:nextToken()
				stat.has_semicolon = true
			end

			body:pushBack(stat)

			if isStatLast(stat) then
				break
			end
		end

		local location = Location.new(previous_position, self.tokenizer:getCurrent().location.begin)
		return AstStatBlock.new(location, body)
	end,

	parseStat = function(self)
		local current_token_type = self.tokenizer:getCurrent().token_type
		if current_token_type == TokenType.ReservedIf then
			return self:parseIf()
		elseif current_token_type == TokenType.ReservedWhile then
			return self:parseWhile()
		elseif current_token_type == TokenType.ReservedDo then
			return self:parseDo()
		elseif current_token_type == TokenType.ReservedFor then
			return self:parseFor()
		elseif current_token_type == TokenType.ReservedRepeat then
			return self:parseRepeat()
		elseif current_token_type == TokenType.ReservedFunction then
			return self:parseFunctionStat()
		elseif current_token_type == TokenType.ReservedLocal then
			return self:parseLocal()
		elseif current_token_type == TokenType.ReservedReturn then
			return self:parseReturn()
		elseif current_token_type == TokenType.ReservedBreak then
			return self:parseBreak()
		end

		local start = self.tokenizer:getCurrent().location

		local expression = self:parsePrimaryExpression(--[[as_statement =]] true)

		if expression:is(AstExpressionCall) then
			return AstStatExpression.new(expression.location, expression)
		end

		-- if the next token is , or =, it's an assignment (, means it's an assignment with multiple variables)
		if
			self.tokenizer:getCurrent().token_type == TokenType:getChar(",")
			or self.tokenizer:getCurrent().token_type == TokenType:getChar("=")
		then
			return self:parseAssignment(expression)
		end

		-- if the next token is a compound assignment operator, it's a compound assignment (these don't support multiple variables)
		local op = self:parseCompoundOperation(self.tokenizer:getCurrent())
		if op then
			return self:parseCompoundAssignment(expression, op)
		end

		-- we know this isn't a call or an assignment; therefore it must be a context-sensitive keyword such as `type` or `continue`
		local identifier = getIdentifier(expression)

		if identifier == "type" then
			return self:parseTypeAlias(expression.location, --[[is_exported =]] false)
		end

		if
			identifier == "export"
			and self.tokenizer:getCurrent().token_type == TokenType.Name
			and AstName.new(self.tokenizer:getCurrent().value).value == "type"
		then
			self:nextToken()
			return self:parseTypeAlias(expression.location, --[[is_exported =]] true)
		end

		if identifier == "continue" then
			return self:parseContinue(expression.location)
		end

		if self.options.allow_declaration_syntax then
			if identifier == "declare" then
				return self:parseDeclaration(self.expression.location)
			end
		end

		-- skip unexpected symbol if tokenizer couldn't advance at all (statements are parsed in a loop)
		if start == self.tokenizer:getCurrent().location then
			self:nextToken()
		end

		return self:reportStatError(
			expression.location,
			expression,
			nil,
			"Incomplete statement: expected assignment or function call"
		)
	end,

	parseIf = function(self)
		local start = self.tokenizer:getCurrent().location

		self:nextToken() --if / elseif

		local condition = self:parseExpression()

		local match_then = self.tokenizer:getCurrent()
		local then_location
		if self:expectAndConsume(TokenType.ReservedThen, "if statement") then
			then_location = match_then.location
		end

		local then_body = self:parseBlock()

		local else_body
		local endd = start
		local else_location
		local has_end = false

		if self.tokenizer:getCurrent().token_type == TokenType.ReservedElseif then
			local recursion_counter_old = self.recursion_counter
			self:incrementRecursionCounter("elseif")
			else_location = self.tokenizer:getCurrent().location
			else_body = self:parseIf()
			endd = else_body.location
			has_end = else_body:as(AstStatIf).has_end
			self.recursion_counter = recursion_counter_old
		else
			local match_then_else = match_then

			if self.tokenizer:getCurrent().token_type == TokenType.ReservedElse then
				else_location = self.tokenizer:getCurrent().location
				match_then_else = self.tokenizer:getCurrent()
				self:nextToken()

				else_body = self:parseBlock()
				else_body.location.begin = match_then_else.location.endd
			end

			endd = self.tokenizer:getCurrent().location

			has_end = self:expectMatchEndAndConsume(TokenType.ReservedEnd, MatchToken.new(match_then_else))
		end

		return AstStatIf.new(
			Location.new(start, endd),
			condition,
			then_body,
			else_body,
			then_location,
			else_location,
			has_end
		)
	end,

	parseWhile = function(self)
		local start = self.tokenizer:getCurrent().location

		self:nextToken() -- while

		local condition = self:parseExpression()

		local match_do = self.tokenizer:getCurrent()
		local has_do = self:expectAndConsume(TokenType.ReservedDo, "while loop")

		self.function_stack:getBack().loop_depth = self.function_stack:getBack().loop_depth + 1

		local body = self:parseBlock()

		self.function_stack:getBack().loop_depth = self.function_stack:getBack().loop_depth - 1

		local endd = self.tokenizer:getCurrent().location

		local has_end = self:expectMatchEndAndConsume(TokenType.ReservedEnd, MatchToken.new(match_do))

		return AstStatWhile.new(Location.new(start, endd), condition, body, has_do, match_do.location, has_end)
	end,

	parseRepeat = function(self)
		local start = self.tokenizer:getCurrent().location

		local match_repeat = self.tokenizer:getCurrent()
		self:nextToken() -- repeat

		local locals_begin = self:saveLocals()

		self.function_stack:getBack().loop_depth = self.function_stack:getBack().loop_depth + 1

		local body = self:parseBlockNoScope()

		self.function_stack:getBack().loop_depth = self.function_stack:getBack().loop_depth - 1

		local has_until = self:expectMatchEndAndConsume(TokenType.ReservedUntil, MatchToken.new(match_repeat))

		local condition = self:parseExpression()

		self:restoreLocals(locals_begin)

		return AstStatRepeat.new(Location.new(start, condition.location), condition, body, has_until)
	end,

	parseDo = function(self)
		local start = self.tokenizer:getCurrent().location

		local match_do = self.tokenizer:getCurrent()
		self:nextToken() -- do

		local body = self:parseBlock()

		body.location.begin = start.begin

		self:expectMatchEndAndConsume(TokenType.ReservedEnd, MatchToken.new(match_do))

		return body
	end,

	parseBreak = function(self)
		local start = self.tokenizer:getCurrent().location

		self:nextToken() -- break

		if self.function_stack:getBack().loop_depth == 0 then
			return self:reportStatError(start, nil, AstStatBreak.new(start), "break statement must be inside a loop")
		end

		return AstStatBreak.new(start)
	end,

	parseContinue = function(self, start)
		if self.function_stack:getBack().loop_depth == 0 then
			return self:reportStatError(
				start,
				nil,
				AstStatContinue.new(start),
				"continue statement must be inside a loop"
			)
		end

		-- note: the token is already parsed for us!

		return AstStatContinue.new(start)
	end,

	parseFor = function(self)
		local start = self.tokenizer:getCurrent().location

		self:nextToken() -- for

		local var_name = self:parseBinding()

		if self.tokenizer:getCurrent().token_type == TokenType:getChar("=") then
			self:nextToken()

			local from = self:parseExpression()

			self:expectAndConsume(TokenType:getChar(","), "index range")

			local to = self:parseExpression()

			local step

			if self.tokenizer:getCurrent().token_type == TokenType:getChar(",") then
				self:nextToken()

				step = self:parseExpression()
			end

			local match_do = self.tokenizer:getCurrent()
			local has_do = self:expectAndConsume(TokenType.ReservedDo, "for loop")

			local locals_begin = self:saveLocals()

			self.function_stack:getBack().loop_depth = self.function_stack:getBack().loop_depth + 1

			local var = self:pushLocal(var_name)

			local body = self:parseBlock()

			self.function_stack:getBack().loop_depth = self.function_stack:getBack().loop_depth - 1

			self:restoreLocals(locals_begin)

			local endd = self.tokenizer:getCurrent().location

			local has_end = self:expectMatchEndAndConsume(TokenType.ReservedEnd, MatchToken.new(match_do))

			return AstStatFor.new(
				Location.new(start, endd),
				var,
				from,
				to,
				step,
				body,
				has_do,
				match_do.location,
				has_end
			)
		else
			local names = Vector.new()
			names:pushBack(var_name)

			if self.tokenizer:getCurrent().token_type == TokenType:getChar(",") then
				self:nextToken()

				self:parseBindingList(names)
			end

			local in_location = self.tokenizer:getCurrent().location
			local has_in = self:expectAndConsume(TokenType.ReservedIn, "for loop")

			local values = Vector.new()
			self:parseExpressionList(values)

			local match_do = self.tokenizer:getCurrent()
			local has_do = self:expectAndConsume(TokenType.ReservedDo, "for loop")

			local locals_begin = self:saveLocals()

			self.function_stack:getBack().loop_depth = self.function_stack:getBack().loop_depth + 1

			local vars = Vector.new()

			for i = 1, names.size do
				vars:pushBack(self:pushLocal(names[i]))
			end

			local body = self:parseBlock()

			self.function_stack:getBack().loop_depth = self.function_stack:getBack().loop_depth - 1

			self:restoreLocals(locals_begin)

			local endd = self.tokenizer:getCurrent().location

			local has_end = self:expectMatchEndAndConsume(TokenType.ReservedEnd, MatchToken.new(match_do))

			return AstStatForIn.new(
				Location.new(start, endd),
				vars,
				values,
				body,
				has_in,
				in_location,
				has_do,
				match_do.location,
				has_end
			)
		end
	end,

	parseFunctionName = function(self, start)
		local has_self, debug_name
		if self.tokenizer:getCurrent().token_type == TokenType.Name then
			debug_name = AstName.new(self.tokenizer:getCurrent().value)
		end

		-- parse funcname into a chain of indexing operators
		local expression = self:parseNameExpression("function name")

		local rescurion_counter_old = self.recursion_counter

		while self.tokenizer:getCurrent().token_type == TokenType:getChar(".") do
			local op_position = self.tokenizer:getCurrent().location.begin
			self:nextToken()

			local name = self:parseName("field name")

			-- while we could concatenate the name chain, for now let's just write the short name
			debug_name = name.name

			expression = AstExpressionIndexName.new(
				Location.new(start, name.location),
				expression,
				name.name,
				name.location,
				op_position,
				"."
			)

			-- note: while the parser isn't recursive here, we're generating recursive structures of unbounded depth
			self:incrementRecursionCounter("function name")
		end

		self.recursion_counter = rescurion_counter_old

		-- finish with :

		if self.tokenizer:getCurrent().token_type == TokenType:getChar(":") then
			local op_position = self.tokenizer:getCurrent().location.begin
			self:nextToken()

			local name = self:parseName("method name")

			-- while we could concatenate the name chain, for now let's just write the short name
			debug_name = name.name

			expression = AstExpressionIndexName.new(
				Location.new(start, name.location),
				expression,
				name.name,
				name.location,
				op_position,
				":"
			)

			has_self = true
		end

		return expression, has_self, debug_name
	end,

	parseFunctionStat = function(self)
		local start = self.tokenizer:getCurrent().location

		local match_function = self.tokenizer:getCurrent()
		self:nextToken()

		local expression, has_self, debug_name = self:parseFunctionName(start)

		self.match_recovery_stop_on_token[TokenType.ReservedEnd.value] = self.match_recovery_stop_on_token[TokenType.ReservedEnd.value]
			+ 1

		local body = (self:parseFunctionBody(has_self, MatchToken.new(match_function), debug_name, nil))

		self.match_recovery_stop_on_token[TokenType.ReservedEnd.value] = self.match_recovery_stop_on_token[TokenType.ReservedEnd.value]
			- 1

		return AstStatFunction.new(Location.new(start, body.location), expression, body)
	end,

	parseLocal = function(self)
		local start = self.tokenizer:getCurrent().location

		self:nextToken() -- local

		if self.tokenizer:getCurrent().token_type == TokenType.ReservedFunction then
			local match_function = self.tokenizer:getCurrent()
			self:nextToken()

			-- match_function is only used for diagnostics; to make it suitable for detecting missed indentation between
			-- `local function` and `end`, we patch the token to begin at the column where `local` starts
			if match_function.location.begin.line == start.begin.line then
				match_function.location.begin.column = start.begin.column
			end

			local name = self:parseName("variable name")

			self.match_recovery_stop_on_token[TokenType.ReservedEnd.value] = self.match_recovery_stop_on_token[TokenType.ReservedEnd.value]
				+ 1

			local body, var = self:parseFunctionBody(false, MatchToken.new(match_function), name.name, name)

			self.match_recovery_stop_on_token[TokenType.ReservedEnd.value] = self.match_recovery_stop_on_token[TokenType.ReservedEnd.value]
				- 1

			local location = Location.new(start.begin, body.location.endd)

			return AstStatLocalFunction.new(location, var, body)
		else
			self.match_recovery_stop_on_token[TokenType:getChar("=").value] = self.match_recovery_stop_on_token[TokenType:getChar(
				"="
			).value] + 1

			local names = Vector.new()
			self:parseBindingList(names)

			self.match_recovery_stop_on_token[TokenType:getChar("=").value] = self.match_recovery_stop_on_token[TokenType:getChar(
				"="
			).value] - 1

			local vars = Vector.new()

			local values = Vector.new()

			local equals_sign_location

			if self.tokenizer:getCurrent().token_type == TokenType:getChar("=") then
				equals_sign_location = self.tokenizer:getCurrent().location

				self:nextToken()

				self:parseExpressionList(values)
			end

			for i = 1, names.size do
				vars:pushBack(self:pushLocal(names[i]))
			end

			local endd = (values:isEmpty() and self.tokenizer:getPreviousLocation()) or values:getBack().location

			return AstStatLocal.new(Location.new(start, endd), vars, values, equals_sign_location)
		end
	end,

	parseReturn = function(self)
		local start = self.tokenizer:getCurrent().location

		self:nextToken()

		local list = Vector.new()

		if
			not self:blockFollow(self.tokenizer:getCurrent())
			and self.tokenizer:getCurrent().token_type ~= TokenType:getChar(";")
		then
			self:parseExpressionList(list)
		end

		local endd = (list:isEmpty() and start) or list:getBack().location

		return AstStatReturn.new(Location.new(start, endd), list)
	end,

	parseTypeAlias = function(self, start, is_exported)
		-- note: `type` token is already parsed for us, so we just need to parse the rest

		local name = self:parseNameOptional("type name")

		-- Use error name if the name is missing
		if not name then
			name = Name.new(self.name_error, self.tokenizer:getCurrent().location)
		end

		local generics, generic_packs = self:parseGenericTypeList(--[[with_default_values =]] true)

		self:expectAndConsume(TokenType:getChar("="), "type alias")

		local ttype = self:parseType()

		return AstStatTypeAlias.new(
			Location.new(start, ttype.location),
			name.name,
			name.location,
			generics,
			generic_packs,
			ttype,
			is_exported
		)
	end,

	parseDeclaredClassMethod = function(self)
		self:nextToken()
		local start = self.tokenizer:getCurrent().location
		local function_name = self:parseName("function name")

		local generics = AstArray.new()
		local generic_packs = AstArray.new()

		generics.size = 0
		generics.data = nil

		generic_packs.size = 0
		generic_packs.data = nil

		local match_parenthesis = MatchToken.new(self.tokenizer:getCurrent())
		self:expectAndConsume(TokenType:getChar("("), "function parameter list start")

		local args = Vector.new()

		local var_arg = false
		local _var_arg_location = Location.new()
		local var_arg_annotation
		if self.tokenizer:getCurrent().token_type ~= TokenType:getChar(")") then
			var_arg, _var_arg_location, var_arg_annotation = self:parseBindingList(args, --[[allow_dot3 =]] true)
		end

		self:expectMatchAndConsume(TokenType:getChar(")"), match_parenthesis)

		local return_types = self:parseOptionalReturnType() or AstTypeList.new(AstType.new(nil, 0), nil)
		local endd = self.tokenizer:getCurrent().location

		local vars = Vector.new()
		local var_names = Vector.new()

		if args.size == 0 or args[1].name.name ~= "self" or args[1].annotation ~= nil then
			return AstDeclaredClassProp.new(
				function_name.name,
				self:reportTypeError(
					Location.new(start, endd),
					nil,
					"'self' must be present as the unannotated first parameter"
				),
				true
			)
		end

		-- Skip the first index.
		for i = 2, args.size do
			var_names:pushBack(AstArgumentName.new(args[i].name.name), args[i].name.location)

			if args[i].annotation then
				vars:pushBack(args[i].annotation)
			else
				vars:pushBack(
					self:reportTypeError(
						Location.new(start, endd),
						nil,
						"declaration parameters aside from 'self' must be annotated"
					)
				)
			end
		end

		if var_arg and not var_arg_annotation then
			self:report(start, "All declaration parameters aside from 'self' must be annotated")
		end

		local function_type = AstTypeFunction.new(
			Location.new(start, endd),
			generics,
			generic_packs,
			AstTypeList.new(vars, var_arg_annotation),
			var_names,
			return_types
		)

		return AstDeclaredClassProp.new(function_name.name, function_type, true)
	end,

	-- i haven't used the declare feature in LuaU, so until I do, this function should not be used
	parseDeclaration = function(self, start)
		if true then
			error("parseDeclaration should not be used until it can be verified with cpp LuaU's")
		end
		-- 'declare' token is already parsed at this point
		if self.tokenizer:getCurrent().token_type == TokenType.ReservedFunction then
			self:nextToken()
			local global_name = self:parseName("global function name")

			local generics, generic_packs = self:parseGenericTypeList(--[[with_default_values= ]] false)

			local match_parenthesis = MatchToken.new(self.tokenizer:getCurrent())
			self:expectAndConsume(TokenType:getChar("("), "global function declaration")

			local args = Vector.new()

			local var_arg = false
			local _var_arg_location = Location.new()
			local var_arg_annotation
			if self.tokenizer:getCurrent().token_type ~= TokenType:getChar(")") then
				var_arg, _var_arg_location, var_arg_annotation = self:parseBindingList(args, --[[allow_dot3 =]] true)
			end

			self:expectMatchAndConsume(TokenType:getChar(")"), match_parenthesis)

			local return_types = self:parseOptionalReturnType() or AstTypeList.new(AstType.new(nil, 0))
			local endd = self.tokenizer:getCurrent().location

			local vars = Vector.new()
			local var_names = Vector.new()

			for i = 1, args.size do
				if not args[i].annotation then
					return self:reportStatError(
						Location.new(start, endd),
						nil,
						nil,
						"All declaration parameters must be annotated"
					)
				end

				vars:pushBack(args[i].annotation)
				var_names:pushBack(AstArgumentName.new(args[i].name.name, args[i].name.location))
			end

			if var_arg and not var_arg_annotation then
				return self:reportStatError(
					Location.new(start, endd),
					nil,
					nil,
					"All declaration parameters must be annotated"
				)
			end

			return AstStatDeclareFunction.new(
				Location.new(start, endd),
				global_name.name,
				generics,
				generic_packs,
				AstTypeList.new(vars, var_arg_annotation),
				var_names,
				return_types
			)
		elseif AstName.new(self.tokenizer:getCurrent().name).value == "class" then
			self:nextToken()
			local class_start = self.tokenizer:getCurrent().location
			local class_name = self:parseName("class name")
			local super_name

			if AstName.new(self.tokenizer:getCurrent().name).name == "extends" then
				self:nextToken()
				super_name = self:parseName("superclass name").name
			end

			local props = Vector.new()
			local indexer = nil

			while self.tokenizer:getCurrent().token_type ~= TokenType.ReservedEnd do
				-- There are two possibilities: Either it's a property or a function
				if self.tokenizer:getCurrent().token_type == TokenType.ReservedFunction then
					props:pushback(self:parseDeclaredClassMethod())
				elseif self.tokenizer:getCurrent().token_type == TokenType:getChar("[") then
					if
						self.tokenizer:lookAhead().token_type == TokenType.RawString
						or self.tokenizer:lookAhead().token_type == TokenType.QuotedString
					then
						local begin = self.tokenizer:getCurrent()
						self:nextToken() -- [

						local chars = self:parseCharArray()

						self:expectMatchAndConsume(TokenType:getChar("]"), begin)
						self:expectAndConsume(TokenType:getChar(":"), "property type annotation")
						local ttype = self:parseType()

						-- TODO: since AstName conains a char*, it can't contain null (this comment is cpp related but I will still keep it)
						local contains_null = chars and (#(chars.data or chars.size) < chars.size)

						if chars and not contains_null then
							props:pushBack(AstDeclaredClassProp.new(AstName.new(chars.data), ttype, false))
						else
							self:report(begin.location, "String literal contains malformed escape sequence")
						end
					end
				else
					local prop_name = self:parseName("property name")
					self:expectAndConsume(TokenType:getChar(":"), "property type annotation")
					local prop_type = self:parseType()
					props:pushBack(AstDeclaredClassProp.new(prop_name.name, prop_type, false))
				end
			end

			local class_end = self.tokenizer:getCurrent().location
			self:nextToken() -- skip past `end`

			return AstStatDeclareClass.new(
				Location.new(class_start, class_end),
				class_name.name,
				super_name,
				props,
				indexer
			)
		else
			local global_name = self:parseNameOptional("global variable name")
			if global_name then
				self:expectAndConsume(TokenType:getChar(":"), "global variable declaration")

				local ttype = self:parseType()
				return AstStatDeclareGlobal.new(Location.new(start, ttype.location), global_name.name, ttype)
			else
				return self:reportStatError(
					start,
					nil,
					nil,
					"declare must be followed by an identifier, 'function', or 'class'"
				)
			end
		end
	end,

	parseAssignment = function(self, initial)
		if not isExprLValue(initial) then
			initial = self:reportExpressionError(
				initial.location,
				initial,
				"Assigned expression must be a variable or a field"
			)
		end

		local vars = Vector.new()
		vars:pushBack(initial)

		while self.tokenizer:getCurrent().token_type == TokenType:getChar(",") do
			self:nextToken(0)

			local expression = self:parsePrimaryExpression(--[[as_statements =]] true)

			if not isExprLValue(expression) then
				expression = self:reportExpressionError(
					expression.location,
					expression,
					"Assigned expression must be a variable or a field"
				)
			end

			vars:pushBack(expression)
		end

		self:expectAndConsume(TokenType:getChar("="), "assignment")

		local values = Vector.new()
		self:parseExpressionList(values)

		return AstStatAssign.new(Location.new(initial.location, values:getBack().location), vars, values)
	end,

	parseCompoundAssignment = function(self, initial, operation)
		if not isExprLValue(initial) then
			initial = self:reportExpressionError(
				initial.location,
				initial,
				"Assigned expression must be a variable or a field"
			)
		end

		self:nextToken()

		local value = self:parseExpression()

		return AstStatCompoundAssign.new(Location.new(initial.location, value.location), operation, initial, value)
	end,

	prepareFunctionArguments = function(self, start, has_self, args)
		local sself

		if has_self then
			sself = self:pushLocal(Binding.new(Name.new(self.name_self, start), nil))
		end

		local vars = Vector.new()

		for i = 1, args.size do
			vars:pushBack(self:pushLocal(args[i]))
		end

		return sself, vars
	end,

	parseFunctionBody = function(self, has_self, match_function, debug_name, local_name)
		local start = match_function.location

		local generics, generic_packs = self:parseGenericTypeList(--[[with_default_values =]] true)

		local match_parenthesis = MatchToken.new(self.tokenizer:getCurrent())
		self:expectAndConsume(TokenType:getChar("("), "function")

		local args = Vector.new()

		local var_arg = false
		local var_arg_location = Location.new()
		local var_arg_annotation
		if self.tokenizer:getCurrent().token_type ~= TokenType:getChar(")") then
			var_arg, var_arg_location, var_arg_annotation = self:parseBindingList(args, --[[allow_dot3 =]] true)
		end

		local arg_location

		if
			match_parenthesis.token_type == TokenType:getChar("(")
			and self.tokenizer:getCurrent().token_type == TokenType:getChar(")")
		then
			arg_location = Location.new(match_parenthesis.position, self.tokenizer:getCurrent().location.endd)
		end

		self:expectMatchAndConsume(TokenType:getChar(")"), match_parenthesis, true)

		local type_list = self:parseOptionalReturnType()

		local func_local

		if local_name then
			func_local = self:pushLocal(Binding.new(local_name, nil))
		end

		local locals_begin = self:saveLocals()

		local func = Function.new(true)

		self.function_stack:pushBack(func)

		local sself, vars = self:prepareFunctionArguments(start, has_self, args)

		local body = self:parseBlock()

		self.function_stack:popBack()

		self:restoreLocals(locals_begin)

		local endd = self.tokenizer:getCurrent().location

		local has_end = self:expectMatchEndAndConsume(TokenType.ReservedEnd, match_function)

		return AstExpressionFunction.new(
			Location.new(start, endd),
			generics,
			generic_packs,
			sself,
			vars,
			var_arg,
			var_arg_location,
			body,
			self.function_stack.size,
			debug_name,
			type_list,
			var_arg_annotation,
			has_end,
			arg_location
		),
			func_local
	end,

	parseExpressionList = function(self, result)
		result:pushBack(self:parseExpression())

		while self.tokenizer:getCurrent().token_type == TokenType:getChar(",") do
			self:nextToken()

			if self.tokenizer:getCurrent().token_type == TokenType:getChar(")") then
				self:report(self.tokenizer:getCurrent().location, "Expected expression after ',' but got ')' instead")
				break
			end

			result:pushBack(self:parseExpression())
		end
	end,

	parseBinding = function(self)
		local name = self:parseNameOptional("variable name")

		-- Use placeholder if the name is missing
		if not name then
			name = Name.new(self.name_error, self.tokenizer:getCurrent().location)
		end

		local annotation = self:parseOptionalType()

		return Binding.new(name, annotation)
	end,

	parseBindingList = function(self, result, allow_dot3)
		while true do
			if allow_dot3 and self.tokenizer:getCurrent().token_type == TokenType.Dot3 then
				local var_arg_location = self.tokenizer:getCurrent().location
				self:nextToken()

				local tail_annotation
				if self.tokenizer:getCurrent().token_type == TokenType:getChar(":") then
					self:nextToken()
					tail_annotation = self:parseVariadicArgumentTypePack()
				end

				return true, var_arg_location, tail_annotation
			end

			result:pushBack(self:parseBinding())

			if self.tokenizer:getCurrent().token_type ~= TokenType:getChar(",") then
				break
			end
			self:nextToken()
		end

		return false, Location.new(), nil
	end,

	parseOptionalType = function(self)
		if self.tokenizer:getCurrent().token_type == TokenType:getChar(":") then
			self:nextToken()
			return self:parseType()
		end
        return
	end,

	parseTypeList = function(self, result, result_names)
		while true do
			if shouldParseTypePack(self.tokenizer) then
				return self:parseTypePack()
			end

			if
				self.tokenizer:getCurrent().token_type == TokenType.Name
				and self.tokenizer:lookAhead().token_type == TokenType:getChar(":")
			then
				-- Fill in previous argument names with empty slots
				while result_names.size < result.size do
					result_names:pushBack({})
				end

				result_names:pushBack(
					AstArgumentName.new(AstExpressionIndexName.new(AstName.new(self.tokenizer:getCurrent().location)))
				)
				self:nextToken()

				self:expectAndConsume(TokenType:getChar(":"))
			elseif not result_names:isEmpty() then
				-- If we have a type with named arguments, provide elements for all types
				result_names:pushBack({})
			end

			result:pushBack(self:parseType())
			if self.tokenizer:getCurrent().token_type ~= TokenType:getChar(",") then
				break
			end

			self:nextToken()

			if self.tokenizer:getCurrent().token_type == TokenType:getChar(")") then
				self:report(self.tokenizer:getCurrent().location, "Expected type after ',' but got ')' instead")
				break
			end
		end
        return
	end,

	parseOptionalReturnType = function(self)
		if
			self.tokenizer:getCurrent().token_type == TokenType:getChar(":")
			or self.tokenizer:getCurrent().token_type == TokenType.SkinnyArrow
		then
			if self.tokenizer:getCurrent().token_type == TokenType.SkinnyArrow then
				self:report(
					self.tokenizer:getCurrent().location,
					"Function return type annotations are written after ':' instead of '->'"
				)
			end

			self:nextToken()

			local recursion_counter_old = self.recursion_counter

			local _, result = self:parseReturnType()

			-- At this point, if we find a , character, it indicates that there are multiple return types
			-- in this type annotation, but the list wasn't wrapped in parentheses.
			if self.tokenizer:getCurrent().token_type == TokenType:getChar(",") then
				self:report(
					self.tokenizer:getCurrent().location,
					"Expected a statement, got ','; did you forget to wrap the list of return types in parentheses?"
				)

				self:nextToken()
			end

			self.recursion_counter = recursion_counter_old

			return result
		end
        return
	end,

	parseReturnType = function(self)
		self:incrementRecursionCounter("type annotation")

		local begin = self.tokenizer:getCurrent()

		if self.tokenizer:getCurrent().token_type == TokenType:getChar("(") then
			if shouldParseTypePack(self.tokenizer) then
				local type_pack = self:parseTypePack()

				return type_pack.location, AstTypeList.new(nil, type_pack)
			else
				local ttype = self:parseType()

				return ttype.location, AstTypeList.new(ttype, nil)
			end
		end

		self:nextToken()

		local inner_begin = self.tokenizer:getCurrent().location

		self.match_recovery_stop_on_token[TokenType.SkinnyArrow.value] = self.match_recovery_stop_on_token[TokenType.SkinnyArrow.value]
			+ 1

		local result = Vector.new()
		local result_names = Vector.new()
		local var_arg_annotation

		-- possibly () -> ReturnType
		if self.tokenizer:getCurrent().token_type ~= TokenType:getChar(")") then
			var_arg_annotation = self:parseTypeList(result, result_names)
		end

		local location = Location.new(begin.location, self.tokenizer:getCurrent().location)

		self:expectMatchAndConsume(TokenType:getChar(")"), begin, true)

		self.match_recovery_stop_on_token[TokenType.SkinnyArrow.value] = self.match_recovery_stop_on_token[TokenType.SkinnyArrow.value]
			- 1

		if self.tokenizer:getCurrent().token_type ~= TokenType.SkinnyArrow and result_names:isEmpty() then
			-- If it turns out that it's just '(A)', it's possible that there are unions/intersections to follow, so fold over it.
			if result.size == 1 then
				local return_type = self:parseTypeSuffix(result[1], inner_begin)

				-- If parseType parses nothing, then returnType->location.end only points at the last non-type-pack
				-- type to successfully parse.  We need the span of the whole annotation.

				local end_pos = (result.size == 1 and location.endd) or return_type.location.endd

				return Location.new(location.begin, end_pos), AstTypeList.new(return_type, var_arg_annotation)
			end

			return location, AstTypeList.new(result, var_arg_annotation)
		end

		local tail = self:parseFunctionTypeTail(begin, nil, nil, result, result_names, var_arg_annotation)

		return Location.new(location, tail.location), AstTypeList.new(tail, var_arg_annotation)
	end,

	parseTableIndexer = function(self)
		local begin = self.tokenizer:getCurrent()
		self:nextToken() -- [

		local index = self:parseType()

		self:expectMatchAndConsume(TokenType:getChar("]"), begin)

		self:expectAndConsume(TokenType:getChar(":"), "table field")

		local result = self:parseType()

		return AstTableIndexer.new(index, result, Location.new(begin.location, result.location))
	end,

	parseTableType = function(self)
		self:incrementRecursionCounter("type annotation")

		local props = Vector.new()
		local indexer

		local start = self.tokenizer:getCurrent().location

		local match_brace = MatchToken.new(self.tokenizer:getCurrent())
		self:expectAndConsume(TokenType:getChar("{"), "table type")

		while self.tokenizer:getCurrent().token_type ~= TokenType:getChar("}") do
			if
				self.tokenizer:getCurrent().token_type == TokenType:getChar("[")
				and (
					self.tokenizer:lookAhead().token_type == TokenType.RawString
					or self.tokenizer:lookAhead().token_type == TokenType.QuotedString
				)
			then
				local begin = self.tokenizer:getCurrent()
				self:nextToken() -- [
				local chars = self:parseCharArray()

				self:expectMatchAndConsume(TokenType:getChar("]"), begin)
				self:expectAndConsume(TokenType:getChar(":"), "table field")

				local ttype = self:parseType()

				-- TODO: since AstName conains a char*, it can't contain null (this comment is cpp related but I will still keep it)
				local contains_null = chars and (#(chars.data or chars.size) < chars.size)

				if chars and not contains_null then
					props:pushBack(AstTableProperty.new(AstName.new(chars.data), begin.location, ttype))
				else
					self:report(begin.location, "String literal contains malformed escape sequence")
				end
			elseif self.tokenizer:getCurrent().token_type == TokenType:getChar("[") then
				if indexer then
					-- maybe we don't need to parse the entire bad_indexer...
					-- however, we either have { or [ to lint, not the entire table type or the bad indexer.
					local bad_indexer = self:parseTableIndexer()

					-- we lose all additional indexer expressions from the AST after error recovery here
					self:report(bad_indexer.location, "Cannot have more than one table indexer")
				else
					indexer = self:parseTableIndexer()
				end
			elseif
				props:isEmpty()
				and not indexer
				and not (
					self.tokenizer:getCurrent().token_type == TokenType.Name
					and self.tokenizer:lookAhead().token_type == TokenType:getChar(":")
				)
			then
				local ttype = self:parseType()

				-- array-like table type: {T} desugars into {[number]: T}
				local index = AstTypeReference.new(ttype.location, nil, self.name_number, nil, ttype.location)
					:toAstType()
				indexer = AstTableIndexer.new(index, ttype, ttype.location)

				break
			else
				local name = self:parseNameOptional("table field")

				if not name then
					break
				end

				self:expectAndConsume(TokenType:getChar(":"), "table field")

				local ttype = self:parseType()

				props:pushBack(AstTableProperty.new(name.name, name.location, ttype))
			end

			if
				self.tokenizer:getCurrent().token_type == TokenType:getChar(",")
				or self.tokenizer:getCurrent().token_type == TokenType:getChar(";")
			then
				self:nextToken()
			elseif self.tokenizer:getCurrent().token_type ~= TokenType:getChar("}") then
				break
			end
		end

		local endd = self.tokenizer:getCurrent().location
		if not self:expectMatchAndConsume(TokenType:getChar("}"), match_brace) then
			endd = self.tokenizer:getPreviousLocation()
		end

		return AstTypeTable.new(Location.new(start, endd), props, indexer)
	end,

	parseFunctionType = function(self, allow_pack)
		self:incrementRecursionCounter("type annotation")

		local force_function_type = self.tokenizer:getCurrent().token_type == TokenType:getChar("<")

		local begin = self.tokenizer:getCurrent()

		local generics, generic_packs = self:parseGenericTypeList(--[[with_default_values= ]] false)

		local parameter_start = self.tokenizer:getCurrent()

		self:expectAndConsume(TokenType:getChar("("), "function parameter")

		self.match_recovery_stop_on_token[TokenType.SkinnyArrow.value] = self.match_recovery_stop_on_token[TokenType.SkinnyArrow.value]
			+ 1

		local params = Vector.new()
		local names = Vector.new()
		local var_arg_annotation

		if self.tokenizer:getCurrent().token_type ~= TokenType:getChar(")") then
			var_arg_annotation = self:parseTypeList(params, names)
		end

		self:expectMatchAndConsume(TokenType:getChar(")"), parameter_start, true)

		self.match_recovery_stop_on_token[TokenType.SkinnyArrow.value] = self.match_recovery_stop_on_token[TokenType.SkinnyArrow.value]
			- 1

		local param_types = params

		if not names:isEmpty() then
			force_function_type = true
		end

		local return_type_introducer = self.tokenizer:getCurrent().token_type == TokenType.SkinnyArrow
			or self.tokenizer:getCurrent().token_type == TokenType:getChar(":")

		-- Not a function at all. Just a parenthesized type. Or maybe a type pack with a single element
		if params.size == 1 and not var_arg_annotation and not force_function_type and not return_type_introducer then
			if allow_pack then
				return nil, AstTypePackExplicit.new(begin.location, AstTypeList.new(param_types, nil))
			else
				return params[1], nil
			end
		end

		if not force_function_type and not return_type_introducer and allow_pack then
			return nil, AstTypePackExplicit.new(begin.location, AstTypeList.new(param_types, var_arg_annotation))
		end

		local param_names = names

		return self:parseFunctionTypeTail(begin, generics, generic_packs, param_types, param_names, var_arg_annotation)
	end,

	parseFunctionTypeTail = function(self, begin, generics, generic_packs, params, param_names, var_arg_annotation)
		self:incrementRecursionCounter("type annotation")

		if self.tokenizer:getCurrent().token_type == TokenType:getChar(":") then
			self:report(
				self.tokenizer:getCurrent().location,
				"Return types in function type annotations are written after '->' instead of ':'"
			)
			self.tokenizer:nextToken()

		-- Users occasionally write '()' as the 'unit' type when they actually want to use 'nil', here we'll try to give a more specific error
		elseif
			self.tokenizer:getCurrent().token_type ~= TokenType.SkinnyArrow
			and generics.size == 0
			and generic_packs.size == 0
			and params.size == 0
		then
			self:report(
				Location.new(begin.location, self.tokenizer:getPreviousLocation()),
				"Expected '->' after '()' when parsing function type; did you mean 'nil'?"
			)

			return AstTypeReference.new(begin.location, nil, self.name_nil, nil, begin.location)
		else
			self:expectAndConsume(TokenType.SkinnyArrow, "function type")
		end

		local end_location, return_type_list = self:parseReturnType()

		local param_types = AstTypeList.new(param_names, var_arg_annotation)
		return AstTypeFunction.new(
			Location.new(begin.location, end_location),
			generics,
			generic_packs,
			param_types,
			param_names,
			return_type_list
		)
	end,

	parseTypeSuffix = function(self, ttype, begin)
		local parts = Vector.new()
		parts:pushBack(ttype)

		self:incrementRecursionCounter("type annotation")

		local is_union = false
		local is_intersection = false

		local location = begin

		while true do
			local char = self.tokenizer:getCurrent().token_type.name
			if char == "|" then
				self:nextToken()
				parts:pushBack(self:parseSimpleType(--[[allow_pack =]] false).ttype)
				is_union = true
			elseif char == "?" then
				local loc = self.tokenizer:getCurrent().location
				self:nextToken()
				parts:pushBack(AstTypeReference.new(loc, nil, self.name_nil, nil, loc))
				is_union = true
			elseif char == "&" then
				self:nextToken()
				self:pushBack(self:parseSimpleType(--[[allow_pack =]] false).ttype)
				is_intersection = true
			elseif char == "Dot3" then
				self:report(self.tokenizer:getCurrent().location, "Unexpected '...' after type annotation")
				self:nextToken()
			else
				break
			end
		end

		if parts.size == 1 then
			return ttype
		end

		if is_union and is_intersection then
			return self:reportTypeError(
				Location.new(begin, parts:getBack().location),
				parts,
				"Mixing union and intersection types is not allowed; consider wrapping in parentheses."
			)
		end

		location.endd = parts:getBack().location.endd

		if is_union then
			return AstTypeUnion.new(location, parts)
		elseif is_intersection then
			return AstTypeIntersection.new(location, parts)
		end

		ParseError.raise(location, "Composite type was not an intersection nor union.")
        return
	end,

	parseTypeOrPack = function(self)
		local recursion_counter_old = self.recursion_counter
		self:incrementRecursionCounter("type annotation")

		local ttype, type_pack = self:parseSimpleType(--[[allow_pack =]] true)

		if type_pack then
			assert(not ttype, "parseTypeOrPack: type_pack and ttype returned from parseSimpleType")

			return nil, type_pack
		end

		self.recursion_counter = recursion_counter_old

		return ttype
	end,

	parseType = function(self)
		local recursion_counter_old = self.recursion_counter

		self:incrementRecursionCounter("type annotation")

		local begin = self.tokenizer:getCurrent().location

		local ttype = self:parseSimpleType(--[[allow_pack =]] false).ttype

		self.recursion_counter = recursion_counter_old

		return self:parseTypeSuffix(ttype, begin)
	end,

	parseSimpleType = function(self, allow_pack)
		self:incrementRecursionCounter("type annotation")

		local start = self.tokenizer:getCurrent().location

		if self.tokenizer:getCurrent().token_type == TokenType.ReservedNil then
			self:nextToken()
			return AstTypeReference.new(start, nil, self.name_nil, nil, start)
		elseif self.tokenizer:getCurrent().token_type == TokenType.ReservedTrue then
			self:nextToken()
			return AstTypeSingletonBool.new(start, true)
		elseif self.tokenizer:getCurrent().token_type == TokenType.ReservedFalse then
			self:nextToken()
			return AstTypeSingletonBool.new(start, false)
		elseif
			self.tokenizer:getCurrent().token_type == TokenType.RawString
			or self.tokenizer:getCurrent().token_type == TokenType.QuotedString
		then
			local value = self:parseCharArray()
			if value then
				return AstTypeSingletonString.new(start, value)
			else
				return self:reportTypeError(start, nil, "String literal contains maliformed escape sequence")
			end
		elseif
			self.tokenizer:getCurrent().token_type == TokenType.InterpolatedStringBegin
			or self.tokenizer:getCurrent().token_type == TokenType.InterpolatedStringSimple
		then
			self:parseInterpolatedString()

			return self:reportTypeError(start, nil, "Interpolated string literals cannot be used as type")
		elseif self.tokenizer:getCurrent().token_type == TokenType.BrokenString then
			self:nextToken()

			return self:reportTypeError(start, nil, "Maliformed string")
		elseif self.tokenizer:getCurrent().token_type == TokenType.Name then
			local prefix, prefix_location

			local name = self:parseName("type name")

			if self.tokenizer:getCurrent().token_type == TokenType:getChar(".") then
				local dot_position = self.tokenizer:getCurrent().location.begin
				self:nextToken()

				prefix = name.name
				prefix_location = name.location
				name = self:parseIndexName("field name", dot_position)
			elseif self.tokenizer:getCurrent().token_type == TokenType.Dot3 then
				self:report(
					self.tokenizer:getCurrent().location,
					"Unexpected '...' after type name; a type pack is not allowed in this context"
				)
				self:nextToken()
			elseif name.name == "typeof" then
				local typeof_begin = self.tokenizer:getCurrent()
				self:expectAndConsume(TokenType:getChar("("), "typeof type")

				local expression = self:parseExpression()

				local endd = self.tokenizer:getCurrent().location

				self:expectMatchAndConsume(TokenType:getChar(")"), typeof_begin)

				return AstTypeTypeof.new(Location.new(start, endd), expression)
			end

			local has_parameters = false
			local parameters

			if self.tokenizer:getCurrent().token_type == TokenType:getChar("<") then
				has_parameters = true
				parameters = self:parseTypeParams()
			end

			local endd = self.tokenizer:getPreviousLocation()

			return AstTypeReference.new(
				Location.new(start, endd),
				prefix,
				name.name,
				prefix_location,
				name.location,
				has_parameters,
				parameters
			)
		elseif self.tokenizer:getCurrent().token_type == TokenType:getChar("{") then
			return self:parseTableType()
		elseif
			self.tokenizer:getCurrent().token_type == TokenType:getChar("(")
			or self.tokenizer:getCurrent().token_type == TokenType:getChar("<")
		then
			return self:parseFunctionType(allow_pack)
		elseif self.tokenizer:getCurrent().token_type == TokenType.ReservedFunction then
			self:nextToken()

			return self:reportTypeError(
				start,
				nil,
				"Using 'function' as a type annotation is not supported, consider replacing with a function type annotation "
					.. "e.g. '(...any) -> ...any'"
			)
		else
			-- For a missing type annotation, capture 'space' between last token and the next one
			local ast_error_location = Location.new(self.tokenizer:getPreviousLocation().endd, start.begin)
			-- The parse error includes the next [token] to make it easier to display where the error is (e.g. in an IDE or a CLI error message).
			-- Including the current [token] also makes the parse error consistent with other parse errors returned by Luau.
			local parse_error_location = Location.new(self.tokenizer:getPreviousLocation().endd, start.endd)

			return self:reportMissingTypeError(
				parse_error_location,
				ast_error_location,
				"Expected type, got %s",
				self.tokenizer:getCurrent():toString()
			)
		end
	end,

	parseVariadicArgumentTypePack = function(self)
		-- Generic: a...
		if
			self.tokenizer:getCurrent().token_type == TokenType.Name
			and self.tokenizer:lookAhead().token_type == TokenType.Dot3
		then
			local name = self:parseName("generic name")
			local endd = self.tokenizer:getCurrent().location

			-- This will not fail because of the lookahead guard.
			self:expectAndConsume(TokenType.Dot3, "generic type pack annotation")
			return AstTypePackGeneric.new(Location.new(name.location, endd), name.name)
		-- Variadic: T
		else
			local variadic_annotation = self:parseType()
			return AstTypePackVariadic.new(variadic_annotation.location, variadic_annotation)
		end
	end,

	parseTypePack = function(self)
		-- Variadic: ...T
		if self.tokenizer:getCurrent().token_type == TokenType.Dot3 then
			local start = self.tokenizer:getCurrent().location
			self:nextToken()
			local var_arg_type = self:parseType()
			return AstTypePackVariadic.new(Location.new(start, var_arg_type.location), var_arg_type)
		-- Generic: a...
		elseif
			self.tokenizer:getCurrent().token_type == TokenType.Name
			and self.tokenizer:lookAhead().token_type == TokenType.Dot3
		then
			local name = self:parseName("generic name")
			local endd = self.tokenizer:getCurrent().location

			-- This will not fail because of the lookahead guard.
			self:expectAndConsume(TokenType.Dot3, "generic type pack annotation")
			return AstTypePackGeneric.new(Location.new(name.location, endd), name.name)
		end
        return
	end,

	parseUnaryOperation = function(self, token)
		if token.token_type == TokenType.ReservedNot then
			return AstExpressionUnary.enum.Not
		elseif token.token_type == TokenType:getChar("-") then
			return AstExpressionUnary.enum.Minus
		elseif token.token_type == TokenType:getChar("#") then
			return AstExpressionUnary.enum.Len
		end
        return
	end,

	parseBinaryOperation = function(self, token)
		if token.token_type == TokenType:getChar("+") then
			return AstExpressionBinary.enum.Add
		elseif token.token_type == TokenType:getChar("-") then
			return AstExpressionBinary.enum.Sub
		elseif token.token_type == TokenType:getChar("*") then
			return AstExpressionBinary.enum.Mul
		elseif token.token_type == TokenType:getChar("/") then
			return AstExpressionBinary.enum.Div
		elseif token.token_type == TokenType:getChar("%") then
			return AstExpressionBinary.enum.Mod
		elseif token.token_type == TokenType:getChar("^") then
			return AstExpressionBinary.enum.Pow
		elseif token.token_type == TokenType.Dot2 then
			return AstExpressionBinary.enum.Concat
		elseif token.token_type == TokenType.NotEqual then
			return AstExpressionBinary.enum.CompareNe
		elseif token.token_type == TokenType.Equal then
			return AstExpressionBinary.enum.CompareEq
		elseif token.token_type == TokenType:getChar("<") then
			return AstExpressionBinary.enum.CompareLt
		elseif token.token_type == TokenType.LessEqual then
			return AstExpressionBinary.enum.CompareLe
		elseif token.token_type == TokenType:getChar(">") then
			return AstExpressionBinary.enum.CompareGt
		elseif token.token_type == TokenType.GreaterEqual then
			return AstExpressionBinary.enum.CompareGe
		elseif token.token_type == TokenType.ReservedAnd then
			return AstExpressionBinary.enum.And
		elseif token.token_type == TokenType.ReservedOr then
			return AstExpressionBinary.enum.Or
		end
        return
	end,

	parseCompoundOperation = function(self, token)
		if token.token_type == TokenType.AddAssign then
			return AstExpressionBinary.enum.Add
		elseif token.token_type == TokenType.SubAssign then
			return AstExpressionBinary.enum.Sub
		elseif token.token_type == TokenType.MulAssign then
			return AstExpressionBinary.enum.Mul
		elseif token.token_type == TokenType.DivAssign then
			return AstExpressionBinary.enum.Div
		elseif token.token_type == TokenType.ModAssign then
			return AstExpressionBinary.enum.Mod
		elseif token.token_type == TokenType.PowAssign then
			return AstExpressionBinary.enum.Pow
		elseif token.token_type == TokenType.ConcatAssign then
			return AstExpressionBinary.enum.Concat
		end
        return
	end,

	checkUnaryConfusables = function(self)
		local current = self.tokenizer:getCurrent()

		-- early out: need to check if this is a possible confusable quickly
		if current.token_type ~= TokenType:getChar("!") then
			return
		end

		-- slow path: possible confusion
		local start = current.location

		if current.token_type == TokenType:getChar("!") then -- [lua port dev] is this neccessary???
			self:report(start, "Unexpected '!', did you mean 'not'?")
			return AstExpressionUnary.enum.Not
		end
        return
	end,

	checkBinaryConfusables = function(self, binary_priority, limit)
		local current = self.tokenizer:getCurrent()

		-- early out: need to check if this is a possible confusable quickly
		if
			current.token_type ~= TokenType:getChar("&")
			and current.token_type ~= TokenType:getChar("|")
			and current.token_type ~= TokenType:getChar("!")
		then
			return
		end

		-- slow path: possible confusable
		local start = current.location
		local after = self.tokenizer:lookAhead()

		if
			current.token_type == TokenType:getChar("&")
			and after.token_type == TokenType:getChar("&")
			and current.location.endd == after.location.begin
			and binary_priority[AstExpressionBinary.enum.Add.value].left > limit
		then
			self:nextToken()
			self:report(Location.new(start, after.location), "Unexpected '&&', did you mean 'and'?")
			return AstExpressionBinary.enum.And
		elseif
			current.token_type == TokenType:getChar("|")
			and after.token_type == TokenType:getChar("|")
			and current.location.endd == after.location.begin
			and binary_priority[AstExpressionBinary.enum.Or.value].left > limit
		then
			self:nextToken()
			self:report(Location.new(start, after.location), "Unexpected 'or' did you mean 'and'?")
			return AstExpressionBinary.enum.Or
		elseif
			current.token_type == TokenType:getChar("!")
			and after.token_type == TokenType:getChar("=")
			and current.location.endd == after.location.begin
			and binary_priority[AstExpressionBinary.enum.CompareNe.value].left > limit
		then
			self:nextToken()
			self:report(Location.new(start, after.location), "Unexpected '!=' did you mean 'and'?")
			return AstExpressionBinary.enum.CompareNe
		end
        return
	end,

	parseExpression = function(self, limit)
		if type(limit) == "nil" then
			limit = 0
		end
		local binary_priority = createBinaryPriority({
			{ 6, 6 },
			{ 6, 6 },
			{ 7, 7 },
			{ 7, 7 },
			{ 7, 7 }, -- `+' `-' `*' `/' `%'
			{ 10, 9 },
			{ 5, 4 }, -- power and concat (right associative)
			{ 3, 3 },
			{ 3, 3 }, -- equality and inequality
			{ 3, 3 },
			{ 3, 3 },
			{ 3, 3 },
			{ 3, 3 }, -- order
			{ 2, 2 },
			{ 1, 1 }, -- logical (and/or)
		})

		local recursion_counter_old = self.recursion_counter

		local unary_priority = 0

		local start = self.tokenizer:getCurrent().location

		local expression

		local unary_operation = self:parseUnaryOperation(self.tokenizer:getCurrent())

		if not unary_operation then
			unary_operation = self:checkUnaryConfusables()
		end

		if unary_operation then
			self:nextToken()

			local sub_expression = self:parseExpression(unary_priority)

			expression =
				AstExpressionUnary.new(Location.new(start, sub_expression.location), unary_operation, sub_expression)
		else
			expression = self:parseAssertionExpression()
		end

		-- expand while operators have priorities higher than `limit`
		local operation = self:parseBinaryOperation(self.tokenizer:getCurrent())

		if not operation then
			operation = self:checkBinaryConfusables(binary_priority, limit)
		end

		while operation and (binary_priority[operation.value].left > limit) do
			self:nextToken()

			-- read sub-expression with higher priority
			local after = self:parseExpression(binary_priority[operation.value].right)

			expression = AstExpressionBinary.new(Location.new(start, after.location), operation, expression, after)
			operation = self:parseBinaryOperation(self.tokenizer:getCurrent())

			if not operation then
				operation = self:checkBinaryConfusables(binary_priority, limit)
			end

			-- not: while the parse isn't recursive here, we're generating recursive symbols of unbounded depth
			self:incrementRecursionCounter("expression")
		end

		self.recursion_counter = recursion_counter_old

		return expression
	end,

	parseNameExpression = function(self, context)
		local name = self:parseNameOptional(context)

		if not name then
			return AstExpressionError.new(self.tokenizer:getCurrent().location, {}, self.parse_errors.size - 1)
		end

		local value = self.local_map:findKey(name.name)

		if value then
			return AstExpressionLocal.new(name.location, value, value.function_depth ~= self.function_stack.size - 1)
		end

		return AstExpressionGlobal.new(name.location, name.name)
	end,

	parsePrefixExpression = function(self)
		if self.tokenizer:getCurrent().token_type == TokenType:getChar("(") then
			local start = self.tokenizer:getCurrent().location.begin

			local match_parenthesis = MatchToken.new(self.tokenizer:getCurrent())
			self:nextToken()

			local expression = self:parseExpression()

			local endd = self.tokenizer:getCurrent().location.endd

			if self.tokenizer:getCurrent().token_type ~= TokenType:getChar(")") then
				local suggestion = (
					self.tokenizer:getCurrent().token_type == TokenType:getChar("=")
					and "; did you mean to use '{' when defining a table?"
				) or nil

				self:expectMatchAndConsumeFail(TokenType:getChar(")"), match_parenthesis, suggestion)

				endd = self.tokenizer:getPreviousLocation().endd
			else
				self:nextToken()
			end

			return AstExpressionGroup.new(Location.new(start, endd), expression)
		else
			return self:parseNameExpression("expression")
		end
	end,

	parsePrimaryExpression = function(self, as_statement)
		local start = self.tokenizer:getCurrent().location.begin

		local expression = self:parsePrefixExpression()

		local recursion_counter_old = self.recursion_counter

		while true do
			if self.tokenizer:getCurrent().token_type == TokenType:getChar(".") then
				local position = self.tokenizer:getCurrent().location.begin
				self:nextToken()

				local index = self:parseIndexName(nil, position)

				expression = AstExpressionIndexName.new(
					Location.new(start, index.location.endd),
					expression,
					index.name,
					index.location,
					position,
					"."
				)
			elseif self.tokenizer:getCurrent().token_type == TokenType:getChar("[") then
				local match_bracket = MatchToken.new(self.tokenizer:getCurrent())
				self:nextToken()

				local index = self:parseExpression()

				local endd = self.tokenizer:getCurrent().location.endd

				self:expectMatchAndConsume(TokenType:getChar("]"), match_bracket)

				expression = AstExpressionIndexExpression.new(Location.new(start, endd), expression, index)
			elseif self.tokenizer:getCurrent().token_type == TokenType:getChar(":") then
				local position = self.tokenizer:getCurrent().location.begin
				self:nextToken()

				local index = self:parseIndexName("method name", position)
				local func = AstExpressionIndexName.new(
					Location.new(start, index.location.endd),
					expression,
					index.name,
					index.location,
					position,
					":"
				)

				expression = self:parseFunctionArgs(func, true)
			elseif self.tokenizer:getCurrent().token_type == TokenType:getChar("(") then
				-- This error is handled inside 'parseFunctionArgs' as well, but for better error recovery we need to break out the current loop here
				if
					not as_statement
					and (expression.location.endd.line ~= self.tokenizer:getCurrent().location.begin.line)
				then
					self:reportAmbiguousCallError()
					break
				end

				expression = self:parseFunctionArgs(expression, false)
			elseif
				self.tokenizer:getCurrent().token_type == TokenType:getChar("{")
				or self.tokenizer:getCurrent().token_type == TokenType.RawString
				or self.tokenizer:getCurrent().token_type == TokenType.QuotedString
			then
				expression = self:parseFunctionArgs(expression, false)
			else
				break
			end

			-- note: while the parser isn't recursive here, we're generating recursive structures of unbounded depth
			self:incrementRecursionCounter("expression")
		end

		self.recursion_counter = recursion_counter_old

		return expression
	end,

	parseAssertionExpression = function(self)
		local start = self.tokenizer:getCurrent().location
		local expression = self:parseSimpleExpression()

		if self.tokenizer:getCurrent().token_type == TokenType.DoubleColon then
			self:nextToken()
			local annotation = self:parseType()
			return AstExpressionTypeAssertion.new(Location.new(start, annotation.location), expression, annotation)
		else
			return expression
		end
	end,

	parseSimpleExpression = function(self)
		local start = self.tokenizer:getCurrent().location

		if self.tokenizer:getCurrent().token_type == TokenType.ReservedNil then
			self:nextToken()

			return AstExpressionConstantNil.new(start)
		elseif self.tokenizer:getCurrent().token_type == TokenType.ReservedTrue then
			self:nextToken()

			return AstExpressionConstantBool.new(start, true)
		elseif self.tokenizer:getCurrent().token_type == TokenType.ReservedFalse then
			self:nextToken()

			return AstExpressionConstantBool.new(start, false)
		elseif self.tokenizer:getCurrent().token_type == TokenType.ReservedFunction then
			local match_function = self.tokenizer:getCurrent()
			self:nextToken()

			return (self:parseFunctionBody(false, MatchToken.new(match_function), AstName.new(), nil))
		elseif self.tokenizer:getCurrent().token_type == TokenType.Number then
			return self:parseNumber()
		elseif
			self.tokenizer:getCurrent().token_type == TokenType.RawString
			or self.tokenizer:getCurrent().token_type == TokenType.QuotedString
			or self.tokenizer:getCurrent().token_type == TokenType.InterpolatedStringSimple
		then
			return self:parseString()
		elseif self.tokenizer:getCurrent().token_type == TokenType.InterpolatedStringBegin then
			return self:parseInterpolatedString()
		elseif self.tokenizer:getCurrent().token_type == TokenType.BrokenString then
			self:nextToken()
			return self:reportExpressionError(start, nil, "Maliformed string")
		elseif self.tokenizer:getCurrent().token_type == TokenType.BrokenInterpolatedDoubleBrace then
			self:nextToken()
			return self:reportExpressionError(start, nil, ERROR_INVALID_INTERP_DOUBLE_BRACE)
		elseif self.tokenizer:getCurrent().token_type == TokenType.Dot3 then
			if self.function_stack:getBack().has_var_arg then
				self:nextToken()

				return AstExpressionVarargs.new(start)
			else
				self:nextToken()

				return self:reportExpressionError(start, "Cannot use '...' outside of a vararg function")
			end
		elseif self.tokenizer:getCurrent().token_type == TokenType:getChar("{") then
			return self:parseTableConstructor()
		elseif self.tokenizer:getCurrent().token_type == TokenType.ReservedIf then
			return self:parseIfElseExpression()
		else
			return self:parsePrimaryExpression(--[[as_statement =]] false)
		end
	end,

	parseFunctionArgs = function(self, func, has_self)
		if self.tokenizer:getCurrent().token_type == TokenType:getChar("(") then
			local arg_start = self.tokenizer:getCurrent().location.endd
			if func.location.endd.line ~= self.tokenizer:getCurrent().location.begin.line then
				self:reportAmbiguousCallError()
			end

			local match_parenthesis = MatchToken.new(self.tokenizer:getCurrent())
			self:nextToken()

			local args = Vector.new()

			if self.tokenizer:getCurrent().token_type ~= TokenType:getChar(")") then
				self:parseExpressionList(args)
			end

			local endd = self.tokenizer:getCurrent().location
			local arg_end = endd.endd

			self:expectMatchAndConsume(TokenType:getChar(")"), match_parenthesis)

			return AstExpressionCall.new(
				Location.new(func.location, endd),
				func,
				args,
				has_self,
				Location.new(arg_start, arg_end)
			)
		elseif self.tokenizer:getCurrent().token_type == TokenType:getChar("{") then
			local arg_start = self.tokenizer:getCurrent().location.endd
			local expression = self:parseTableConstructor()
			local arg_end = self.tokenizer:getPreviousLocation().endd

			return AstExpressionCall.new(
				Location.new(func.location, expression.location),
				func,
				expression,
				has_self,
				Location.new(arg_start, arg_end)
			)
		elseif
			self.tokenizer:getCurrent().token_type == TokenType.RawString
			or self.tokenizer:getCurrent().token_type == TokenType.QuotedString
		then
			local arg_location = self.tokenizer:getCurrent().location
			local expression = self:parseString()

			return AstExpressionCall.new(
				Location.new(func.location, expression.location),
				func,
				expression,
				has_self,
				arg_location
			)
		else
			return self:reportFunctionArgsError(func, has_self)
		end
	end,

	reportFunctionArgsError = function(self, func, has_self)
		if has_self and self.tokenizer:getCurrent().location.begin.line ~= func.location.endd.line then
			return self:reportExpressionError(func.location, func, "Expected function call arguments after '('")
		else
			return self:reportExpressionError(
				Location.new(func.location.begin, self.tokenizer:getCurrent().location.begin),
				func,
				"Expected '(', '{' or <string> when parsing function call, got %s",
				self.tokenizer:getCurrent():toString()
			)
		end
	end,

	reportAmbiguousCallError = function(self)
		self:report(
			self.tokenizer:getCurrent().location,
			"Ambiguous syntax: this looks like an argument list for a function call, "
				.. "but could also be the start of a new statement; use ';' to separate statements"
		)
	end,

	parseTableConstructor = function(self)
		local items = Vector.new()

		local start = self.tokenizer:getCurrent().location

		local match_brace = MatchToken.new(self.tokenizer:getCurrent())
		self:expectAndConsume(TokenType:getChar("{"), "table literal")
		local last_element_indent = 0

		while self.tokenizer:getCurrent().token_type ~= TokenType:getChar("}") do
			last_element_indent = self.tokenizer:getCurrent().location.begin.column

			if self.tokenizer:getCurrent().token_type == TokenType:getChar("[") then
				local match_location_bracket = MatchToken.new(self.tokenizer:getCurrent())
				self:nextToken()

				local key = self:parseExpression()

				self:expectMatchAndConsume(TokenType:getChar("]"), match_location_bracket)

				self:expectAndConsume(TokenType:getChar("="), "table field")

				local value = self:parseExpression()

				items:pushBack(AstExpressionTable.Item.new(AstExpressionTable.Item.enum.General, key, value))
			elseif
				self.tokenizer:getCurrent().token_type == TokenType.Name
				and self.tokenizer:lookAhead().token_type == TokenType:getChar("=")
			then
				local name = self:parseName("table field")

				self:expectAndConsume(TokenType:getChar("="), "table field")

				local name_string = name.name.value

				local key = AstExpressionConstantString.new(name.location, name_string):asExpression()
				local value = self:parseExpression():asExpression()

				local func = value:as(AstExpressionFunction)
				if func then
					func.debug_name = name.name
				end

				items:pushBack(AstExpressionTable.Item.new(AstExpressionTable.Item.enum.Record, key, value))
			else
				local expression = self:parseExpression()

				items:pushBack(AstExpressionTable.Item.new(AstExpressionTable.Item.enum.List, nil, expression))
			end

			if
				self.tokenizer:getCurrent().token_type == TokenType:getChar(",")
				or self.tokenizer:getCurrent().token_type == TokenType:getChar(";")
			then
				self:nextToken()
			elseif
				(
					self.tokenizer:getCurrent().token_type == TokenType:getChar("[")
					or self.tokenizer:getCurrent().token_type == TokenType.Name
				) and self.tokenizer:getCurrent().location.begin.column == last_element_indent
			then
				self:report(self.tokenizer:getCurrent().location, "Expected ',' after table constructor element")
			elseif self.tokenizer:getCurrent().token_type ~= TokenType:getChar("}") then
				break
			end
		end

		local endd = self.tokenizer:getCurrent().location

		if not self:expectMatchAndConsume(TokenType:getChar("}"), match_brace) then
			endd = self.tokenizer:getPreviousLocation()
		end

		return AstExpressionTable.new(Location.new(start, endd), items)
	end,

	parseIfElseExpression = function(self)
		local has_else = false
		local start = self.tokenizer:getCurrent().location

		self:nextToken() -- skip if / elseif

		local condition = self:parseExpression()

		local has_then = self:expectAndConsume(TokenType.ReservedThen, "if then else expression")

		local true_expression = self:parseExpression()
		local false_expression

		if self.tokenizer:getCurrent().token_type == TokenType.ReservedElseif then
			local recursion_counter_old = self.recursion_counter
			self:incrementRecursionCounter("expression")
			has_else = true
			false_expression = self:parseIfElseExpression()
			self.recursion_counter = recursion_counter_old
		else
			has_else = self:expectAndConsume(TokenType.ReservedElse, "if then else expression")
			false_expression = self:parseExpression()
		end

		local endd = false_expression.location

		return AstExpressionIfElse.new(
			Location.new(start, endd),
			condition,
			has_then,
			true_expression,
			has_else,
			false_expression
		)
	end,

	parseNameOptional = function(self, context)
		if self.tokenizer:getCurrent().token_type ~= TokenType.Name then
			self:reportNameError(context)
			return
		end

		local result = Name.new(AstName.new(self.tokenizer:getCurrent().value), self.tokenizer:getCurrent().location)

		self:nextToken()

		return result
	end,

	parseName = function(self, context)
		local name = self:parseNameOptional(context)
		if name then
			return name
		end

		local location = self.tokenizer:getCurrent().location
		location.endd = location.begin

		return Name.new(self.name_error, location)
	end,

	parseIndexName = function(self, context, previous)
		local name = self:parseNameOptional(context)
		if name then
			return name
		end

		-- If we have a reserved keyword next at the same line, assume it's an incomplete name
		if
			self.tokenizer:getCurrent().token_type >= TokenType.Reserved_BEGIN
			and self.tokenizer:getCurrent().token_type < TokenType.Reserved_END
			and self.tokenizer:getCurrent().location.begin.line == previous.line
		then
			local result =
				Name.new(AstName.new(self.tokenizer:getCurrent().value), self.tokenizer:getCurrent().location)

			self:nextToken()

			return result
		end

		local location = self.tokenizer:getCurrent().location
		location.endd = location.begin

		return Name.new(self.name_error, location)
	end,

	parseGenericTypeList = function(self, with_default_values)
		local names = Vector.new()
		local name_packs = Vector.new()

		if self.tokenizer:getCurrent().token_type == TokenType:getChar("<") then
			local begin = self.tokenizer:getCurrent()
			self:nextToken()

			local seen_pack = false
			local seen_default = false

			while true do
				local name_location = self.tokenizer:getCurrent().location
				local name = self:parseName().name
				if self.tokenizer:getCurrent().token_type == TokenType.Dot3 or seen_pack then
					seen_pack = true

					if self.tokenizer:getCurrent().token_type ~= TokenType.Dot3 then
						self:report(
							self.tokenizer:getCurrent().location,
							"Generic types come before generic type packs"
						)
					else
						self:nextToken()
					end

					if with_default_values and self.tokenizer:getCurrent().token_type == TokenType:getChar("=") then
						seen_default = true
						self:nextToken()

						if shouldParseTypePack(self.tokenizer) then
							local type_pack = self:parseTypePack()

							name_packs:pushBack(AstGenericTypePack.new(name, name_location, type_pack))
						else
							local ttype, type_pack = self:parseTypeOrPack()

							if ttype then
								self:report(ttype.location, "Expected type pack after '=', got type")
							end

							name_packs:pushBack(AstGenericTypePack.new(name, name_location, type_pack))
						end
					else
						if seen_default then
							self:report(
								self.tokenizer:getCurrent().location,
								"Expected default type pack after type pack name"
							)
						end

						name_packs:pushBack(AstGenericTypePack.new(name, name_location, nil))
					end
				else
					if seen_default then
						self:report(self.tokenizer:getCurrent().location, "Expected default type after type name")
					end

					names:pushBack(AstGenericType.new(name, name_location, nil))
				end

				if self.tokenizer:getCurrent().token_type == TokenType:getChar(",") then
					self:nextToken()

					if self.tokenizer:getCurrent().token_type == TokenType:getChar(">") then
						self:report(self.tokenizer:getCurrent().location, "Expected type after ',' but got '<' instead")
						break
					end
				else
					break
				end
			end

			self:expectMatchAndConsume(TokenType:getChar(">"), begin)
		end

		local generics = names
		local generic_packs = name_packs
		return generics, generic_packs
	end,

	parseTypeParams = function(self)
		local parameters = Vector.new()

		if self.tokenizer:getCurrent().token_type == TokenType:getChar("<") then
			local begin = self.tokenizer:getCurrent()
			self:nextToken()

			while true do
				if shouldParseTypePack(self.tokenizer) then
					local type_pack = self:parseTypePack()

					parameters:pushBack(AstTypeOrPack.new(nil, type_pack))
				elseif self.tokenizer:getCurrent().token_type == TokenType:getChar("(") then
					local ttype, type_pack = self:parseTypeOrPack()

					if type_pack then
						parameters:pushBack(AstTypeOrPack.new(nil, type_pack))
					else
						parameters:pushBack(AstTypeOrPack.new(ttype, nil))
					end
				elseif self.tokenizer:getCurrent().token_type == TokenType:getChar("<") and parameters:isEmpty() then
					break
				else
					parameters:pushBack(AstTypeOrPack.new(self:parseType(), nil))
				end

				if self.tokenizer:getCurrent().token_type == TokenType:getChar(",") then
					self:nextToken()
				else
					break
				end
			end

			self:expectMatchAndConsume(TokenType:getChar(">"), begin)
		end

		return parameters
	end,

	parseCharArray = function(self)
		assert(
			self.tokenizer:getCurrent().token_type == TokenType.QuotedString
				or self.tokenizer:getCurrent().token_type == TokenType.RawString
				or self.tokenizer:getCurrent().token_type == TokenType.InterpolatedStringSimple,
			"parseCharArray saw an invalid current token type: " .. tostring(self.tokenizer:getCurrent().token_type)
		)

		local data = self.tokenizer:getCurrent().value

		if
			self.tokenizer:getCurrent().token_type == TokenType.QuotedString
			or self.tokenizer:getCurrent().token_type == TokenType.InterpolatedStringSimple
		then
			if not self.tokenizer:fixupQuotedString(data) then
				self:nextToken()
				return nil
			end
		else
			self.tokenizer:fixupMultilineString(data)
		end

		self:nextToken()
		return data
	end,

	parseString = function(self)
		local location = self.tokenizer:getCurrent().location
		local value = self:parseCharArray()
		if value then
			return AstExpressionConstantString.new(location, value)
		else
			return self:reportExpressionError(location, nil, "String literal contains maliformed escape sequence")
		end
	end,

	parseInterpolatedString = function(self)
		local strings = Vector.new()
		local expressions = Vector.new()

		local start_location = self.tokenizer:getCurrent().location
		local end_location

		repeat
			local current_token = self.tokenizer:getCurrent()
			assert(
				current_token.token_type == TokenType.InterpolatedStringBegin
					or current_token.token_type == TokenType.InterpolatedStringMid
					or current_token.token_type == TokenType.InterpolatedStringEnd
					or current_token.token_type == TokenType.InterpolatedStringSimple,
				"parseInterpolatedString saw an invalid current token type: "
					.. tostring(self.tokenizer:getCurrent().token_type)
			)

			end_location = current_token.location

			self.scratch_data = current_token.value

			if not self.tokenizer:fixupQuotedString(self.scratch_data) then
				self:nextToken()
				return self:reportExpressionError(
					Location.new(start_location, end_location),
					nil,
					"Interpolated string literal contains maliformed escape sequence"
				)
			end

			local chars = AstArray.new(self.scratch_data)

			self:nextToken()

			strings:pushBack(chars)

			if
				current_token.token_type == TokenType.InterpolatedStringEnd
				or current_token.token_type == TokenType.InterpolatedStringSimple
			then
				break
			end

			local got_error_while_checking = false

			local current_type = self.tokenizer:getCurrent().token_type
			if current_type == TokenType.InterpolatedStringMid or current_type == TokenType.InterpolatedStringEnd then
				got_error_while_checking = true
				self:nextToken()
				expressions:pushBack(
					self:reportExpressionError(
						end_location,
						nil,
						"Maliformed interpolated string, expected expression inside '{}'"
					)
				)
			elseif current_type == TokenType.BrokenString then
				got_error_while_checking = true
				self:nextToken()
				expressions:pushBack(
					self:reportExpressionError(
						end_location,
						nil,
						"Maliformed interpolated string, did you forget to add a '`'?"
					)
				)
			else
				expressions:pushBack(self:parseExpression())
			end

			if got_error_while_checking then
				break
			end

			current_type = self.tokenizer:getCurrent().token_type
			if
				current_type == TokenType.InterpolatedStringBegin
				or current_type == TokenType.InterpolatedStringMid
				or current_type == TokenType.InterpolatedStringEnd
			then
			elseif current_type == TokenType.BrokenInterpolatedDoubleBrace then
				self:nextToken()
				return self:reportExprError(end_location, nil, ERROR_INVALID_INTERP_DOUBLE_BRACE)
			elseif current_type == TokenType.BrokenString then
				self:nextToken()
				return self:reportExprError(
					end_location,
					nil,
					"Malformed interpolated string, did you forget to add a '}'?"
				)
			else
				return self:reportExprError(
					end_location,
					nil,
					"Malformed interpolated string, got %s",
					self.tokenizer:getCurrent():toString()
				)
			end
		until false

		local strings_array = strings
		local expressions_array = expressions
		return AstExpressionInterpolatedString.new(
			Location.new(start_location, end_location),
			strings_array,
			expressions_array
		)
	end,

	parseNumber = function(self)
		local start = self.tokenizer:getCurrent().location

		self.scratch_data = self.tokenizer:getCurrent().value

		-- Remove all internal underscores as they don't hold any meaning and this allows parsing code to just pass the string pointer to strtod et al
		if self.scratch_data:find("_") then
			self.scratch_data = (self.scratch_data:gsub("_", ""))
		end

		local result, value = parseDouble(self.scratch_data)
		self:nextToken()

		if result == ConstantNumberParseResult.Maliformed then
			return self:reportExpressionError(start, nil, "Maliformed number")
		end

		return AstExpressionConstantNumber.new(start, value, result)
	end,

	pushLocal = function(self, binding)
		local name = binding.name
		local llocal = self.local_map[name.name]

		llocal = AstLocal.new(
			name.name,
			name.location, --[[shadow =]]
			llocal,
			self.function_stack.size,
			self.function_stack:getBack().loop_depth,
			binding.annotation
		)
		self.local_stack:pushBack(llocal)

		return llocal
	end,

	saveLocals = function(self)
		return self.local_stack.size
	end,

	restoreLocals = function(self, offset)
		local i = self.local_stack.size
		while i > offset do
			local l = self.local_stack[i]

			self.local_map[l.name] = l.shadow
			i = i - 1
		end

		self.local_stack:resize(offset)
	end,

	expectAndConsume = function(self, ttype, context)
		if self.tokenizer:getCurrent().token_type ~= ttype then
			self:expectAndConsumeFail(ttype, context)

			-- check if this is an extra token and the expected token is next
			if self.tokenizer:lookAhead().token_type == ttype then
				-- skip invalid and consume expected
				self:nextToken()
				self:nextToken()
			end

			return false
		else
			self:nextToken()
			return true
		end
	end,

	expectAndConsumeFail = function(self, ttype, context)
		local type_string = Token.new(Location.new(Position.new(0, 0), 0), ttype):toString()
		local current_token_string = self.tokenizer:getCurrent():toString()

		if context then
			self:report(
				self.tokenizer:getCurrent().location,
				"Expected %s when parsing %s, got %s",
				type_string,
				context,
				current_token_string
			)
		else
			self:report(self.tokenizer:getCurrent().location, "Expected %s, got %s", type_string, current_token_string)
		end
	end,

	expectMatchAndConsume = function(self, ttype, begin, search_for_missing)
		if self.tokenizer:getCurrent().token_type ~= ttype then
			self:expectMatchAndConsumeFail(ttype, begin)

			return self:expectMatchAndConsumeRecover(ttype, begin, search_for_missing)
		else
			self:nextToken()

			return true
		end
	end,

	expectMatchAndConsumeRecover = function(self, ttype, begin, search_for_missing)
		if search_for_missing then
			-- previous location is taken because 'current' token is already the next token
			local current_line = self.tokenizer:getPreviousLocation().endd.line

			-- search to the end of the line for expected token
			-- we will also stop if we hit a token that can be handled by parsing function above the current one
			local token_type = self.tokenizer:getCurrent().token_type

			while
				current_line == self.tokenizer:getCurrent().location.begin.line
				and token_type ~= ttype
				and self.match_recovery_stop_on_token[ttype.value] == 0
			do
				self:nextToken()
				token_type = self.tokenizer:getCurrent().token_type
			end

			if token_type == ttype then
				self:nextToken()
				return true
			end
		else
			-- check if this is an extra token and the expected token is next
			if self.tokenizer:lookAhead().token_type == ttype then
				-- skip invalid and consume expected
				self:nextToken()
				self:nextToken()

				return true
			end
		end

		return false
	end,

	expectMatchAndConsumeFail = function(self, ttype, begin, extra)
		local type_string = Token.new(Location.new(Position.new(0, 0), 0), ttype):toString()
		local match_string = Token.new(Location.new(Position.new(0, 0), 0), begin.ttype):toString()

		if self.tokenizer:getCurrent().location.begin.line == begin.position.line then
			self:report(
				self.tokenizer:getCurrent().location,
				"Expected %s (to close %s at column %d), got %s%s",
				type_string,
				match_string,
				begin.position.column + 1,
				self.tokenizer:getCurrent():toString(),
				extra or ""
			)
		else
			self:report(
				self.tokenizer:getCurrent().location,
				"Expected %s (to close %s at line %d), got %s%s",
				type_string,
				match_string,
				begin.position.line + 1,
				self.tokenizer:getCurrent():toString(),
				extra or ""
			)
		end
	end,

	expectMatchEndAndConsume = function(self, ttype, begin)
		if self.tokenizer:getCurrent().token_type ~= ttype then
			self:expectMatchEndAndConsumeFail(ttype, begin)

			-- check if this is an extra token and the expected token is next
			if self.tokenizer:lookAhead().token_type == ttype then
				-- skip invalid and consume expected
				self:nextToken()
				self:nextToken()

				return true
			end

			return false
		else
			-- If the token matches on a different line and a different column, it suggests misleading indentation
			-- This can be used to pinpoint the problem location for a possible future *actual* mismatch
			if
				self.tokenizer:getCurrent().location.begin.line ~= begin.position.line
				and self.tokenizer:getCurrent().location.begin.column ~= begin.position.column
				and self.end_mismatch_suspect.position.line < begin.position.line
			then -- Only replace the previous suspect with more recent suspects
				self.end_mismatch_suspect = begin
			end

			self:nextToken()
			return true
		end
	end,

	expectMatchEndAndConsumeFail = function(self, ttype, begin)
		if
			self.end_mismatch_suspect.ttype ~= TokenType.Eof
			and self.end_mismatch_suspect.position.line > begin.position.line
		then
			local match_string = Token.new(Location.new(Position.new(0, 0), 0), self.end_mismatch_suspect.ttype)
				:toString()
			local suggestion = ("; did you forget to close %s at line %d?"):format(
				match_string,
				self.end_mismatch_suspect.position.line + 1
			)
			self:expectMatchAndConsumeFail(ttype, begin, suggestion)
		else
			self:expectMatchAndConsumeFail(ttype, begin)
		end
	end,

	incrementRecursionCounter = function(self, context)
		self.recursion_counter = self.recursion_counter + 1
	end,

	report = function(self, location, format_string, ...)
		-- To reduce number of errors reported to user for incomplete statements, we skip multiple errors at the same location
		-- For example, consider 'local a = (((b + ' where multiple tokens haven't been written yet
		if (not self.parse_errors:isEmpty()) and location == self.parse_errors:getBack():getLocation() then
			return
		end

		local message = format_string:format(...)
		self.parse_errors:pushBack(ParseError.new(location, message))
	end,

	reportNameError = function(self, context)
		if context then
			self:report(
				self.tokenizer:getCurrent().location,
				"Expected identifier when parsing %s, got %s",
				context,
				self.tokenizer:getCurrent():toString()
			)
		else
			self:report(
				self.tokenizer:getCurrent().location,
				"Expected identifier, got %s",
				self.tokenizer:getCurrent():toString()
			)
		end
	end,

	reportStatError = function(self, location, expressions, statements, format_string, ...)
		self:report(location, format_string, ...)

		return AstStatError.new(location, expressions, statements, self.parse_errors.size - 1)
	end,

	reportExpressionError = function(self, location, expressions, format_string, ...)
		self:report(location, format_string, ...)

		return AstExpressionError.new(location, expressions, self.parse_errors.size - 1)
	end,

	reportTypeError = function(self, location, types, format_string, ...)
		self:report(location, format_string, ...)

		return AstTypeError.new(location, types, false, self.parse_errors.size - 1)
	end,

	reportMissingTypeError = function(self, parse_error_location, ast_error_location, format_string, ...)
		self:report(parse_error_location, format_string, ...)

		return AstTypeError.new(ast_error_location, AstArray.new(), true, self.parse_errors.size - 1)
	end,

	nextToken = function(self)
		local token_type = self.tokenizer:next(--[[skip_comments =]] false, true).token_type

		while
			token_type == TokenType.BrokenComment
			or token_type == TokenType.Comment
			or token_type == TokenType.BlockComment
		do
			local token = self.tokenizer:getCurrent()

			if self.options.capture_comments then
				self.comment_locations:pushBack(Comment.new(token.token_type, token.location))
			end

			-- Subtlety: Broken comments are weird because we record them as comments AND pass them to the parser as a token.
			-- The parser will turn this into a proper syntax error.
			if token.token_type == TokenType.BrokenComment then
				return
			end

			-- Comments starting with ! are called "hot comments" and contain directives for type checking / linting / compiling
			if token.token_type == TokenType.Comment and token.value:sub(1, 1) == "!" then
				local text = token.value

				local endd = #text
				while endd > 0 and isSpace(text:sub(endd, endd)) do
					endd = endd - 1
				end

				self.hot_comments:pushBack(HotComment.new(self.hot_comment_header, token.location, text:sub(2, endd)))
			end

			token_type = self.tokenizer:next(--[[skipComments =]] false, --[[updatePrevLocation =]] false).token_type
		end
	end,
})

return Parser

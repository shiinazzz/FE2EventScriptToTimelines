-- based off of luau's Lexer (https://github.com/Roblox/luau/blob/master/Ast/src/Lexer.cpp)
local shared = require("shared")
local binaryToInt = shared.binaryToInt
local class, newBuffer = shared.class, shared.newBuffer

local TokenType = shared.TokenType
local Token = shared.Token
local Location = shared.Location
local AstNameTable = shared.AstNameTable
local Vector = shared.Vector
local Position = shared.Position
local BraceType = shared.BraceType

local isSpace = shared.isSpace

local function unsigned(v)
	if v < 0 then
		return math.huge
	end
	return v
end

local a_byte = ("a"):byte()
local function isAlpha(ch)
	return unsigned(ch:lower():byte() - a_byte) < 26
end

local zero_byte = ("0"):byte()
local function isDigit(ch)
	return unsigned(ch:byte() - zero_byte) < 10
end

local function isHexDigit(ch)
	return isDigit(ch) or unsigned(ch:lower():byte() - a_byte) < 6
end

local function isNewLine(ch)
	return ch == "\n"
end

local function replaceCharacter(str, index, replacement)
	return str:sub(1, index - 1) .. replacement .. str:sub(index + 1) -- data[index] = replacement
end

local unescape_map = {
	a = "\a",
	b = "\b",
	f = "\f",
	n = "\n",
	r = "\r",
	t = "\r",
	v = "\v",
}
local function unescape(char)
	return unescape_map[char] or char
end

local bit = bit32 --require("bit");

function toUtf8(data, code)
	-- U+0000..U+007F
	if code < 0x80 then
		-- data[0] = ...
		data = utf8.char(code) .. data:sub(2)
		return data, 1
	--  U+0080..U+07FF
	elseif code < 0x800 then
		-- data[0] = ...; data[1] = ...;
		data = utf8.char(bit.bor(0xC0, bit.rshift(code, 6)))
			.. utf8.char(bit.bor(0x80, bit.band(code, 0x3F)))
			.. data:sub(3)
		return data, 2
	-- U+0800..U+FFFF
	elseif code < 0x10000 then
		-- data[0] = ...; data[1] = ...; data[2] = ...;
		data = utf8.char(bit.bor(0xE0, bit.rshift(code, 12)))
			.. utf8.char(bit.bor(0x80, bit.band(bit.rshift(code, 6), 0x3F)))
			.. utf8.char(bit.bor(0x80, bit.band(code, 0x3F)))
			.. data:sub(4)
		return data, 3
	-- U+10000..U+10FFFF
	elseif code < 0x110000 then
		-- data[0] = ...; data[1] = ...; data[2] = ...; data[3] = ...;
		data = utf8.char(bit.bor(0xF0, bit.rshift(code, 18)))
			.. utf8.char(bit.bor(0x80, bit.band(bit.rshift(code, 12), 0x3F)))
			.. utf8.char(bit.bor(0x80, bit.band(bit.rshift(code, 6), 0x3F)))
			.. utf8.char(bit.bor(0x80, bit.band(code, 0x3F)))
			.. data:sub(5)
		return data, 4
	else
		return data, 0
	end
end

local Tokenizer = class(function(self, content, names)
	self.content = content
	self.buffer, self.length = newBuffer(content)

	self.skip_comments = false

	self.offset = 1
	self.line = 1
	self.line_offset = 1

	self.previous_location = Location.new()

	self.token = Token.new(Location.new(nil, 0), TokenType.Eof)

	self.names = names or AstNameTable.new()
	self.brace_stack = Vector.new()
end, {
	class_name = "Tokenizer",
	getCurrent = function(self)
		return self.token
	end,
	getPreviousLocation = function(self)
		return self.previous_location
	end,

	setSkipComments = function(self, value)
		self.skip_comments = value
	end,

	next = function(self, skip_comments, update_previous_loation)
		if type(skip_comments) == "nil" then
			skip_comments = self.skip_comments
		end
		if type(update_previous_loation) == "nil" then
			update_previous_loation = true
		end

		repeat
			while isSpace(self:peek()) do
				self:consume()
			end

			if update_previous_loation then
				self.previous_location = self.token.location
			end

			self.token = self:readNext()
			update_previous_loation = false
		until not (
				skip_comments and (
					self.token.token_type == Token.Comment or self.token.token_type == Token.BlockComment
				)
			)

		return self.token
	end,
	nextLine = function(self)
		while self:peek() ~= "\r" and not isNewLine(self:peek()) do
			self:consume()
		end
		self:next()
	end,

	lookAhead = function(self)
		local current_offset = self.offset
		local current_line = self.line
		local current_line_offset = self.line_offset
		local current_token = self.token
		local current_previous_location = self.previous_location

		local result = self:next()

		self.offset = current_offset
		self.line = current_line
		self.line_offset = current_line_offset
		self.token = current_token
		self.previous_location = current_previous_location

		return result
	end,

	peek = function(self, look_ahead)
		look_ahead = look_ahead or 0
		return ((self.offset + look_ahead < self.length) and self.buffer[self.offset + look_ahead]) or 0
	end,

	getPosition = function(self)
		return Position.new(self.line, self.offset - self.line_offset)
	end,

	consume = function(self)
		if isNewLine(self.buffer[self.offset]) then
			self.line = self.line + 1
			self.line_offset = self.offset + 1
		end
		self.offset = self.offset + 1
	end,

	readCommentBody = function(self)
		local start = self:getPosition()

		assert(self:peek() == "-" and self:peek(1) == "-")
		self:consume()
		self:consume()

		local start_offset = self.offset

		if self:peek() == "[" then
			local separator = self:skipLongSeparator()
			if separator >= 0 then
				return self:readLongString(start, separator, TokenType.BlockComment, TokenType.BrokenComment)
			end
		end

		while self:peek() ~= 0 and self:peek() ~= "\r" and not isNewLine(self:peek()) do
			self:consume()
		end

		return Token.new(
			Location.new(start, self:getPosition()),
			TokenType.Comment,
			self.buffer:fromTo(start_offset, self.offset)
		)
	end,

	skipLongSeparator = function(self)
		local start = self:peek()

		assert(start == "[" or start == "]")
		self:consume()

		local count = 0

		while self:peek() == "=" do
			self:consume()
			count = count + 1
		end

		return (start == self:peek() and count) or -count - 1
	end,

	readLongString = function(self, start, separator, ok, broken)
		assert(self:peek() == "[")
		self:consume()

		local start_offset = self.offset

		while self:peek() ~= 0 do
			if self:peek() == "]" then
				if self:skipLongSeparator() == separator then
					assert(self:peek() == "]")
					self:consume()

					local end_offset = self.offset - separator - 2
					assert(end_offset >= start_offset)

					return Token.new(
						Location.new(start, self:getPosition()),
						ok,
						self.buffer:fromTo(start_offset, end_offset)
					)
				end
			else
				self:consume()
			end
		end

		return Token.new(Location.new(start, self:getPosition()), broken)
	end,

	readBackslashInString = function(self)
		assert(self:peek() == "\\")
		self:consume()

		local ch = self:peek()
		if ch == "\r" then
			self:consume()
			if self:peek() == "\n" then
				self:consume()
			end
		elseif ch == 0 then
		elseif ch == "z" then
			self:consume()
			while isSpace(self:peek()) do
				self:consume()
			end
		else
			self:consume()
		end
	end,

	readQuotedString = function(self)
		local start = self:getPosition()

		local delimeter = self:peek()
		assert(delimeter == "'" or delimeter == '"')
		self:consume()

		local start_offset = self.offset

		while self:peek() ~= delimeter do
			local ch = self:peek()

			if ch == 0 or ch == "\r" or ch == "\n" then
				return Token.new(Location.new(start, self:getPosition()), TokenType.BrokenString)
			elseif ch == "\\" then
				self:readBackslashInString()
			else
				self:consume()
			end
		end

		self:consume()

		return Token.new(
			Location.new(start, self:getPosition()),
			TokenType.QuotedString,
			self.buffer:fromTo(start_offset, self.offset - 2)
		)
	end,

	readInterpolatedStringBegin = function(self)
		assert(self:peek() == "`")

		local start = self:getPosition()
		self:consume()

		return self:readInterpolatedStringSection(
			start,
			TokenType.InterpolatedStringBegin,
			TokenType.InterpolatedStringSimple
		)
	end,

	readInterpolatedStringSection = function(self, start, format_type, end_type)
		local start_offset = self.offset

		while self:peek() ~= "`" do
			local ch = self:peek()
			if ch == 0 or ch == "\r" or ch == "\n" then
				return Token.new(Location.new(start, self:getPosition()), TokenType.BrokenString)
			elseif ch == "\\" then
				-- Allow for \u{}, which would otherwise be consumed by looking for {
				if self:peek(1) == "u" and self:peek(2) == "{" then
					self:consume() -- backslash
					self:consume() -- u
					self:consume() -- {
				else
					self:readBackslashInString()
				end
			elseif ch == "{" then
				self.brace_stack:pushBack(BraceType.InterpolatedString)

				if self:peek(1) == "{" then
					local broken_double_brace = Token.new(
						Location.new(start, self:getPosition()),
						TokenType.BrokenInterpolatedDoubleBrace,
						self.buffer:fromTo(start_offset, self.offset)
					)
					self:consume()
					self:consume()
					return broken_double_brace
				end

				self:consume()
				return Token.new(
					Location.new(start, self:getPosition()),
					format_type,
					self.buffer:fromTo(start_offset, self.offset - 1)
				)
			else
				self:consume()
			end
		end

		self:consume()

		return Token.new(
			Location.new(start, self:getPosition()),
			end_type,
			self.buffer:fromTo(start_offset, self.offset - 1)
		)
	end,

	readNumber = function(self, start, start_offset)
		assert(isDigit(self:peek()))

		repeat
			self:consume()
		until not (isDigit(self:peek()) or self:peek() == "." or self:peek() == "_")

		if self:peek() == "e" or self:peek() == "E" then
			self:consume()

			if self:peek() == "+" or self:peek() == "-" then
				self:consume()
			end
		end

		while isAlpha(self:peek()) or isDigit(self:peek()) or self:peek() == "_" do
			self:consume()
		end

		return Token.new(
			Location.new(start, self:getPosition()),
			TokenType.Number,
			self.buffer:fromTo(start_offset, self.offset - 1)
		)
	end,

	readName = function(self)
		assert(isAlpha(self:peek()) or self:peek() == "_")

		local start_offset = self.offset

		repeat
			self:consume()
		until not (isAlpha(self:peek()) or isDigit(self:peek()) or self:peek() == "_")

		return self.names:getOrAddWithType(self.buffer:fromTo(start_offset, self.offset - 1))
	end,

	readNext = function(self)
		local start = self:getPosition()

		local ch = self:peek()
		if ch == 0 then
			return Token.new(Location.new(start, 0), TokenType.Eof)
		elseif ch == "-" then
			if self:peek(1) == ">" then
				self:consume()
				self:consume()
				return Token.new(Location.new(start, 2), TokenType.SkinnyArrow)
			elseif self:peek(1) == "=" then
				self:consume()
				self:consume()
				return Token.new(Location.new(start, 2), TokenType.SubAssign)
			elseif self:peek(1) == "-" then
				return self:readCommentBody()
			else
				self:consume()
				return Token.new(Location.new(start, 1), TokenType:getChar("-"))
			end
		elseif ch == "[" then
			local sep = self:skipLongSeparator()
			if sep >= 0 then
				return self:readLongString(start, sep, TokenType.RawString, TokenType.BrokenString)
			elseif sep == -1 then
				return Token.new(Location.new(start, 1), TokenType:getChar("["))
			else
				return Token.new(Location.new(start, self:getPosition()), TokenType.BrokenString)
			end
		elseif ch == "{" then
			self:consume()

			if self.brace_stack:isEmpty() then
				self.brace_stack:pushBack(BraceType.Normal)
			end

			return Token.new(Location.new(start, 1), TokenType:getChar("{"))
		elseif ch == "}" then
			self:consume()

			if self.brace_stack:isEmpty() then
				return Token.new(Location.new(start, 1), TokenType:getChar("}"))
			end

			local brace_stack_top = self.brace_stack:getBack()
			self.brace_stack:popBack()

			if brace_stack_top ~= BraceType.InterpolatedString then
				return Token.new(Location.new(start, 1), TokenType:getChar("}"))
			end

			return self:readInterpolatedStringSection(
				self:getPosition(),
				TokenType.InterpolatedStringMid,
				TokenType.InterpolatedStringEnd
			)
		elseif ch == "=" then
			self:consume()

			if self:peek() == "=" then
				self:consume()
				return Token.new(Location.new(start, 2), TokenType.Equal)
			else
				return Token.new(Location.new(start, 1), TokenType:getChar("="))
			end
		elseif ch == "<" then
			self:consume()

			if self:peek() == "=" then
				self:consume()
				return Token.new(Location.new(start, 2), TokenType.LessEqual)
			else
				return Token.new(Location.new(start, 1), TokenType:getChar("<"))
			end
		elseif ch == ">" then
			self:consume()

			if self:peek() == "=" then
				self:consume()
				return Token.new(Location.new(start, 2), TokenType.GreaterEqual)
			else
				return Token.new(Location.new(start, 1), TokenType:getChar(">"))
			end
		elseif ch == "~" then
			self:consume()

			if self:peek() == "=" then
				self:consume()
				return Token.new(Location.new(start, 2), TokenType.NotEqual)
			else
				return Token.new(Location.new(start, 1), TokenType:getChar("~"))
			end
		elseif ch == '"' or ch == "'" then
			return self:readQuotedString()
		elseif ch == "`" then
			return self:readInterpolatedStringBegin()
		elseif ch == "." then
			self:consume()

			if self:peek() == "." then
				self:consume()

				if self:peek() == "." then
					self:consume()

					return Token.new(Location.new(start, 3), TokenType.Dot3)
				elseif self:peek() == "=" then
					self:consume()

					return Token.new(Location.new(start, 3), TokenType.ConcatAssign)
				else
					return Token.new(Location.new(start, 2), TokenType.Dot2)
				end
			elseif isDigit(self:peek()) then
				return self:readNumber(start, self.offset - 1)
			else
				return Token.new(Location.new(start, 1), TokenType:getChar("."))
			end
		elseif ch == "+" then
			self:consume()

			if self:peek() == "=" then
				self:consume()
				return Token.new(Location.new(start, 2), TokenType.AddAssign)
			else
				return Token.new(Location.new(start, 1), TokenType:getChar("+"))
			end
		elseif ch == "/" then
			self:consume()

			if self:peek() == "=" then
				self:consume()
				return Token.new(Location.new(start, 2), TokenType.DivAssign)
			else
				return Token.new(Location.new(start, 1), TokenType:getChar("/"))
			end
		elseif ch == "*" then
			self:consume()

			if self:peek() == "=" then
				self:consume()
				return Token.new(Location.new(start, 2), TokenType.MulAssign)
			else
				return Token.new(Location.new(start, 1), TokenType:getChar("*"))
			end
		elseif ch == "%" then
			self:consume()

			if self:peek() == "=" then
				self:consume()
				return Token.new(Location.new(start, 2), TokenType.ModAssign)
			else
				return Token.new(Location.new(start, 1), TokenType:getChar("%"))
			end
		elseif ch == "^" then
			self:consume()

			if self:peek() == "=" then
				self:consume()
				return Token.new(Location.new(start, 2), TokenType.PowAssign)
			else
				return Token.new(Location.new(start, 1), TokenType:getChar("^"))
			end
		elseif ch == ":" then
			self:consume()

			if self:peek() == "=" then
				self:consume()
				return Token.new(Location.new(start, 2), TokenType.DoubleColon)
			else
				return Token.new(Location.new(start, 1), TokenType:getChar(":"))
			end
		elseif ch == "(" or ch == ")" or ch == "]" or ch == ";" or ch == "," or ch == "#" then
			ch = self:peek()
			self:consume()

			return Token.new(Location.new(start, 1), TokenType:getChar(ch))
		elseif isDigit(ch) then
			return self:readNumber(start, self.offset)
		elseif isAlpha(ch) or ch == "_" then
			local first, second = self:readName()

			return Token.new(Location.new(start, self:getPosition()), second, first.value)
		else
			ch = self:peek()
			self:consume()

			return Token.new(Location.new(start, 1), TokenType:getChar(ch))
		end
	end,

	-- fixupQuotedString = function(self, data)
	--     if #data < 1 or not data:find('\\') then
	--         return true;
	--     end;
	-- end,
	fixupQuotedString = function(self, data)
		if #data < 1 or not data:find("\\") then
			return data, true
		end

		local size = #data
		local write = 0

		local index = 1
		while (index == 1) or (index < size) do
			local do_continue = false
			if data:sub(index, index) ~= "\\" then
				write = write + 1
				data = replaceCharacter(data, write, data:sub(index, index)) -- data[write] = data[index]
				index = index + 1
				do_continue = true
			end

			if not do_continue then
				if index + 1 == size then
					return data, false
				end

				local escape = data:sub(index + 1, index + 1)
				index = index + 2 -- skip \e

				if escape == "\n" then
					write = write + 1
					data = replaceCharacter(data, write, "\n") -- data[write] = "\n"
				elseif escape == "\r" then
					write = write + 1
					data = replaceCharacter(data, write, "\n") -- data[write] = "\n"
					if index < size and data:sub(index, index) == "\n" then
						index = index + 1
					end
					break
				elseif escape == "\0" then
					return data, false
				elseif escape == "x" then
					-- hex escape codes are exactly 2 hex digits long
					if index + 2 > size then
						return data, false
					end

					local code = -1

					for j = 0, 1 do
						local ch = data:sub(index + j, index + j)
						if not isHexDigit(ch) then
							return data, false
						end

						-- cpp says "... convert to lower case"
						-- this is probably wrong
						-- code = string.byte(string.char((code ~= -1 and code) or string.byte(ch)):lower());
						code = 16 * code + ((isDigit(ch) and (ch:byte() - ("0"):byte())) or ch:lower():byte())
					end

					write = write + 1
					data = replaceCharacter(data, write, string.char(code))
				elseif escape == "z" then
					while index < size and isSpace(data:sub(index, index)) do
						index = index + 1
					end
				elseif escape == "u" then
					-- unicode escape codes are at least 3 characters including braces
					if index + 3 > size then
						return data, false
					end

					if data:sub(index, index) ~= "{" then
						return data, false
					end
					index = index + 1

					if data:sub(index, index) ~= "}" then
						return data, false
					end

					local code = 0

					for _ = 0, 15 do
						if index == size then
							return data, false
						end

						local ch = data:sub(index, index)

						if ch == "}" then
							break
						end

						if not isHexDigit(ch) then
							return data, false
						end

						-- cpp says "... convert to lower case"
						-- this is probably wrong
						-- code = string.byte(string.char((code ~= -1 and code) or string.byte(ch)):lower());
						code = 16 * code + ((isDigit(ch) and (ch:byte() - ("0"):byte())) or ch:lower():byte())
						index = index + 1
					end

					if index == size or data:sub(index, index) ~= "}" then
						return data, false
					end
					index = index + 1

					local utf8code = toUtf8(data:sub(write, write), code)
					if utf8code == 0 then
						return data, false
					end

					write = write + utf8code
				else
					if isDigit(escape) then
						local code = escape:byte() - ("0"):byte()

						for _ = 0, 1 do
							if index == size or (not isDigit(data:sub(index, index))) then
								break
							end

							code = 10 * code + (data:sub(index, index) - ("0"):byte())
							index = index + 1
						end

						if code > 255 then
							return data, false
						end

						write = write + 1
						data = replaceCharacter(data, write, utf8.char(code))
					else
						write = write + 1
						data = replaceCharacter(data, write, unescape(escape))
					end
				end
			end
		end

		assert(write <= size)
		data = data:sub(1, write) -- the cpp code is 'data.resize()' and I *think* this is the equivalent.
		-- (from cplusplus.com, 'n' being 'write' in this case)
		-- "If 'n' is smaller than the current string length, the current value is shortened to its first 'n' character,
		-- removing the characters beyond the 'n'th."

		return data, true
	end,
	fixupMultilineString = function(self, data)
		if #data < 1 then
			return
		end
		error("IMPLEMENT FIXUP MULTILINE STRING")
	end,

	readUtf8Error = function(self)
		local start = self:getPosition()
		local codepoint = 0
		local size = 0
		if (bit.band(self:peek(), binaryToInt("10000000"))) == binaryToInt("00000000") then
			size = 1
			codepoint = bit.band(self:peek(), 0x7F)
		elseif (bit.band(self:peek(), binaryToInt("11100000"))) == binaryToInt("11000000") then
			size = 2
			codepoint = bit.band(self:peek(), binaryToInt("11111"))
		elseif (bit.band(self:peek(), binaryToInt("11110000"))) == binaryToInt("11100000") then
			size = 3
			codepoint = bit.band(self:peek(), binaryToInt("1111"))
		elseif (bit.band(self:peek(), binaryToInt("11111000"))) == binaryToInt("11110000") then
			size = 4
			codepoint = bit.band(self:peek(), binaryToInt("111"))
		else
			self:consume()
			return Token.new(Location.new(start, self:getPosition()), TokenType.BrokenUnicode)
		end

		self:consume()

		for _ = 1, size do
			if (bit.band(self:peek(), binaryToInt("11000000"))) ~= binaryToInt("10000000") then
				return Token.new(Location.new(start, self:getPosition()), TokenType.BrokenUnicode)
			end

			codepoint = bit.lshift(codepoint, 6)
			codepoint = bit.bor(codepoint, bit.band(self:peek(), binaryToInt("00111111"))) -- order of args to bor might need a switch
			self:consume()
		end

		local result = Token.new(Location.new(start, self:getPosition()), TokenType.BrokenUnicode)
		result.codepoint = codepoint
		return result
	end,
})

return Tokenizer

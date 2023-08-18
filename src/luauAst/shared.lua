-- based off of parts of luau's Ast (https://github.com/Roblox/luau/blob/master/Ast/)

local function isInstance(object, query_type)
	if type(object) ~= "table" then
		return false
	end
	local success, res = pcall(function()
		return getmetatable(object) == query_type
	end)
	if success then
		return res
	end

	return false
end

local function isSpace(ch)
	return ch == " " or ch == "\t" or ch == "\r" or ch == "\n" or ch == "\v" or ch == "\f"
end

local function class(init, properties, inherit)
	local class = {}
	for k, v in next, (inherit or {}) do
		class[k] = v
	end
	class.__index = class

	for k, v in next, (properties or {}) do
		class[k] = v
	end

	class.new = function(...)
		local object = setmetatable({
			_class = class,
		}, class)
		init(object, ...)
		return object
	end

	return class
end

local function simpleclass(properties)
	local class = {}
	class.__index = class

	if properties.name then
		class.class_name = properties.name
		properties.name = nil
	end
	class.new = function(...)
		local obj = {}
		for i, v in next, { ... } do
			obj[properties[i]] = v
		end
		obj._class = class
		return setmetatable(obj, class)
	end

	return class
end

local function enum(items)
	local value_map = {}
	local enum = setmetatable({
		class_name = "Enum",
		fromValue = function(self, value)
			return value_map[value]
				or error(
					"failed to find item with value '"
						.. tostring(value)
						.. "' in enum '"
						.. (self.name or "[UNNAMED ENUM]")
						.. "'",
					2
				)
		end,
	}, {
		__call = function(self, name)
			self.name = name
			setmetatable(self, nil)
			return self
		end,
		__index = function(self, key)
			return "'" .. tostring(key) .. "' is not a valid member of enum '" .. (self.name or "[UNNAMED ENUM]") .. "'"
		end,
	})

	enum.__tostring = function(self)
		return (enum.name or "[UNNAMED ENUM]") .. "." .. (self.ischar and "Char" or self.name)
	end
	enum.__add = function(self, other)
		return self.value + other.value
	end
	enum.__sub = function(self, other)
		return self.value - other.value
	end
	enum.__lt = function(self, other)
		return self.value < other.value
	end
	enum.__eq = function(self, other)
		return self.value == other.value
	end
	enum.__le = function(self, other)
		return self == other or self < other
	end

	function enum:getChar(ch)
		if not enum[ch] then
			local item = setmetatable({ ischar = true, name = ch, value = ch:byte() }, enum)
			enum[ch] = item
			return item
		end
		return enum[ch]
	end

	local map = {}
	local current_value = 0
	for _, v in next, items do
		local name, value
		if type(v) == "table" then
			name, value = next(v)
			if type(value) == "string" then
				value = map[value]
			end
		else
			name = v
			value = current_value
		end
		current_value = value + 1

		map[name] = value
	end

	for k, v in next, map do
		enum[k] = setmetatable({ class_name = "EnumItem", name = k, value = v }, enum)
		value_map[v] = enum[k]
	end

	return enum
end

local function newBuffer(str)
	local buffer = {
		from = function(_, from)
			return str:sub(from, -1)
		end,
		fromTo = function(_, from, to)
			return str:sub(from, to)
		end,
	}
	local i = 1
	str:gsub(".", function(ch)
		buffer[i] = ch
		i = i + 1
	end)

	return buffer, i
end

local TokenType = enum({
	{ Eof = 0 },
	{ Char_END = 256 },

	"Equal",
	"LessEqual",
	"GreaterEqual",
	"NotEqual",
	"Dot2",
	"Dot3",
	"SkinnyArrow",
	"DoubleColon",

	"InterpolatedStringBegin",
	"InterpolatedStringMid",
	"InterpolatedStringEnd",
	-- An interpolated string with no expressions (like `x`)
	"InterpolatedStringSimple",

	"AddAssign",
	"SubAssign",
	"MulAssign",
	"DivAssign",
	"ModAssign",
	"PowAssign",
	"ConcatAssign",

	"RawString",
	"QuotedString",
	"Number",
	"Name",

	"Comment",
	"BlockComment",

	"BrokenString",
	"BrokenComment",
	"BrokenUnicode",
	"BrokenInterpolatedDoubleBrace",

	"Error",

	"Reserved_BEGIN",
	{ ReservedAnd = "Reserved_BEGIN" },
	"ReservedBreak",
	"ReservedDo",
	"ReservedElse",
	"ReservedElseif",
	"ReservedEnd",
	"ReservedFalse",
	"ReservedFor",
	"ReservedFunction",
	"ReservedIf",
	"ReservedIn",
	"ReservedLocal",
	"ReservedNil",
	"ReservedNot",
	"ReservedOr",
	"ReservedRepeat",
	"ReservedReturn",
	"ReservedThen",
	"ReservedTrue",
	"ReservedUntil",
	"ReservedWhile",
	"Reserved_END",
})("TokenType")

local kReserved = {
	"and",
	"break",
	"do",
	"else",
	"elseif",
	"end",
	"false",
	"for",
	"function",
	"if",
	"in",
	"local",
	"nil",
	"not",
	"or",
	"repeat",
	"return",
	"then",
	"true",
	"until",
	"while",
}

local function isReserved(word)
	for i = TokenType.Reserved_BEGIN.value, TokenType.Reserved_END.value - 1 do
		if word == kReserved[i - TokenType.Reserved_BEGIN.value + 1] then
			return true
		end
	end
	return false
end

local function format(f, value)
	return (value and f:format(value)) or nil
end

local token_tostring_functions = {}
do
	function token_tostring_functions.Eof()
		return "<eof>"
	end
	function token_tostring_functions.Equal()
		return "'=='"
	end

	function token_tostring_functions.LessEqual()
		return "'<='"
	end

	function token_tostring_functions.GreaterEqual()
		return "'>='"
	end

	function token_tostring_functions.NotEqual()
		return "'~='"
	end

	function token_tostring_functions.Dot2()
		return "'..'"
	end

	function token_tostring_functions.Dot3()
		return "'...'"
	end

	function token_tostring_functions.SkinnyArrow()
		return "'->'"
	end

	function token_tostring_functions.DoubleColon()
		return "'::'"
	end

	function token_tostring_functions.AddAssign()
		return "'+='"
	end

	function token_tostring_functions.SubAssign()
		return "'-='"
	end

	function token_tostring_functions.MulAssign()
		return "'*='"
	end

	function token_tostring_functions.DivAssign()
		return "'/='"
	end

	function token_tostring_functions.ModAssign()
		return "'%='"
	end

	function token_tostring_functions.PowAssign()
		return "'^='"
	end

	function token_tostring_functions.ConcatAssign()
		return "'..='"
	end

	local rawstring_format = '"%s"'
	function token_tostring_functions.RawString(self)
		return format(rawstring_format, self.value) or "string"
	end
	token_tostring_functions.QuotedString = token_tostring_functions.RawString

	local interpstringbegin_format = "`%s{"
	function token_tostring_functions.InterpolatedStringBegin(self)
		return format(interpstringbegin_format, self.value) or "the beginning of an interpolated string"
	end

	local interpstringmid_format = "}%s{"
	function token_tostring_functions.InterpolatedStringMid(self)
		return format(interpstringmid_format, self.value) or "the middle of an interpolated string"
	end

	local interpstringend_format = "}%s`"
	function token_tostring_functions.InterpolatedStringEnd(self)
		return format(interpstringend_format, self.value) or "the end of an interpolated string"
	end

	local interpstringsimple_format = "`%s`"
	function token_tostring_functions.InterpolatedStringSimple(self)
		return format(interpstringsimple_format, self.value) or "interpolated string"
	end

	local number_format = "'%s'"
	function token_tostring_functions.Number(self)
		return format(number_format, self.value) or "number"
	end

	function token_tostring_functions.Name(self)
		return format(number_format, self.value) or "identifier"
	end

	function token_tostring_functions.Comment()
		return "comment"
	end

	function token_tostring_functions.BrokenString()
		return "malformed string"
	end

	function token_tostring_functions.BrokenComment()
		return "unfinished comment"
	end

	function token_tostring_functions.BrokenInterpolatedDoubleBrace()
		return "'{{', which is invalid (did you mean '\\{'?)"
	end

	-- TODO: implement BrokenUnicode tostring
	-- function token_tostring_functions.BrokenUnicode(self)
	--     return "";
	-- end;

	function token_tostring_functions.default(self)
		if self.token_type < TokenType.Char_END then
			return format("'%c'", self.token_type.value)
		elseif self.token_type >= TokenType.Reserved_BEGIN and self.token_type < TokenType.Reserved_END then
			return format("'%s'", kReserved[self.token_type - TokenType.Reserved_BEGIN + 1])
		else
			return "<unknown>"
		end
	end
end
local Token = class(function(self, location, token_type, value)
	if not isInstance(token_type, TokenType) then
		error("Expected TokenType for second arg to Token.new", 3)
	end

	self.location = location
	self.token_type = token_type
	self.value = value
	self.toString = token_tostring_functions[token_type.name] or token_tostring_functions.default
end, {
	class_name = "Token",
	__tostring = function(self)
		return self:toString()
	end,
})

local BraceType = enum({
	"InterpolatedString",
	"Normal",
})("BraceType")
local Position = class(function(self, line, column)
	self.line = line or 0
	self.column = column or 0
end, {
	class_name = "Position",
	__tostring = function(self)
		return "(line #" .. self.line .. ", column #" .. self.column .. ")"
	end,
	__eq = function(self, other)
		return self.column == other.column and self.line == other.line
	end,
	__lt = function(self, other)
		if self.line == other.line then
			return self.column < other.column
		else
			return self.line < other.line
		end
	end,
	__le = function(self, other)
		return self == other or self < other
	end,

	shift = function(self, start, from, to)
		if self >= start then
			if self.line > from.line then
				self.line = self.line + (to.line - from.line)
			else
				self.line = to.line
				self.column = self.column + (to.column - from.column)
			end
		end
	end,
})

local Location
Location = class(function(self, begin, endd)
	if isInstance(begin, Location) then
		begin = begin.begin
	else
		begin = begin or Position.new()
	end

	if isInstance(endd, Location) then
		endd = endd.endd
	elseif type(endd) == "number" then
		endd = Position.new(begin.line, begin.column + endd)
	else
		endd = endd or Position.new()
	end

	self.begin = begin
	self.endd = endd
end, {
	class_name = "Location",
	__tostring = function(self)
		return "(begin: " .. tostring(self.begin) .. ", end: " .. tostring(self.endd) .. ")"
	end,
	__eq = function(self, other)
		return self.begin == other.begin and self.endd == other.endd
	end,

	checkEncloses = function(self, other)
		return self.begin <= other.begin and self.endd >= other.endd
	end,
	checkOverlaps = function(self, other)
		return (self.begin <= other.begin and self.endd >= other.begin)
			or (self.begin <= other.endd and self.endd >= other.endd)
			or (self.begin >= other.begin and self.endd <= other.endd)
	end,
	contains = function(self, pos)
		return self.begin <= pos and pos < self.endd
	end,

	containsClosed = function(self, pos)
		return self.begin <= pos and pos <= self.endd
	end,
	extend = function(self, other)
		if other.begin < self.begin then
			self.begin = other.begin
		end
		if other.endd > self.endd then
			self.endd = other.endd
		end
	end,
	shift = function(self, start, from, to)
		self.begin.shift(start, from, to)
		self.endd.shift(start, from, to)
	end,
})

local Vector = class(function(self)
	self.size = 0
	self.elements = {}
end, {
	class_name = "Vector",
	__index = function(self, key)
		local oldindex = self._class[key]
		if type(oldindex) ~= "nil" then
			return oldindex
		end

		return self.elements[key]
	end,

	isEmpty = function(self)
		return self.size == 0
	end,

	getBack = function(self)
		return self.elements[self.size]
	end,
	pushBack = function(self, element)
		self.size = self.size + 1
		self.elements[self.size] = element
	end,
	popBack = function(self)
		self.elements[self.size] = nil
		self.size = self.size - 1
	end,

	getFront = function(self)
		return self.elements[1]
	end,
	pushFront = function(self, element)
		self.size = self.size + 1
		table.insert(self.elements, 1, element)
	end,
	popFront = function(self)
		table.remove(self.elements, 1)
		self.size = self.size - 1
	end,

	assign = function(self, count, value)
		for _ = 1, count do
			self:pushBack(value)
		end
	end,

	resize = function(self, new_size)
		local current_size = self.size

		if current_size == new_size then
			return
		elseif current_size > new_size then
			while current_size > new_size do
				self:popBack()
				current_size = self.size
			end
		elseif current_size < new_size then
			self:assign(new_size - current_size, {})
		end
	end,

	begin = function(self)
		return (self.size > 0 and 1) or 0
	end,
	endd = function(self)
		return self.size
	end,
})

local TempVector
TempVector = class(function(self, storage)
	self.storage = storage
	self.offset = storage.size
	self._size = 0
end, {
	class_name = "TempVector",
	__index = function(self, key)
		local oldindex = self._class[key]
		if type(oldindex) ~= "nil" then
			return oldindex
		end

		assert(key < self._size + 1, "k: " .. key .. ", s: " .. self._size .. "\n" .. debug.traceback())
		return self.storage.elements[self.offset + key]
	end,
	isEmpty = function(self)
		return self._size == 0
	end,

	getSize = function(self)
		return self._size
	end,

	getFront = function(self)
		assert(self._size > 0)
		return self.storage.elements[self.offset]
	end,

	getBack = function(self)
		assert(self._size > 0)
		return self.storage:getBack()
	end,
	pushBack = function(self, item)
		-- print(debug.traceback());
		-- assert(self.storage.size == self.offset + self._size, "A: " .. self.storage.size .. " B: " .. self.offset + self._size);
		self.storage:pushBack(item)
		self._size = self._size + 1
	end,

	begin = function(self)
		return self.storage:begin() + self.offset
	end,
	endd = function(self)
		return self.storage:endd() + self.offset + self._size
	end,

	copy = function(self)
		local result = TempVector.new(self.storage)
		result.offset = self.offset
		result._size = self._size

		return result
	end,
})

local AstName = class(function(self, value)
	self.value = value
end, {
	class_name = "AstName",
	__eq = function(self, other)
		return self.value == other.value
	end,
	-- __lt = function(self, other)
	--     return ((self.value and other.value) and self.value == other.value) or self.value < other.value;
	-- end
})
local AstNameEntry = class(function(self, value, token_type)
	self.value = value
	self.token_type = token_type
end, {
	class_name = "AstNameEntry",
	__eq = function(self, other)
		return self.value == other.value
	end,
})
local AstNameTable = class(function(self)
	self.entries = {}
	self.size = 0

	assert(
		#kReserved == TokenType.Reserved_END - TokenType.Reserved_BEGIN,
		"kReserved table does not contain the same number of elements as there are reserved token types"
	)

	for i = TokenType.Reserved_BEGIN.value, TokenType.Reserved_END.value - 1 do
		self:addStatic(kReserved[i - TokenType.Reserved_BEGIN.value + 1], TokenType:fromValue(i))
	end
end, {
	class_name = "AstNameTable",
	getOrAdd = function(self, name)
		return (self:getOrAddWithType(name))
	end,
	getOrAddWithType = function(self, name)
		local key = AstNameEntry.new(AstName.new(name), TokenType.Eof)
		local entry = self:insert(key)

		if entry.token_type ~= TokenType.Eof then
			return entry.value, entry.token_type
		end

		entry.value = AstName.new(name)
		entry.token_type = TokenType.Name

		return entry.value, entry.token_type
	end,

	addStatic = function(self, name, token_type)
		local entry = AstNameEntry.new(AstName.new(name), token_type or TokenType.Name)

		assert(not self:find(entry))
		self:insert(entry)

		return entry.value
	end,
	insert = function(self, key)
		local entry = self:find(key)
		if entry then
			return entry
		end

		self.size = self.size + 1
		self.entries[self.size] = key

		return key
	end,
	find = function(self, query)
		for _, entry in next, self.entries do
			if entry == query then
				return entry
			end
		end
        return
	end,
})

local AstType
local AstStat
local AstStatBlock
local AstExpression
local AstTypePack

local function visitTypeList(visitor, list)
	for ttype in list.types do
		ttype:visit(visitor)
	end

	if list.tail_type then
		list.tail_type:visit(visitor)
	end
end

local AstLocal =
	simpleclass({ name = "AstLocal", "name", "location", "shadow", "function_depth", "loop_depth", "annotation" })

local AstArray = class(function(self, data, size)
	self.data = data
	self.size = size or #data
end, {
	class_name = "AstArray",
	getBegin = function(self)
		return self.data:sub(1, 1)
	end,
	getEnd = function(self)
		return self.data:sub(self.size, self.size)
	end,
})

local AstTypeList = simpleclass({ name = "AstTypeList", "types", "tail_type" })
local AstArgumentName = simpleclass({ name = "AstArgumentName", "name", "location" })
local AstGenericType = simpleclass({ name = "AstGenericType", "name", "location", "default_value" })
local AstGenericTypePack = simpleclass({ name = "AstGenericTypePack", "name", "location", "default_value" })

local function ast_node_init(self, class_index, location)
	self.class_index = class_index
	self.location = location
end

local AstNode = class(ast_node_init, {
	class_name = "AstNode",
	is = function(self, query_type)
		return isInstance(self, query_type)
	end,
	as = function(self, new_type)
		return (self.class_index == new_type.class_index and new_type.new(self.class_index, self.location)) or nil
	end,
})

AstExpression = class(ast_node_init, {
	class_name = "AstExpression",
	asExpression = function(self)
		return self
	end,
}, AstNode)

AstStat = class(function(self, ...)
	ast_node_init(self, ...)
	self.has_semicolon = false
end, {
	class_name = "AstStat",
	asStat = function(self)
		return self
	end,
}, AstNode)

local AstExpressionGroup = class(function(self, location, expression)
	ast_node_init(self, 0, location)
	self.expression = expression
end, {
	class_index = 0,
	class_name = "AstExpressionGroup",
	visit = function(self, visitor)
		if visitor:visit(self) then
			self.expression:visit(visitor)
		end
	end,
}, AstExpression)

local function visitBase(self, visitor)
	visitor:visit(self)
end

local AstExpressionConstantNil = class(function(self, location)
	ast_node_init(self, 1, location)
end, {
	class_index = 1,
	class_name = "AstExpressionConstantNil",
	visit = visitBase,
}, AstExpression)

local AstExpressionConstantBool = class(function(self, location, value)
	ast_node_init(self, 2, location)
	self.value = value
end, {
	class_index = 2,
	class_name = "AstExpressionConstantBool",
	visit = visitBase,
}, AstExpression)

local ConstantNumberParseResult = enum({
	"Ok",
	"Malformed",
})("ConstantNumberParseResult")

-- credits to https://www.rapidtables.com/convert/number/binary-to-decimal.html
local function binaryToInt(a)
	if a:find("[^01]") then
		return ConstantNumberParseResult.Maliformed, a
	end

	local result = 0

	for i = #a - 1, 0, -1 do
		local b = tonumber(a:sub(#a - i, #a - i))
		result = result + (b * 2 ^ i)
	end

	return ConstantNumberParseResult.Ok, result
end

local AstExpressionConstantNumber = class(function(self, location, value, parse_result)
	if not parse_result then
		parse_result = ConstantNumberParseResult.Ok
	end
	ast_node_init(self, 3, location)
	self.value = value
	self.parse_result = parse_result
end, {
	class_index = 3,
	class_name = "AstExpressionConstantNumber",
	visit = visitBase,
}, AstExpression)

local AstExpressionConstantString = class(function(self, location, value)
	ast_node_init(self, 4, location)
	self.value = value
end, {
	class_index = 4,
	class_name = "AstExpressionConstantString",
	visit = visitBase,
}, AstExpression)

local AstExpressionLocal = class(function(self, location, llocal, is_upvalue)
	ast_node_init(self, 5, location)
	self.llocal = llocal
	self.is_upvalue = is_upvalue
end, {
	class_index = 5,
	class_name = "AstExpressionLocal",
	visit = visitBase,
}, AstExpression)

local AstExpressionGlobal = class(function(self, location, name)
	ast_node_init(self, 6, location)
	self.name = name
end, {
	class_index = 6,
	class_name = "AstExpressionGlobal",
	visit = visitBase,
}, AstExpression)

local AstExpressionVarargs = class(function(self, location)
	ast_node_init(self, 7, location)
end, {
	class_index = 7,
	class_name = "AstExpressionVarargs",
	visit = visitBase,
}, AstExpression)

local AstExpressionCall = class(function(self, location, func, args, has_self, arg_location)
	ast_node_init(self, 8, location)
	self.func = func
	self.args = args
	self.has_self = has_self
	self.arg_location = arg_location
end, {
	class_index = 8,
	class_name = "AstExpressionCall",
	visit = function(self, visitor)
		if visitor:visit(self) then
			self.func:visit(visitor)

			local args = self.args
			if isInstance(args, Vector) then
				for _, arg in next, self.args.elements do
					arg:visit(visitor)
				end
			else
				args:visit(visitor)
			end
		end
	end,
}, AstExpression)

local AstExpressionIndexName = class(function(self, location, expression, index, index_location, op_position, op)
	ast_node_init(self, 9, location)
	self.expression = expression
	self.index = index
	self.index_location = index_location
	self.op_position = op_position
	self.op = op or "."
end, {
	class_index = 9,
	class_name = "AstExpressionIndexName",
	visit = function(self, visitor)
		if visitor:visit(self) then
			self.expression:visit(visitor)
		end
	end,
}, AstExpression)

local AstExpressionIndexExpression = class(function(self, location, expression, index)
	ast_node_init(self, 10, location)
	self.expression = expression
	self.index = index
end, {
	class_index = 10,
	class_name = "AstExpressionIndexExpression",
	visit = function(self, visitor)
		if visitor:visit(self) then
			self.expression:visit(visitor)
			self.index:visit(visitor)
		end
	end,
}, AstExpression)

local AstExpressionFunction = class(
	function(
		self,
		location,
		generics,
		generic_packs,
		has_self,
		args,
		has_var_arg,
		var_arg_location,
		body,
		function_depth,
		debug_name,
		return_annotation,
		var_arg_annotation,
		has_end,
		arg_location
	)
		ast_node_init(self, 11, location)
		self.generics = generics
		self.generic_packs = generic_packs
		self.has_self = has_self
		self.args = args
		self.return_annotation = return_annotation
		self.has_var_arg = has_var_arg
		self.var_arg_location = var_arg_location
		self.var_arg_annotation = var_arg_annotation
		self.body = body
		self.function_depth = function_depth
		self.debug_name = debug_name
		self.has_end = has_end
		self.arg_location = arg_location
	end,
	{
		class_index = 11,
		class_name = "AstExpressionFunction",
		visit = function(self, visitor)
			if visitor:visit(self) then
				for _, arg in next, self.args.elements do
					if arg.annotation then
						arg.annotation:visit(visitor)
					end
				end

				if self.var_arg_annotation then
					self.var_arg_annotation:visit(visitor)
				end

				if self.return_annotation then
					self.return_annotation:visit(visitor)
				end

				self.body:visit(visitor)
			end
		end,
	},
	AstExpression
)

local AstExpressionTable = class(function(self, location, items)
	ast_node_init(self, 12, location)
	self.items = items
end, {
	class_index = 12,
	class_name = "AstExpressionTable",
	Item = class(function(self, kind, key, value)
		self.kind = kind
		self.key = key
		self.value = value
	end, {
		class_name = "AstExpressionTableItem",
		enum = enum({
			"List", -- foo, in which case key is a nullptr
			"Record", -- foo=bar, in which case key is a AstExprConstantString
			"General", -- [foo]=bar
		})("AstExpressionTableItemKind"),
	}),
	visit = function(self, visitor)
		if visitor:visit(self) then
			for _, item in next, self.items.elements do
				if item.key then
					item.key:visit(visitor)
				end

				item.value:visit(visitor)
			end
		end
	end,
}, AstExpression)

local unary_tostring_functions = {}
do
	unary_tostring_functions.Minus = function()
		return "-"
	end
	unary_tostring_functions.Not = function()
		return "not"
	end
	unary_tostring_functions.Len = function()
		return "#"
	end
end
local AstExpressionUnary = class(function(self, location, op, expression)
	ast_node_init(self, 13, location)
	self.op = op
	self.expression = expression
	self.toString = unary_tostring_functions[op.name]
end, {
	class_index = 13,
	class_name = "AstExpressionUnary",
	enum = enum({
		"Not",
		"Minus",
		"Len",
	})("AstExpressionUnaryOp"),

	visit = function(self, visitor)
		if visitor:visit(self) then
			self.expression:visit(visitor)
		end
	end,
}, AstExpression)

local AstExpressionBinary = class(function(self, location, op, left, right)
	ast_node_init(self, 14, location)
	self.op = op
	self.left = left
	self.right = right
end, {
	class_index = 14,
	class_name = "AstExpressionBinary",
	enum = enum({
		"Add",
		"Sub",
		"Mul",
		"Div",
		"Mod",
		"Pow",
		"Concat",
		"CompareNe",
		"CompareEq",
		"CompareLt",
		"CompareLe",
		"CompareGt",
		"CompareGe",
		"And",
		"Or",
	})("AstExpressionBinaryOp"),

	visit = function(self, visitor)
		if visitor:visit(self) then
			self.left:visit(visitor)
			self.right:visit(visitor)
		end
	end,
}, AstExpression)

local AstExpressionTypeAssertion = class(function(self, location, expression, annotation)
	ast_node_init(self, 15, location)
	self.expression = expression
	self.annotation = annotation
end, {
	class_index = 15,
	class_name = "AstExpressionTypeAssertion",
	visit = function(self, visitor)
		if visitor:visit(self) then
			self.expression:visit(visitor)
			self.annotation:visit(visitor)
		end
	end,
}, AstExpression)

local AstExpressionIfElse = class(
	function(self, location, condition, has_then, true_expression, has_else, false_expression)
		ast_node_init(self, 16, location)
		self.condition = condition
		self.has_then = has_then
		self.true_expression = true_expression
		self.has_else = has_else
		self.false_expression = false_expression
	end,
	{
		class_index = 16,
		class_name = "AstExpressionIfElse",
		visit = function(self, visitor)
			if visitor:visit(self) then
				self.condition:visit(visitor)
				self.true_expression:visit(visitor)
				self.false_expression:visit(visitor)
			end
		end,
	},
	AstExpression
)

local AstExpressionInterpolatedString = class(function(self, location, strings, expressions)
	ast_node_init(self, 17, location)
	self.strings = strings
	self.expressions = expressions
end, {
	class_index = 17,
	class_name = "AstExpressionInterpolatedString",
	visit = function(self, visitor)
		if visitor:visit(self) then
			for _, expression in next, self.expressions.elements do
				expression:visit(visitor)
			end
		end
	end,
}, AstExpression)

AstStatBlock = class(function(self, location, body)
	ast_node_init(self, 18, location)
	self.body = body
end, {
	class_index = 18,
	class_name = "AstStatBlock",
	visit = function(self, visitor)
		if visitor:visit(self) then
			for _, stat in next, self.body.elements do
				stat:visit(visitor)
			end
		end
	end,
}, AstStat)

local AstStatIf = class(function(self, location, condition, then_body, else_body, then_location, else_location, has_end)
	ast_node_init(self, 19, location)
	self.condition = condition
	self.then_body = then_body
	self.else_body = else_body

	self.then_location = then_location

	self.else_location = else_location

	self.has_end = has_end
end, {
	class_index = 19,
	class_name = "AstStatIf",
	visit = function(self, visitor)
		if visitor:visit(self) then
			self.condition:visit(visitor)
			self.then_body:visit(visitor)

			if self.else_body then
				self.else_body:visit(visitor)
			end
		end
	end,
}, AstStat)

local AstStatWhile = class(function(self, location, condition, body, has_do, do_location, has_end)
	ast_node_init(self, 20, location)
	self.condition = condition
	self.body = body

	self.has_do = has_do
	self.do_location = do_location

	self.has_end = has_end
end, {
	class_index = 20,
	class_name = "AstStatWhile",
	visit = function(self, visitor)
		if visitor:visit(self) then
			self.condition:visit(visitor)
			self.body:visit(visitor)
		end
	end,
}, AstStat)

local AstStatRepeat = class(function(self, location, condition, body, has_until)
	ast_node_init(self, 21, location)
	self.condition = condition
	self.body = body

	self.has_until = has_until
end, {
	class_index = 21,
	class_name = "AstStatRepeat",
	visit = function(self, visitor)
		if visitor:visit(self) then
			self.body:visit(visitor)
			self.condition:visit(visitor)
		end
	end,
}, AstStat)

local AstStatBreak = class(function(self, location)
	ast_node_init(self, 22, location)
end, {
	class_index = 22,
	class_name = "AstStatBreak",
	visit = visitBase,
}, AstStat)

local AstStatContinue = class(function(self, location)
	ast_node_init(self, 23, location)
end, {
	class_index = 23,
	class_name = "AstStatContinue",
	visit = visitBase,
}, AstStat)

local AstStatReturn = class(function(self, location, list)
	ast_node_init(self, 24, location)
	self.list = list
end, {
	class_index = 24,
	class_name = "AstStatReturn",
	visit = function(self, visitor)
		if visitor:visit(self) then
			for _, expression in next, self.list.elements do
				expression:visit(visitor)
			end
		end
	end,
}, AstStat)

local AstStatExpression = class(function(self, location, expression)
	ast_node_init(self, 25, location)
	self.expression = expression
end, {
	class_index = 25,
	class_name = "AstStatExpression",
	visit = function(self, visitor)
		if visitor:visit(self) then
			self.expression:visit(visitor)
		end
	end,
}, AstStat)

local AstStatLocal = class(function(self, location, vars, values, equals_sign_location)
	ast_node_init(self, 26, location)
	self.vars = vars
	self.values = values
	self.equals_sign_location = equals_sign_location
end, {
	class_index = 26,
	class_name = "AstStatLocal",
	visit = function(self, visitor)
		if visitor:visit(self) then
			for _, var in next, self.vars.elements do
				if var.annotation then
					var.annotation:visit(visitor)
				end
			end

			for _, expression in next, self.values.elements do
				expression:visit(visitor)
			end
		end
	end,
}, AstStat)

local AstStatFor = class(function(self, location, var, from, to, step, body, has_do, do_location, has_end)
	ast_node_init(self, 27, location)
	self.var = var
	self.from = from
	self.to = to
	self.step = step
	self.body = body

	self.has_do = has_do
	self.do_location = do_location

	self.has_end = has_end
end, {
	class_index = 27,
	class_name = "AstStatFor",
	visit = function(self, visitor)
		if visitor:visit(self) then
			if self.var.annotation then
				self.var.annotation:visit(visitor)
			end

			self.from:visit(visitor)
			self.to:visit(visitor)

			if self.step then
				self.step:visit(visitor)
			end

			self.body:visit(visitor)
		end
	end,
}, AstStat)

local AstStatForIn = class(
	function(self, location, vars, values, body, has_in, in_location, has_do, do_location, has_end)
		ast_node_init(self, 28, location)
		self.vars = vars
		self.values = values
		self.body = body

		self.has_in = has_in
		self.in_location = in_location

		self.has_do = has_do
		self.do_location = do_location

		self.has_end = has_end
	end,
	{
		class_index = 28,
		class_name = "AstStatForIn",
		visit = function(self, visitor)
			if visitor:visit(self) then
				for _, var in next, self.vars.elements do
					if var.annotation then
						var.annotation:visit(visitor)
					end
				end

				for _, expression in next, self.values.elements do
					expression:visit(visitor)
				end

				self.body:visit(visitor)
			end
		end,
	},
	AstStat
)

local AstStatAssign = class(function(self, location, vars, values)
	ast_node_init(self, 29, location)
	self.vars = vars
	self.values = values
end, {
	class_index = 29,
	class_name = "AstStatAssign",
	visit = function(self, visitor)
		if visitor:visit(self) then
			for _, lvalue in next, self.vars.elements do
				lvalue:visit(visitor)
			end

			for _, expression in next, self.values.elements do
				expression:visit(visitor)
			end
		end
	end,
}, AstStat)

local AstStatCompoundAssign = class(function(self, location, op, var, value)
	ast_node_init(self, 30, location)
	self.op = op
	self.var = var
	self.value = value
end, {
	class_index = 30,
	class_name = "AstStatCompoundAssign",
	visit = function(self, visitor)
		if visitor:visit(self) then
			self.var:visit(visitor)
			self.value:visit(visitor)
		end
	end,
}, AstStat)

local AstStatFunction = class(function(self, location, name, func)
	ast_node_init(self, 31, location)
	self.name = name
	self.func = func
end, {
	class_index = 31,
	class_name = "AstStatFunction",
	visit = function(self, visitor)
		if visitor:visit(self) then
			self.name:visit(visitor)
			self.func:visit(visitor)
		end
	end,
}, AstStat)

local AstStatLocalFunction = class(function(self, location, name, func)
	ast_node_init(self, 32, location)
	self.name = name
	self.func = func
end, {
	class_index = 32,
	class_name = "AstStatLocalFunction",
	visit = function(self, visitor)
		if visitor:visit(self) then
			self.func:visit(visitor)
		end
	end,
}, AstStat)

local AstStatTypeAlias = class(function(self, location, name, name_location, generics, generic_packs, ttype, exported)
	ast_node_init(self, 33, location)
	self.name = name
	self.name_location = name_location
	self.generics = generics
	self.generic_packs = generic_packs
	self.ttype = ttype
	self.exported = exported
end, {
	class_index = 33,
	class_name = "AstStatTypeAlias",
	visit = function(self, visitor)
		if visitor:visit(self) then
			for _, el in next, self.generics.elements do
				if el.default_value then
					el.default_value:visit(visitor)
				end
			end

			for _, el in next, self.generic_packs.elements do
				if el.default_value then
					el.default_value:visit(visitor)
				end
			end

			self.ttype:visit(visitor)
		end
	end,
}, AstStat)

local AstStatDeclareGlobal = class(function(self, location, name, ttype)
	ast_node_init(self, 34, location)
	self.name = name
	self.ttype = ttype
end, {
	class_index = 34,
	class_name = "AstStatDeclareGlobal",
	visit = function(self, visitor)
		if visitor:visit(self) then
			self.ttype:visit(visitor)
		end
	end,
}, AstStat)

local AstStatDeclareFunction = class(
	function(self, location, name, generics, generic_packs, params, param_names, return_types)
		ast_node_init(self, 35, location)
		self.name = name
		self.generics = generics
		self.generic_packs = generic_packs
		self.params = params
		self.param_names = param_names
		self.return_types = return_types
	end,
	{
		class_index = 35,
		class_name = "AstStatDeclareFunction",
		visit = function(self, visitor)
			if visitor:visit(self) then
				visitTypeList(visitor, self.params)
				visitTypeList(visitor, self.return_types)
			end
		end,
	},
	AstStat
)

local AstDeclaredClassProp = simpleclass({ name = "AstDeclaredClassProp", "name", "ttype", "is_method" })

local AstTableIndexer = simpleclass({ name = "AstTableIndexer", "index_type", "result_type", "location" })

local AstStatDeclareClass = class(function(self, location, name, super_name, props, indexer)
	ast_node_init(self, 36, location)
	self.name = name
	self.super_name = super_name

	self.props = props
	self.indexer = indexer
end, {
	class_index = 36,
	class_name = "AstStatDeclareClass",
	visit = function(self, visitor)
		if visitor:visit(self) then
			for _, prop in next, self.props.elements do
				prop.ttype:visit(visitor)
			end
		end
	end,
}, AstStat)

AstType = class(ast_node_init, {
	class_name = "AstType",
	asType = function(self)
		return self
	end,
}, AstNode)

local AstTypeOrPack = simpleclass({ name = "AstTypeOrPack", "ttype", "type_pack" })

local AstTypeReference = class(
	function(self, location, prefix, name, prefix_location, name_location, has_parameter_list, parameters)
		ast_node_init(self, 37, location)
		self.has_parameter_list = has_parameter_list
		self.prefix = prefix
		self.prefix_location = prefix_location
		self.name = name
		self.name_location = name_location
		self.parameters = parameters
	end,
	{
		class_index = 37,
		class_name = "AstTypeReference",
		visit = function(self, visitor)
			if visitor:visit(self) then
				for _, param in next, self.parameters.elements do
					if param.ttype then
						param.ttype:visit(visitor)
					else
						param.type_pack:visit(visitor)
					end
				end
			end
		end,
	},
	AstType
)

local AstTableProperty = simpleclass({ name = "AstTableProperty", "name", "location", "ttype" })

local AstTypeTable = class(function(self, location, props, indexer)
	ast_node_init(self, 38, location)
	self.props = props
	self.indexer = indexer
end, {
	class_index = 38,
	class_name = "AstTypeTable",
	visit = function(self, visitor)
		if visitor:visit(self) then
			for _, prop in next, self.props.elements do
				prop.ttype:visit(visitor)
			end

			if self.indexer then
				self.indexer.index_type:visit(visitor)
				self.indexer.result_type:visit(visitor)
			end
		end
	end,
}, AstType)

local AstTypeFunction = class(function(self, location, generics, generic_packs, arg_types, arg_names, return_types)
	ast_node_init(self, 39, location)
	self.generics = generics
	self.generic_packs = generic_packs
	self.arg_types = arg_types
	self.arg_names = arg_names
	self.return_types = return_types
end, {
	class_index = 39,
	class_name = "AstTypeFunction",
	visit = function(self, visitor)
		if visitor:visit(self) then
			visitTypeList(visitor, self.arg_types)
			visitTypeList(visitor, self.return_types)
		end
	end,
}, AstType)

local AstTypeTypeof = class(function(self, location, expression)
	ast_node_init(self, 40, location)
	self.expression = expression
end, {
	class_index = 40,
	class_name = "AstTypeTypeof",
	visit = function(self, visitor)
		if visitor:visit(self) then
			self.expression:visit(visitor)
		end
	end,
}, AstType)

local AstTypeUnion = class(function(self, location, types)
	ast_node_init(self, 41, location)
	self.types = types
end, {
	class_index = 41,
	class_name = "AstTypeUnion",
	visit = function(self, visitor)
		if visitor:visit(self) then
			for _, ttype in next, self.ttypes do
				ttype:visit(visitor)
			end
		end
	end,
}, AstType)

local AstTypeIntersection = class(function(self, location, types)
	ast_node_init(self, 42, location)
	self.types = types
end, {
	class_index = 42,
	class_name = "AstTypeIntersection",
	visit = function(self, visitor)
		if visitor:visit(self) then
			for _, ttype in next, self.ttypes do
				ttype:visit(visitor)
			end
		end
	end,
}, AstType)

local AstExpressionError = class(function(self, location, expressions, message_index)
	ast_node_init(self, 43, location)
	self.expressions = expressions
	self.message_index = message_index
end, {
	class_index = 43,
	class_name = "AstExpressionError",
	visit = function(self, visitor)
		if visitor:visit(self) then
			for _, expression in next, self.expressions.elements do
				expression:visit(visitor)
			end
		end
	end,
}, AstExpression)

local AstStatError = class(function(self, location, expressions, statements, message_index)
	ast_node_init(self, 44, location)
	self.expressions = expressions
	self.statements = statements
	self.message_index = message_index
end, {
	class_index = 44,
	class_name = "AstStatError",
	visit = function(self, visitor)
		if visitor:visit(self) then
			for _, expression in next, self.expressions.elements do
				expression:visit(visitor)
			end

			for _, statement in next, self.statements.elements do
				statement:visit(visitor)
			end
		end
	end,
}, AstStat)

local AstTypeError = class(function(self, location, types, is_missing, message_index)
	ast_node_init(self, 45, location)
	self.types = types
	self.is_missing = is_missing
	self.message_index = message_index
end, {
	class_index = 45,
	class_name = "AstTypeError",
	visit = function(self, visitor)
		if visitor:visit(self) then
			for _, ttype in next, self.ttypes do
				ttype:visit(visitor)
			end
		end
	end,
}, AstType)

local AstTypeSingletonBool = class(function(self, location, value)
	ast_node_init(self, 46, location)
	self.value = value
end, {
	class_index = 46,
	class_name = "AstTypeSingletonBool",
	visit = visitBase,
}, AstType)

local AstTypeSingletonString = class(function(self, location, value)
	ast_node_init(self, 47, location)
	self.value = value
end, {
	class_index = 47,
	class_name = "AstTypeSingletonString",
	visit = visitBase,
}, AstType)

AstTypePack = class(ast_node_init, { class_name = "AstTypePack" }, AstNode)

local AstTypePackExplicit = class(function(self, location, type_list)
	ast_node_init(self, 48, location)
	self.type_list = type_list
end, {
	class_index = 48,
	class_name = "AstTypePackExplicit",
	visit = function(self, visitor)
		if visitor:visit(self) then
			for _, ttype in next, self.ttypes do
				ttype:visit(visitor)
			end

			if self.type_list.tail_type then
				self.type_list.tail_type:visit(visitor)
			end
		end
	end,
}, AstTypePack)

local AstTypePackVariadic = class(function(self, location, variadic_type)
	ast_node_init(self, 49, location)
	self.variadic_type = variadic_type
end, {
	class_index = 49,
	class_name = "AstTypePackVariadic",
	visit = function(self, visitor)
		if visitor:visit(self) then
			self.variadic_type:visit(visitor)
		end
	end,
}, AstTypePack)

local AstTypePackGeneric = class(function(self, location, name)
	ast_node_init(self, 50, location)
	self.generic_name = name
end, {
	class_index = 50,
	class_name = "AstTypePackGeneric",
	visit = visitBase,
}, AstTypePack)

local function getIdentifier(node)
	local expression = node:as(AstExpressionGlobal)
	if expression then
		return expression.name
	end

	expression = node:as(AstExpressionLocal)
	if expression then
		return expression.llocal.name
	end

	return AstName.new()
end

local function getLocation(type_list)
	local result
	if type_list.types.size > 0 then
		result =
			Location.new(type_list.types.elements[1].location, type_list.types.elements[type_list.types.size].location)
	else
		result = Location.new()
	end

	if type_list.tail_type then
		result.endd = type_list.tail_type.location.endd
	end

	return result
end

local Local = simpleclass({ name = "Local", "llocal", "offset" })
local Name = simpleclass({ name = "Name", "name", "location" })
local Binding = simpleclass({ name = "Binding", "name", "annotation" })
local HotComment = simpleclass({ name = "HotComment", "header", "location", "content" })
local Comment = simpleclass({ name = "Comment", "ttype", "location" })

local function classFast(name, t)
	return {
		class_name = name,
		set = function(self, key, value)
			if type(value) ~= t then
				error(
					"class '" .. name .. "' only allows for the type '" .. t .. "', not '" .. type(value) .. "'",
					2
				)
			end
			self[key] = value
		end,
	}
end
local FFlag = classFast("FFlag", "boolean")
local FInt = classFast("FInt", "number")

local ParseOptions = simpleclass({ "allow_declaration_syntax", "capture_comments" })

return {
	binaryToInt = binaryToInt,
	isInstance = isInstance,
	isSpace = isSpace,
	class = class,
	simpleclass = simpleclass,
	enum = enum,
	newBuffer = newBuffer,
	format = format,

	TokenType = TokenType,
	kReserved = kReserved,
	isReserved = isReserved,
	Token = Token,
	BraceType = BraceType,
	Position = Position,
	Location = Location,
	Vector = Vector,
	TempVector = TempVector,
	AstName = AstName,
	AstNameEntry = AstNameEntry,
	AstNameTable = AstNameTable,

	AstLocal = AstLocal,
	AstArray = AstArray,
	AstTypeList = AstTypeList,
	AstArgumentName = AstArgumentName,
	AstGenericType = AstGenericType,
	AstGenericTypePack = AstGenericTypePack,
	AstNode = AstNode,
	AstExpression = AstExpression,
	AstStat = AstStat,
	AstExpressionGroup = AstExpressionGroup,
	AstExpressionConstantNil = AstExpressionConstantNil,
	AstExpressionConstantBool = AstExpressionConstantBool,
	ConstantNumberParseResult = ConstantNumberParseResult,
	AstExpressionConstantNumber = AstExpressionConstantNumber,
	AstExpressionConstantString = AstExpressionConstantString,
	AstExpressionLocal = AstExpressionLocal,
	AstExpressionGlobal = AstExpressionGlobal,
	AstExpressionVarargs = AstExpressionVarargs,
	AstExpressionCall = AstExpressionCall,
	AstExpressionIndexName = AstExpressionIndexName,
	AstExpressionIndexExpression = AstExpressionIndexExpression,
	AstExpressionFunction = AstExpressionFunction,
	AstExpressionTable = AstExpressionTable,
	AstExpressionUnary = AstExpressionUnary,
	AstExpressionBinary = AstExpressionBinary,
	AstExpressionTypeAssertion = AstExpressionTypeAssertion,
	AstExpressionIfElse = AstExpressionIfElse,
	AstExpressionInterpolatedString = AstExpressionInterpolatedString,
	AstStatBlock = AstStatBlock,
	AstStatIf = AstStatIf,
	AstStatWhile = AstStatWhile,
	AstStatRepeat = AstStatRepeat,
	AstStatBreak = AstStatBreak,
	AstStatContinue = AstStatContinue,
	AstStatReturn = AstStatReturn,
	AstStatExpression = AstStatExpression,
	AstStatLocal = AstStatLocal,
	AstStatFor = AstStatFor,
	AstStatForIn = AstStatForIn,
	AstStatAssign = AstStatAssign,
	AstStatCompoundAssign = AstStatCompoundAssign,
	AstStatFunction = AstStatFunction,
	AstStatLocalFunction = AstStatLocalFunction,
	AstStatTypeAlias = AstStatTypeAlias,
	AstStatDeclareGlobal = AstStatDeclareGlobal,
	AstStatDeclareFunction = AstStatDeclareFunction,
	AstDeclaredClassProp = AstDeclaredClassProp,
	AstTableIndexer = AstTableIndexer,
	AstStatDeclareClass = AstStatDeclareClass,
	AstType = AstType,
	AstTypeOrPack = AstTypeOrPack,
	AstTypeReference = AstTypeReference,
	AstTableProperty = AstTableProperty,
	AstTypeTable = AstTypeTable,
	AstTypeFunction = AstTypeFunction,
	AstTypeTypeof = AstTypeTypeof,
	AstTypeUnion = AstTypeUnion,
	AstTypeIntersection = AstTypeIntersection,
	AstExpressionError = AstExpressionError,
	AstStatError = AstStatError,
	AstTypeError = AstTypeError,
	AstTypeSingletonBool = AstTypeSingletonBool,
	AstTypeSingletonString = AstTypeSingletonString,
	AstTypePackExplicit = AstTypePackExplicit,
	AstTypePackVariadic = AstTypePackVariadic,
	AstTypePackGeneric = AstTypePackGeneric,

	Local = Local,
	Name = Name,
	Binding = Binding,
	HotComment = HotComment,
	Comment = Comment,

	FFlag = FFlag,
	FInt = FInt,

	getIdentifier = getIdentifier,
	getLocation = getLocation,

	ParseOptions = ParseOptions,
}

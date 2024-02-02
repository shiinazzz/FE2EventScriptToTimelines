-- Convert from EventScript to Timelines with custom Luau parser.
-- cutymeo (ari)
-- Additional help: LacticMilk (yay :troll:)

local Lexer = require(script.LuauAstBuilder.Lexer)
local Parser = require(script.LuauAstBuilder.Parser)
local SyntaxNode = require(script.LuauAstBuilder.SyntaxNode)
local Visitor = require(script.Visitor)

local Converter = {}

local SpecialIdentifier = {
	FE2Lib = ".FE2Lib",
	TweenService = ".TweenService"
}

function Converter.ReadScript(scriptSource: string)
	-- Create new Lexer instance to lex tokens from source
	local newLexerInstance = Lexer.new(scriptSource)
	local lexedTokens = newLexerInstance:Scan()

	-- After we got the tokens, we parse the tokens into an abstract syntax tree
	-- This syntax tree can be used with GetTimelines to analyze the source
	-- And then be able to output Timelines.
	local newParserInstance = Parser.new(scriptSource, lexedTokens)
	return newParserInstance:Parse()
end

function Converter.GetTimelines(MapModel: Model, Tree: SyntaxNode.SyntaxNode)
	-- Note: We do not process interpolated strings, functions that process things, etc.
	-- I will refer to these as "dynamic" objects.
	-- The Converter will only be able to convert "static" objects, which are things that are guaranteed to be there
	-- That's why "dynamic" will be ignored, as we do not know what it will do or return.
	-- We can try to keep this simple, and the user will have to figure the rest to convert.
	
	local startTime = tick()
	
	-- [[ UTILITIES ]]
	local function getInstanceFromStrings(strings: string | {string}): (Instance?, string?)
		local success, resultInstance = pcall(function()
			local resolvedInstance = nil
			if type(strings) == "table" then
				local startIndex = -1
				if strings[1] == SpecialIdentifier.FE2Lib and strings[2] == "Map" then
					resolvedInstance = MapModel
					startIndex = 3
				elseif strings[1] == "script" then
					resolvedInstance = MapModel.EventScript -- This will have to exist, because how else are you going to get the source from parsing LOL
					startIndex = 2
				end

				if startIndex ~= -1 then
					if startIndex <= #strings then
						for i = startIndex, #strings do
							resolvedInstance = resolvedInstance[strings[i]]
						end
					end

					return resolvedInstance
				else
					error("Unknown indexing.")
				end
			elseif type(strings) == "string" then
				if strings == "script" then
					return MapModel.EventScript
				else
					error("Unknown global.")
				end
			else
				error(`Unknown type: {type(strings)}`)
			end
		end)

		return (success and resultInstance), (not success and resultInstance)
	end

	-- [[ TIMELINES HANDLER ]]
	local timelineScope = {}
	timelineScope.__index = timelineScope
	
	type xframe = {
		name: string,
		object: Instance?,

		properties: {
			general: {[string]: any},
			xframe: {[string]: any},
			property: {[string]: any},
			tween: {[string]: any}
		}
	}

	function timelineScope.new(name: string, properties: {[string]: any}?)
		return setmetatable({
			name = name,
			properties = properties or {} :: {[string]: any},
			xframes = {} :: {xframe}
		}, timelineScope)
	end
	type timelineScope = typeof(timelineScope.new(""))

	function timelineScope:newXFrame(
		name: string,
		object: Instance?,
		generalProps: {[string]: any}?,
		xframeProps: {[string]: any}?,
		objectProps: {[string]: any}?,
		tweenProps: {[string]: any}?
	)
		local xframe = {
			name = name,
			object = object,

			properties = {
				general = generalProps or {},
				xframe = xframeProps or {},
				property = objectProps or {},
				tween = tweenProps or {}
			}
		}

		table.insert((self :: timelineScope).xframes, xframe)
		return xframe
	end

	function timelineScope:makeInstance(): Configuration
		local root = Instance.new("Configuration")
		root.Name = self.name
		
		for property, value in self.properties do
			root:SetAttribute(property, value)
		end
		
		for _, xframe in self.xframes do
			local xframeObj = Instance.new("ObjectValue")
			xframeObj.Name = xframe.name
			xframeObj.Value = xframe.object
			
			for property, value in xframe.properties.general do
				xframeObj:SetAttribute(property, value)
			end
			for property, value in xframe.properties.xframe do
				xframeObj:SetAttribute("XFrame_" .. property, value)
			end
			for property, value in xframe.properties.property do
				xframeObj:SetAttribute("Property_" .. property, value)
			end
			for property, value in xframe.properties.tween do
				xframeObj:SetAttribute("Tween_" .. property, value)
			end
			
			xframeObj.Parent = root
		end
		
		return root
	end

	-- [[ VISITOR HANDLER ]]
	-- Each "do end" creates a new scope
	-- Each scope should return some data that can be used to analyze
	-- (Such as FunctionCall, LocalReassignment, etc.)
	
	local visitor = Visitor.createFromTree(Tree)
	local scopesRunTime: {[number]: number} = {}
	local scopesTimeline: {[number]: {timelineScope}} = {}
	
	local function getScopeTimeline(scopeId: number?): timelineScope
		return scopesTimeline[scopeId or visitor:currentScopeId()][1]
	end
	
	-- [[ Scope related functions ]] --
	local function scopeWait(time: number?)
		if type(time) ~= "number" then
			return
		end
		
		local scopeId = visitor:currentScopeId()
		if not scopesRunTime[scopeId] then
			scopesRunTime[scopeId] = 0
		end
		scopesRunTime[scopeId] += time
	end

	local function getScopeTotalRunTime()
		local scopeId = visitor:currentScopeId()
		return scopesRunTime[scopeId] or 0
	end

	-- [[ AST node related functions ]] --
	function Visitor:onNewScope(scopeId: number)
		scopesTimeline[scopeId] = {timelineScope.new(scopeId == 2 and "Flood_Timeline" or "Timeline_Scope" .. scopeId)}
	end
	
	function Visitor:visitFunctionCall(identifier: Visitor.Identifier | Visitor.IndexIdentifier, arguments: {Visitor.Identifier | Visitor.IndexIdentifier | Visitor.FunctionDefinitionData}, isSelfCall: boolean)
		if isSelfCall then
			table.insert(arguments, 1, table.move(identifier, 1, #identifier - 1, 1))
		end

		-- Seperate scope for handling timelines
		if type(identifier) == "table" then
			local GetMapValsCall = false
			local GetMapValsIdentifiers = {{"game", "Workspace", "Multiplayer", "GetMapVals", "Invoke"}, {"workspace", "Multiplayer", "GetMapVals", "Invoke"}}
			for _, findIdentifiers in GetMapValsIdentifiers do
				if GetMapValsCall then
					break
				end

				if #identifier == #findIdentifiers then
					local passed = true
					for i = 1, #findIdentifiers do
						if identifier[i] ~= findIdentifiers[i] then
							passed = false
							break
						end
					end

					GetMapValsCall = passed
				end
			end

			local TweenServiceAsign = false
			local TweenServiceIdentifiers = {{"game", "TweenService"}}

			for _, findIdentifiers in TweenServiceIdentifiers do
				if TweenServiceAsign then
					break
				end

				if #identifier == #findIdentifiers then
					local passed = true
					for i = 1, #findIdentifiers do
						if identifier[i] ~= findIdentifiers[i] then
							passed = false
							break
						end
					end

					TweenServiceAsign = passed
				end
			end

			if GetMapValsCall then
				print('Recognize GetMapVals, is Lib')
				return SpecialIdentifier.FE2Lib
			elseif TweenServiceAsign then
				print('TweenService Assignment found!')
				return SpecialIdentifier.TweenService

				-- idk how this works
			elseif identifier[1] == "game" then
				if identifier[2] == "GetService" and isSelfCall then
					local serviceName = arguments[2]
					if type(serviceName) == "string" then
						if serviceName == "TweenService" then
							print('TweenService Assignment via GetService found!')
							return SpecialIdentifier.TweenService
						end
					end
				end
			elseif identifier[1] == SpecialIdentifier.FE2Lib then
				if identifier[2] == "Script" then
					print('Entering Script Section of Lib')
					if identifier[3] == 'MovePart' or identifier[3] == 'moveWater' then
						print("moveWater/MovePart solver")

						print("Attempt to find instance from argument:", arguments[1])
						local resolvedInstance, resolveError = getInstanceFromStrings(arguments[1])
						if not resolvedInstance then
							return warn(`Failed to resolve instance: {resolveError}`)
						end

						print("Attempt to find translation from argument:", arguments[2])
						local translation = nil
						-- Second argument must be Vector3.new, or CFrame.new, or etc.
						-- That's why it must be a table
						-- Also for now we assume there's only 2 identifiers inside
						if type(arguments[2]) ~= "table" then
							return warn(`Second argument is not an expression.`)
						end
                        local secondArgument: {name: string | {string}, type: string, arguments: {string}} = arguments[2] :: any

						if secondArgument.type == "functionCall" then
							if type(secondArgument.name) ~= "table" then
								return warn(`Invalid function.`) -- TODO: better warning
							end

							local resolvedProp
							if secondArgument.name[1] == "Vector3" then
								resolvedProp = Vector3[secondArgument.name[2]]
							elseif secondArgument.name[1] == "CFrame" then
								resolvedProp = CFrame[secondArgument.name[2]]
							end

							if not resolvedProp then
								return warn(`Unknown property "{secondArgument.name[2]}" for {secondArgument.name[1]}`)
							end
							if type(resolvedProp) ~= "function" then
								return warn(`{secondArgument.name[1]}.{secondArgument.name[2]} is not a function.`)
							end
							local success, functionError = pcall(function()
								translation = resolvedProp(unpack(secondArgument.arguments))
							end)
							if not success then
								return warn(`Failed to get translation from {secondArgument.name[1]}.{secondArgument.name[2]}: {functionError}`)
							end
						else
							return warn("Cannot handle second argument:", arguments[2])
						end

						local easingStyle = nil
						if arguments[5] then
							if type(arguments[5]) == "string" then
								pcall(function()
									easingStyle = (Enum.EasingStyle[arguments[5]] :: Enum.EasingStyle).Name
								end)
							elseif type(arguments[5]) == "table" then
                                local fifthArgument: {string} = arguments[5] :: any

								if fifthArgument[1] == "Enum" and fifthArgument[2] == "EasingStyle" then
									pcall(function()
										easingStyle = (Enum.EasingStyle[fifthArgument[3]] :: Enum.EasingStyle).Name
									end)
								else
									return warn(`Unhandled 5th argument: {fifthArgument}`)
								end
							end
						end

						local easingDirection = nil
						if arguments[6] then
							if type(arguments[6]) == "string" then
								pcall(function()
									easingDirection = (Enum.EasingDirection[arguments[6]] :: Enum.EasingDirection).Name
								end)
							elseif type(arguments[6]) == "table" then
                                local sixthArgument: {string} = arguments[6] :: any

								if sixthArgument[1] == "Enum" and sixthArgument[2] == "EasingDirection" then
									pcall(function()
										easingDirection = (Enum.EasingDirection[sixthArgument[3]] :: Enum.EasingDirection).Name
									end)
								else
									return warn(`Unhandled 6th argument: {sixthArgument}`)
								end
							end
						end

						print(`Creating XFrame {identifier[3]} at time {getScopeTotalRunTime()}`)
						getScopeTimeline():newXFrame(`{identifier[3]}_At_{getScopeTotalRunTime()}`, resolvedInstance, {
							Translation = translation,--Vector3.new(unpack(arguments[2]))
							UseLocalSpace = arguments[4],
							EasingStyle = easingStyle or 'Linear',
							EasingDirection = easingDirection or 'Out'
						},
						{
							Function = "MovePart",
							Length = arguments[3],
							Timestamp = getScopeTotalRunTime()
						})
					elseif identifier[3] == 'setWaterState' then
						print("setWaterState solver")

						print("Attempt to find instance from argument:", arguments[1])
						local resolvedInstance, resolveError = getInstanceFromStrings(arguments[1])
						if not resolvedInstance then
							return warn(`Failed to resolve instance: {resolveError}`)
						end

						print(`Creating XFrame {identifier[3]} at time {getScopeTotalRunTime()}`)
						getScopeTimeline():newXFrame(`{identifier[3]}_At_{getScopeTotalRunTime()}`, resolvedInstance, {
							State = arguments[2]
						},
						{
							Function = "SetWaterState",
							Timestamp = getScopeTotalRunTime()
						})

					end
				end
			elseif identifier[1] == "task" then
				if identifier[2] == "wait" then
					local duration = arguments[1] or 1 / 60
					scopeWait(tonumber(duration))
				end
			elseif identifier[1] == SpecialIdentifier.TweenService and identifier[2] == "Create" then
				print('Tween function found', arguments)

				local resolvedInstance = arguments[1]
				local Tween_Info = arguments[2]
				local Property_Table = arguments[3]
				local TweenDuration = 0

				local Timeline_TweenInfo = {
					EasingStyle = "Linear",
					EasingDirection = "Out",
					RepeatCount = 0,
					Reverses = false,
					DelayTime = 0
				}
				local Timeline_PropertyTable = {}

				-- Of course, this only pass the case: TweenService:Create(Object, TweenInfo.new())
				-- If we wanna make it work with variable, we need to do some uhm... tricky thing
				-- Let's figure out later, we need to do simple stuff first
				if type(Tween_Info) == "table" and Tween_Info.type == "functionCall" and Tween_Info.name[1] == "TweenInfo" and Tween_Info.name[2] == "new" then
					local Duration = Tween_Info.arguments[1]
					local EasingStyle = Tween_Info.arguments[2]
					local EasingDirection = Tween_Info.arguments[3]


					TweenDuration = tonumber(Duration)
					-- check if Enum thing
					-- EasingStyle can be assigned by just the Enum value (ex: Linear = 0) but whatever
					if EasingStyle[1] == "Enum" and EasingStyle[2] == "EasingStyle" then
						Timeline_TweenInfo.EasingStyle = EasingStyle[3]
					end
					if EasingDirection[1] == "Enum" and EasingDirection[2] == "EasingDirection" then
						Timeline_TweenInfo.EasingStyle = EasingStyle[3]
					end

					if Tween_Info.arguments[4] ~= nil then
						Timeline_TweenInfo.RepeatCount = tonumber(Tween_Info.arguments[4])
					end
					if Tween_Info.arguments[5] ~= nil then
						Timeline_TweenInfo.Reverses = Tween_Info.arguments[5] == "true" and true or false
					end
					if Tween_Info.arguments[6] ~= nil then
						Timeline_TweenInfo.DelayTime = tonumber(Tween_Info.arguments[6])
					end
				else
					return warn("TweenService:Create must have TweenInfo.new as second argument")
				end

				-- if if this is correct but ye
				if type(Property_Table) == "table" then
					for p, v in pairs(Property_Table) do
						Timeline_PropertyTable[p] = v
					end
				end

				-- Deal with Operators
				-- need to check if resolvedInstance is set somewhere in the thing
				local SupportedTypes = {"CFrame", "number", "Vector3", "Vector2", "UDim2", "UDim"}
				if #Timeline_PropertyTable == 1 and Property_Table[2][1] == resolvedInstance.Name and Property_Table then
					-- HOW DO I GET THE OPERATOR IN THE PROPERTY TABLE AA
					local pp = Property_Table[2][2]
					--now to convert the mess into something readable
					-- getfenv() -- >:)

				end

				-- Create XFrame
				-- Might make use of Operators (Especially if we're testing HS V2)
				-- Parser broke, so implementation of Operators are delayed
				-- Idea: If property has operator that adds the object's property, then seperate and create new XFrame with Operator
				getScopeTimeline():newXFrame(`{identifier[3]}_At_{getScopeTotalRunTime()}`, resolvedInstance, {

				},
				{
					Function = "SetProperties",
					Length = Timeline_TweenInfo.Duration,
					Timestamp = getScopeTotalRunTime()
				}, Timeline_PropertyTable, Timeline_TweenInfo)
			end
		elseif type(identifier) == "string" then
			if identifier == "wait" then
				local duration = arguments[1] or 1 / 30
				scopeWait(tonumber(duration))
			end
		end		

		return {
			type = "functionCall",
			name = identifier,
			arguments = arguments
		}
	end
	
	function Visitor:onFunctionReassignment(name: Visitor.Identifier | Visitor.IndexIdentifier, value: Visitor.FunctionDeclarationData)
		if value and type(name) == "table" then
			if name[1] == SpecialIdentifier.FE2Lib and name[2] == "btnFuncs" then
				if not name[3] then
					return warn("No index for btnFuncs assignment")
				end

				print('Is assigning function to btnFuncs[', name[3], '], collecting timeline')

				local btnFuncTimeline: timelineScope = getScopeTimeline(value.scopeId)
				btnFuncTimeline.name = "btnFuncs_" .. name[3]
				btnFuncTimeline.properties.Trigger_Button = name[3]
			end
		end
	end
	
	function Visitor:visitNumericalLoop(expressions: {Visitor.ExpressionNode}?, scopeId: number)
		if not expressions or (#expressions < 3 or #expressions > 3) then
			return
		end

		local loopTimelineRoot = timelineScope.new("IncrementLoop", {
			RepeatOnCompletion = true
		})
		-- should place somewhere in the map, maybe
		local loopNumValue = Instance.new("NumberValue")
		loopNumValue.Value = expressions[1] + expressions[3]

		loopTimelineRoot:newXFrame("Condition", loopNumValue, {
			RelationalOperator = "~=",
		}, {
			Function = "Conditional",
			Timestamp = 0
		}, {
			Value = expressions[2]
		})
		loopTimelineRoot:newXFrame("Increment", loopNumValue, {
			Operator = "+",
		}, {
			Function = "SetProperties",
		}, {
			Value = expressions[3]
		})
		
		table.insert(scopesTimeline[visitor:currentScopeId()], loopTimelineRoot)
	end
	
	-- Start visiting --
	visitor:visit(Tree, true)
	print(visitor)
	
	-- Create timeline instances
	local timelinesFolder = MapModel:FindFirstChild("Timelines")
	if timelinesFolder then
		print('Found existing Timelines folder, backing up.')
		timelinesFolder.Name = "Timelines_Backup"
	end
	timelinesFolder = Instance.new("Folder")
	timelinesFolder.Name = "Timelines"
	timelinesFolder.Parent = MapModel
	
	for _, timelines in scopesTimeline :: {timelineScope} do
		for _, timeline in timelines do
			if #timeline.xframes > 0 then
				print('Creating timeline instance', timeline.name)
				timeline:makeInstance().Parent = timelinesFolder
			end
		end
	end
	
	print('Finished converting to Timelines in:', tick() - startTime, 'seconds.')
end

return Converter
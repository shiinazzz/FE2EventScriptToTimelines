-- Convert from EventScript to Timelines with custom Luau parser.
-- cutymeo (ari)
-- Additional help: LacticMilk (yay :troll:)

local Lexer = require(script.Parent.LuauAstBuilder.Lexer)
local Parser = require(script.Parent.LuauAstBuilder.Parser)
local SyntaxNode = require(script.Parent.LuauAstBuilder.SyntaxNode)

local Converter = {}

local SpecialIdentifier = {
	FE2Lib = ".FE2Lib"
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
	-- (I think we should make a GitHub and have people contribute)
	
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
					if startIndex < #strings then
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
	
	function timelineScope.new(name: string, properties: {[string]: any}?)
		return setmetatable({
			name = name,
			properties = properties or {},
			xframes = {}
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
	
	function timelineScope:makeInstance()
	end
	
	-- yea idk
	local MainTimeline = timelineScope.new("Flood_Timeline", {})
	
	-- [[ VM HANDLER ]]
	-- Create initial data tables to hold: scope data (variables (global/local), predict exec time, name, etc.)
	-- VM start at "global" scope, which is the "outside" scope in script
	-- Each "do end" creates a new scope
	-- Each scope should return some data that can be used to analyze
	-- (Such as FunctionCall, LocalReassignment, etc.)
	local vm = {
		scopes = {},
		currentScope = nil,
	}
	
	function vm.getVariable(scopeName: string, variableName: string | {string}): any
		local scopeData = vm.scopes[scopeName]
		if not scopeData then
			return nil
		end

		local variables = scopeData.variables
		
		-- If they are an array of strings, we check if the variables tree contains all of that strings
		-- (Lib.btnFuncs, in the tree would be Lib = { btnFuncs = ... }) (sort of)
		if type(variableName) == "table" then
			local currentVariableTable = variables
			for i = 1, #variableName do
				if i == #variableName then
					return currentVariableTable[variableName[i]] and currentVariableTable[variableName[i]].value
				else
					if not currentVariableTable[variableName[i]] or currentVariableTable[variableName[i]].value then
						return false
					end
					currentVariableTable = currentVariableTable[variableName[i]].value
				end
			end
		elseif type(variableName) == "string" then
			return variables[variableName] and variables[variableName].value
		end
		
		return nil
	end
	
	function vm.allocateVariable(variableName: string | {string}, value: any)
		local scopeData = vm.scopes[vm.currentScope]
		if not scopeData then
			error(`Cannot allocate variable: No scope data exist for scope: {vm.currentScope}`)
		end
		
		local variables = scopeData.variables
		if type(variableName) == "table" then
			local currentVariableTable = variables
			for i = 1, #variableName do
				-- If variable table is not a table (to store more variables)
				-- We make it a table
				if type(currentVariableTable[variableName[i]]) ~= "table" or currentVariableTable[variableName[i]].type ~= "Table" then
					currentVariableTable[variableName[i]] = {type = "Table", value = {}}
				end
				-- If we reached the final variable in the tree
				-- We set it as a Literal
				if i == #variableName then
					currentVariableTable[variableName[i]].type = "Literal"
					currentVariableTable[variableName[i]].value = value
				else
					currentVariableTable = currentVariableTable[variableName[i]].value
				end
			end
		elseif type(variableName) == "string" then
			variables[variableName] = {type = "Literal", value = value}
		else
			error(`Cannot allocate variable: Invalid variable name type (Expected \`string\` or \`\{string\}\`, got {type(variableName)}`)
		end
	end
	
	-- [[ Scope related functions ]] --
	
	-- This create a new scope in vm, allocating variables and other data
	function vm.createScope(scopeName: string)
		-- If we already created scope, don't go further
		if vm.scopes[scopeName] then
			return
		end
		
		vm.scopes[scopeName] = {
			complicated = false,
			variables = {},
			data = {},
			timelinesData = {},
			totalRunTime = 0, -- Used to mark for Timestamp
		}
	end
	
	-- Add data returned by a node to scope data
	function vm.addNodeData(scopeName: string, nodeData: any)
		if not vm.scopes[scopeName] then
			error(`Cannot add node data to scope {scopeName}: No scope data exists.`)
		end
		if not nodeData then
			return
		end
		
		table.insert(vm.scopes[scopeName].data, nodeData)
	end
	
	function vm.wait(time: number)
		local scopeData = vm.scopes[vm.currentScope]
		if not scopeData then
			error(`Cannot wait: No scope data exist for current scope: {vm.currentScope}`)
		end
		if type(time) ~= "number" then
			return
		end

		scopeData.totalRunTime += time
	end
	
	function vm.getTotalRunTime()
		local scopeData = vm.scopes[vm.currentScope]
		if not scopeData then
			error(`Cannot get total run time: No scope data exist for current scope: {vm.currentScope}`)
		end

		return scopeData.totalRunTime
	end
	
	-- [[ AST node related functions ]] --
	function vm.visitNode(node: SyntaxNode.SyntaxNode): any
		-- NOTE: Read the Parser source code to understand what's the children actually are
		if node.Kind == SyntaxNode.SyntaxNodeKind.Scope then
			vm.visitScope(node.ScopeName :: string, node)
			return vm.scopes[node.ScopeName]
		end
		
		-- Scope-creating
		if node.Kind == SyntaxNode.SyntaxNodeKind.NumericalLoop then
			-- for i = x, y, z
			-- Children:
			-- [1] = SyntaxNodeKind.ExpressionList (condition)
			-- [2] = SyntaxNodeKind.Scope (body)
            if not node.Children then
                return
            end
			
			local expressions = vm.visitNode(node.Children[2])
			local scopeData = vm.visitNode(node.Children[3])

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
			
			table.insert(vm.scopes[vm.currentScope].timelinesData, {
				object = timelineScope,
				remainScopeData = scopeData -- remainScopeData will be processed later
				-- NOTE: timeline has some complications with timing
			})
            return
		elseif node.Kind == SyntaxNode.SyntaxNodeKind.IfStatement then
			-- How Conditional Xframe works: runs at XFrame_Timeline of the thing. If condition is met, then
			-- continue the Timeline, if not, it'll stop the timeline, or else redirect to a new timeline
			
			-- Children:
			-- [1] = SyntaxNodeKind.ExpressionList (condition)
			-- [2] = SyntaxNodeKind.Scope (then scope)
			-- [3] = SyntaxNodeKind.Scope? (else scope, only if "else" is there)
            if not node.Children then
                return
            end
			
			local ifTimelineRoot = timelineScope.new("IfStatement")
			local ExpressionList = vm.visitNode(node.Children[1])
			ifTimelineRoot:newXFrame("Conditional")
		elseif node.Kind == SyntaxNode.SyntaxNodeKind.WhileLoop then
			-- This is another simple node, easy to do
			-- Children:
			-- [1] = SyntaxNodeKind.ExpressionList (condition)
			-- [2] = SyntaxNodeKind.Scope (body)
            if not node.Children then
                return
            end

			local expressions = vm.visitNode(node.Children[1])
			local scopeData = vm.visitNode(node.Children[2])
			
			-- we skip :troll:
			if #expressions >= 4 then
				return
			end
			
			local loopTimelineRoot = timelineScope.new("IncrementLoop", {
				RepeatOnCompletion = true
			})
			if expressions[1] ~= SyntaxNode.SyntaxNodeKind.True then
				loopTimelineRoot:newXFrame("Condition", expressions[1], {
					RelationalOperator = expressions[2],
				}, {
					Function = "Conditional",
					Timestamp = 0
				}, {
					Value = expressions[3]
				})
			end
			
			table.insert(vm.scopes[vm.currentScope].timelinesData, {
				object = timelineScope,
				remainScopeData = scopeData -- remainScopeData will be processed later
				-- NOTE: timeline has some complications with timing
			})
		elseif node.Kind == SyntaxNode.SyntaxNodeKind.FunctionCall then
			if not node.Children then
				return
			end

			local resolvedIdentifier = vm.visitNode(node.Children[1])
			local resolvedArguments = vm.visitNode(node.Children[2])
			
			-- thinking
			-- do we do a { type = "literal" | "dynamic", value = ... } like in ref
			-- or something
			-- (or tbh if type is dynamic we just say fuck it)
			-- (then we can just make it an array of strings)
			-- idk maybe the easy way
			-- Seperate scope for handling timelines
			do
				if resolvedIdentifier and type(resolvedIdentifier) == "table" then
					-- wanna know why
					-- what if Lib is actually not the Lib we are looking for
					-- :troll:
					if resolvedIdentifier[1] == SpecialIdentifier.FE2Lib and resolvedIdentifier[2] == "Script" then
						if resolvedIdentifier[3] == 'MovePart' or resolvedIdentifier[3] == 'moveWater' then
							local resolvedInstance, resolveError = getInstanceFromStrings(resolvedArguments[1])
							if not resolvedInstance then
								return warn(`Failed to resolve instance: {resolveError}`)
							end
							local translation = nil
							-- Second argument must be Vector3.new, or CFrame.new, or etc.
							-- That's why it must be a table
							-- Also for now we assume there's only 2 identifiers inside
							if type(resolvedArguments[2].name) ~= "table" then
								return warn(`Second argument is not an expression.`)
							end
							if resolvedArguments[2].type == "functionCall" then
								if type(resolvedArguments[2].name) ~= "table" then
									return warn(`Invalid function.`) -- TODO: better warning
								end
								
								local resolvedProp
								if resolvedArguments[2].name[1] == "Vector3" then
									resolvedProp = Vector3[resolvedArguments[2].name[2]]
								elseif resolvedArguments[2].name[1] == "CFrame" then
									resolvedProp = CFrame[resolvedArguments[2].name[2]]
								end
								
								if not resolvedProp then
									return warn(`Unknown property "{resolvedArguments[2].name[2]}" for {resolvedArguments[2].name[1]}`)
								end
								if type(resolvedProp) ~= "function" then
									return warn(`{resolvedArguments[2].name[1]}.{resolvedArguments[2].name[2]} is not a function.`)
								end
								local success, functionError = pcall(function()
									translation = resolvedProp(unpack(resolvedArguments[2].arguments))
								end)
								if not success then
									return warn(`Failed to get translation from {resolvedArguments[2].name[1]}.{resolvedArguments[2].name[2]}: {functionError}`)
								end
							elseif type(resolvedArguments[2][1]) == "string" and type(resolvedArguments[2][2]) == "string" then
								local resolvedProp
								if resolvedArguments[2][1] == "Vector3" then
									resolvedProp = Vector3[resolvedArguments[2][2]]
								elseif resolvedArguments[2][1] == "CFrame" then
									resolvedProp = CFrame[resolvedArguments[2][2]]
								end
								
								if not resolvedProp then
									return warn(`Unknown property "{resolvedArguments[2][2]}" for {resolvedArguments[2][1]}`)
								end
								translation = resolvedProp
							end
							
							local easingStyle = nil
							if resolvedArguments[5] then
								if type(resolvedArguments[5]) == "string" then
									pcall(function()
										easingStyle = (Enum.EasingStyle[resolvedArguments[5]] :: Enum.EasingStyle).Name
									end)
								elseif type(resolvedArguments[5]) == "table" then
									if resolvedArguments[5][1] == "Enum" and resolvedArguments[5][2] == "EasingStyle" then
										pcall(function()
											easingStyle = (Enum.EasingStyle[resolvedArguments[5][3]] :: Enum.EasingStyle).Name
										end)
									else
										return warn(`Unhandled 5th argument: {resolvedArguments[5]}`)
									end
								end
							end
							
							local easingDirection = nil
							if resolvedArguments[6] then
								if type(resolvedArguments[6]) == "string" then
									pcall(function()
										easingDirection = (Enum.EasingDirection[resolvedArguments[6]] :: Enum.EasingDirection).Name
									end)
								elseif type(resolvedArguments[6]) == "table" then
									if resolvedArguments[6][1] == "Enum" and resolvedArguments[6][2] == "EasingDirection" then
										pcall(function()
											easingDirection = (Enum.EasingDirection[resolvedArguments[6][3]] :: Enum.EasingDirection).Name
										end)
									else
										return warn(`Unhandled 6th argument: {resolvedArguments[6]}`)
									end
								end
							end
							
							MainTimeline:newXFrame(`{resolvedIdentifier[3]}_At_{vm.getTotalRunTime()}`, resolvedInstance, {
								general = {
									Translation = translation,--Vector3.new(unpack(resolvedArguments[2]))
									UseLocalSpace = resolvedArguments[4],
									EasingStyle = easingStyle or 'Linear',
									EasingDirection = easingDirection or 'Out'
								},
								xframe = {
									Function = "MovePart",
									Length = resolvedArguments[3],
									Timestamp = vm.getTotalRunTime()
								}
							})
						elseif resolvedIdentifier[3] == 'setWaterState' then
							local resolvedInstance, resolveError = getInstanceFromStrings(resolvedArguments[1])
							if not resolvedInstance then
								return warn(`Failed to resolve instance: {resolveError}`)
							end
							MainTimeline:newXFrame(`{resolvedIdentifier[3]}_At_{vm.getTotalRunTime()}`, resolvedInstance, {
								general = {
									State = resolvedArguments[2]
								},
								xframe = {
									Function = "SetWaterState",
									Timestamp = vm.getTotalRunTime()
								}
							})
							
						end
					-- we handle this in processData
					-- because they are variables, not functionCall
					--elseif resolvedIdentifier[1] == SpecialIdentifier.FE2Lib and resolvedIdentifier[2] == "btnFuncs" then
						
					elseif resolvedIdentifier[1] == "wait" or (resolvedIdentifier[1] == "task" and resolvedIdentifier[2] == "wait") then
						vm.wait(tonumber(resolvedArguments[1]) :: number)
					else
                        -- ok lactic
						-- ignore this for now xD
						local funct = getfenv()[resolvedIdentifier[1]]
						if type(funct) == 'function' then
							funct(unpack(resolvedArguments))
						end
					end
				end
			end			
			
			return {
				type = "functionCall",
				name = resolvedIdentifier,
				arguments = resolvedArguments
			}
		end

        return
	end
	
	function vm.visitScope(scopeName: string, scope: SyntaxNode.SyntaxNode)
		vm.createScope(scopeName)

		local lastScope = vm.currentScope
		vm.currentScope = scopeName
		if scope.Children ~= nil then
			for _, node in scope.Children do
				vm.addNodeData(scopeName, vm.visitNode(node))
			end
		end
		vm.currentScope = lastScope
	end
	
	-- [[ Process-related functions ]]
	function vm.processData(nodeData: {any})
		-- Process scope first
		for _, data in nodeData do
			if data.type == "functionCall" then
				
			end
		end
		
		-- Now we process extra timelines
	end
	
	-- Start visiting --
	vm.createScope("global")
	vm.visitScope("main", Tree)
	table.insert(vm.scopes.main.timelinesData, MainTimeline)
	vm.processData(vm.scopes.main.data)
end

return Converter
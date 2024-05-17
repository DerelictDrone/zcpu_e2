if not CPULib then
	include("wire/cpulib.lua")
end

-- Returns false if extension is already created/registered
local myCPUExtension = CPULib:CreateExtension("e2_compiler","CPU")

if myCPUExtension then

	local function addE2Functions(VM,Operands)
		VM.E2Contexts = {}
		VM.TargetedE2Context = 0 -- intenional wipe of previous state
		function VM:ReadString(address)
			local charString = ""
			local charCount = 0
			local currentChar = 255
		  
			while currentChar ~= 0 do
			  currentChar = self:ReadCell(address + charCount)
			  -- Reading failed
			  if not currentChar then
				return
			  elseif currentChar > 0 and currentChar < 255 then
				charString = charString .. string.char(currentChar)
			  elseif currentChar ~= 0 then
				  self:Interrupt(23,currentChar)
				  return ""
			  end
		  
			  charCount = charCount + 1
			  if charCount > 8192 then
				self:Interrupt(23,0)
				return ""
			  end
			end
			return charString
		end
	end

	myCPUExtension:InstructionFromLuaFunc(
		"E2_INIT",
		0,
		addE2Functions,
		{},
		{
			Version = 0.42,
			Description = "Sets up ZCPU to allow compilation and execution of E2 scripts"
		}
	)

	local function generateE2Context(owner,script) -- Generates the parts of the context we need to be able to track and modify the state of an E2 Script
		if E2Lib.RuntimeContext then
			-- print("Preprocessor")
			local status, directives, code = E2Lib.PreProcessor.Execute(script)
			if not status then return false end
			-- print("Tokenizer")
			local status, tokens = E2Lib.Tokenizer.Execute(code)
			if not status then return false end
			-- print("Parser")
			local status, tree, dvars = E2Lib.Parser.Execute(tokens)
			if not status then return false end
			-- foregoing inputs and outputs, they aren't exactly needed seeing as we can write into any global variable in the global space
			return E2Lib.RuntimeContext.builder():withOwner(owner):withStrict(directives.strict):withPersists(directives.persist[3]):withDeltaVars(dvars):build()
		end
		return false
	end

	local function generateE2Handle(VM,Operands)
		local str = VM:ReadString(Operands[2]) -- read the string from the address in Operands[2]
		if not str then
			Operands[1] = 0 -- Return 0 to indicate context couldn't be made
			return
		end
		if E2Lib then
			local E2 = {context={}}
			local ctx = generateE2Context(VM.Entity:GetPlayer(),str)
			if ctx then
				local status
				E2.context = ctx
				E2.context.entity = VM.Entity
				status,E2.E2Func = E2Lib.compileScript(str,VM.E2Contexts[#VM.E2Contexts])
				if status then
					for ind,i in ipairs(VM.E2Contexts) do -- iterate through checking for any freed indices to reuse
						if not i then
							VM.E2Contexts[ind] = E2
							Operands[1] = ind
							return
						end
					end
					-- No free indices, add a new one
					if #VM.E2Contexts < 8 then
						Operands[1] = table.insert(VM.E2Contexts,E2)
					else
						Operands[1] = 0 -- Discard the attempt, we ran over the max amount of E2's.
					end
					return
				end
			end
		end
		Operands[1] = 0
	end

	myCPUExtension:InstructionFromLuaFunc(
		"E2_COMPILE",
		2,
		generateE2Handle,
		{"W1"},
		{
			Version = 0.42,
			Description = "Compiles an E2 script from string pointer in register 2, then writes handle number or 0(failed) to register 1"
		}
	)

	local function setTargetContext(VM,Operands)
		VM.TargetedE2Context = Operands[1]
	end

	myCPUExtension:InstructionFromLuaFunc(
		"E2_OPEN_HANDLE",
		1,
		setTargetContext,
		{},
		{
			Version = 0.42,
			Description = "Sets currently targeted E2 handle for I/O"
		}
	)

	local function readTargetContext(VM,Operands)
		Operands[1] = VM.TargetedE2Context or 0 -- 0 aka no context selected
	end

	myCPUExtension:InstructionFromLuaFunc(
		"E2_READ_HANDLE",
		1,
		readTargetContext,
		{"W1"},
		{
			Version = 0.42,
			Description = "Reads the currently targeted E2 handle to register"
		}
	)


	local function checkContextValid(VM,Handle)
		if Handle > 0 then
			if #VM.E2Contexts <= Handle then
				if VM.E2Contexts[Handle] then
					return true
				end
			end
		end
		return false
	end

	local function ExecuteE2(VM,Operands)
		if checkContextValid(VM,Operands[2]) then
			local success,msg = VM.E2Contexts[Operands[2]].E2Func(VM.E2Contexts[Operands[2]].context)
			if not success then
				VM.E2Contexts[Operands[2]].StatusMsg = msg
				Operands[1] = 1
				return
			end
			Operands[1] = 0
			return
		end
		Operands[1] = 1
	end

	myCPUExtension:InstructionFromLuaFunc(
		"E2_EXEC_HANDLE",
		2,
		ExecuteE2,
		{"W1"},
		{
			Version = 0.42,
			Description = "Runs E2 handle from Register 2 and 0 writes to Register 1 on success or writes 1 if an error occured in execution"
		}
	)

	local function DestroyE2(VM,Operands)
		if checkContextValid(VM,Operands[1]) then
			VM.E2Contexts[VM.Operands[1]] = false
		end
	end
	myCPUExtension:InstructionFromLuaFunc(
		"E2_FREE_HANDLE",
		2,
		DestroyE2,
		{},
		{
			Version = 0.42,
			Description = "Destroys the E2 with the specified handle, freeing that space for another E2"
		}
	)

	local function writeNumberVariable(VM,Operands)
		if checkContextValid(VM,VM.TargetedE2Context) then
			local str = VM:ReadString(Operands[1])
			if str then
				VM.E2Contexts[VM.TargetedE2Context].context.GlobalScope[str] = Operands[2]
			end
		end
	end

	myCPUExtension:InstructionFromLuaFunc(
		"E2_WRITE_NUM",
		2,
		writeNumberVariable,
		{},
		{
			Version = 0.42,
			Description = "Writes number value to variable in targeted E2 handle using Register 1 as Variable Name and Register 2 as value"
		}
	)

	local function readNumberVariable(VM,Operands)
		if checkContextValid(VM,VM.TargetedE2Context) then
			local str = VM:ReadString(Operands[2])
			if str then
				Operands[1] = VM.E2Contexts[VM.TargetedE2Context].context.GlobalScope[str]
			end
		end
	end
	myCPUExtension:InstructionFromLuaFunc(
		"E2_READ_NUM",
		2,
		readNumberVariable,
		{"W1"},
		{
			Version = 0.42,
			Description = "Reads number value from targeted E2 handle to Register 1 using Register 2 as Variable Name"
		}
	)

	local function writeCPUWirelink(VM,Operands)
		if checkContextValid(VM,VM.TargetedE2Context) then
			local str = VM:ReadString(Operands[1])
			if str then
				VM.E2Contexts[VM.TargetedE2Context].context.GlobalScope[str] = VM.Entity
			end
		end
	end
	myCPUExtension:InstructionFromLuaFunc(
		"E2_LINK_MEM",
		1,
		writeCPUWirelink,
		{},
		{
			Version = 0.42,
			Description = "Writes CPU Entity to variable using Register 1 as Variable Name"
		}
	)

end
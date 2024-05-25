if not CPULib then
	include("wire/cpulib.lua")
end

-- Returns false if extension is already created/registered
local myCPUExtension = CPULib:CreateExtension("e2_compiler", "CPU")

if myCPUExtension then

	local function addE2Functions(VM, Operands)
		VM.E2Contexts = {}
		VM.TargetedE2Context = 0 -- intentional wipe of previous state
		VM.E2TypeInfo = {
			TypeNames = {},
			TypeIDs = {}
		} -- Every E2 type with an arbitrary ID, to automatically generate an ENUM for types
		-- Rather than getting a CRC of the full type name, just pack the shortened type name into a 32 bit number
		-- None of the current E2 types from all of the cores seem to be over 3 bytes, and any collisions would
		-- have just wiped a type out before it got into this table, so all of them should be unique.
		-- Plus, the algorithm should be fairly simple to replicate and reverse for ZCPU users.
		for k, i in pairs(wire_expression_types2) do
			local bytes = table.Pack(string.byte(k, 1, 4));
			if bytes then
				local num = bit.lshift(bytes[1] or 0, 8)
				num = bit.lshift(bit.bor(num, bytes[2] or 0), 8)
				num = bit.lshift(bit.bor(num, bytes[3] or 0), 8)
				num = bit.bor(num, bytes[4] or 0)
				-- Dual lookup so we can get name from ID and ID from name easily
				VM.E2TypeInfo.TypeNames[k] = {
					id = num,
					typeinfo = i
				}
				VM.E2TypeInfo.TypeIDs[num] = {
					name = k,
					typeinfo = i
				}
			end
		end
		VM.HookedJumps = {}
		if not VM.E2HookedJumps then
			VM.E2HookedJumps = true
			VM.oldJumpFunc = VM.Jump
			function VM:Jump(IP,CS)
				print("Jumping to ",IP,CS)
				local jmpSig = (CS or 0)..":"..(IP or 0)
				local hookedJumpsAvailable = VM.HookedJumps[jmpSig]
				local leftoverHooks = {}
				if hookedJumpsAvailable then
					print("Jump got a hook!")
					for ind,i in ipairs(hookedJumpsAvailable) do
						if not i.OneTime then
							table.insert(leftoverHooks,i)
						end
						i.func(self) -- pass vm so they can do what they need to with it
					end
					if #leftoverHooks > 0 then
						VM.HookedJumps[(CS or 0)..":"..(IP or 0)] = leftoverHooks
					else
						-- No hooks left? Clean it out.
						VM.HookedJumps[(CS or 0)..":"..(IP or 0)] = nil
					end
				end
				VM:oldJumpFunc(IP,CS)
			end
			-- replace implementation of JMP to check if we're jumping to an IP that we're expecting
			-- to be returned to after an e2 => zcpu call
			-- Thinking on it now, this may also be the way to allow a zcpu => e2 call, by hooking
			-- an IP that we jump to and then returning immediately after
				-- Generate a one-time jump hook to the current CS,IP
				function VM:GenerateHookedReturn(func)
					local jmpSig = (self.CS or 0)..":"..(self.IP or 0)
					local hookedJump = {func = func, OneTime=true}
					if VM.HookedJumps[jmpSig] then
						table.insert(VM.HookedJumps[jmpSig],hookedJump)
						return
					end
					-- If there's none yet, create it.
					VM.HookedJumps[jmpSig] = {hookedJump}
				end
		end
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
					self:Interrupt(23, currentChar)
					return ""
				end

				charCount = charCount + 1
				if charCount > 8192 then
					self:Interrupt(23, 0)
					return ""
				end
			end
			return charString
		end
		function VM:checkE2ContextValid(Handle)
			if Handle > 0 then
				if #self.E2Contexts <= Handle then
					if self.E2Contexts[Handle] then
						return true
					end
				end
			end
			return false
		end
	end

	myCPUExtension:InstructionFromLuaFunc("E2_INIT", 0, addE2Functions, {}, {
		Version = 0.42,
		Description = "Sets up ZCPU to allow compilation and execution of E2 scripts"
	})

	local function generateE2Context(owner, script) -- Generates the parts of the context we need to be able to track and modify the state of an E2 Script
		if E2Lib.RuntimeContext then
			-- print("Preprocessor")
			local status, directives, code = E2Lib.PreProcessor.Execute(script)
			if not status then
				return false
			end
			-- print("Tokenizer")
			local status, tokens = E2Lib.Tokenizer.Execute(code)
			if not status then
				return false
			end
			-- print("Parser")
			local status, tree, dvars = E2Lib.Parser.Execute(tokens)
			if not status then
				return false
			end
			-- foregoing inputs and outputs, they aren't exactly needed seeing as we can write into any global variable in the global space
			return E2Lib.RuntimeContext.builder():withOwner(owner):withStrict(directives.strict):withPersists(
				directives.persist[3]):withDeltaVars(dvars):build()
		end
		return false
	end

	local function generateE2Handle(VM, Operands)
		local str = VM:ReadString(Operands[2]) -- read the string from the address in Operands[2]
		if not str then
			Operands[1] = 0 -- Return 0 to indicate context couldn't be made
			return
		end
		if E2Lib then
			local E2 = {
				context = {}
			}
			local ctx = generateE2Context(VM.Entity:GetPlayer(), str)
			if ctx then
				local status
				E2.context = ctx
				E2.context.entity = VM.Entity
				status, E2.E2Func = E2Lib.compileScript(str, VM.E2Contexts[#VM.E2Contexts])
				if status then
					for ind, i in ipairs(VM.E2Contexts) do -- iterate through checking for any freed indices to reuse
						if not i then
							VM.E2Contexts[ind] = E2
							Operands[1] = ind
							return
						end
					end
					-- No free indices, add a new one
					if #VM.E2Contexts < 8 then
						Operands[1] = table.insert(VM.E2Contexts, E2)
					else
						Operands[1] = 0 -- Discard the attempt, we ran over the max amount of E2's.
					end
					return
				else
					print(E2.E2Func)
					return
				end
			end
		end
		Operands[1] = 0
	end

	myCPUExtension:InstructionFromLuaFunc("E2_COMPILE", 2, generateE2Handle, {"W1"}, {
		Version = 0.42,
		Description = "Compiles an E2 script from string pointer in Operand 2, then writes handle number or 0(failed) to Operand 1"
	})

	local function setTargetContext(VM, Operands)
		VM.TargetedE2Context = Operands[1]
	end

	myCPUExtension:InstructionFromLuaFunc("E2_OPEN_HANDLE", 1, setTargetContext, {}, {
		Version = 0.42,
		Description = "Sets currently targeted E2 handle for I/O"
	})

	local function readTargetContext(VM, Operands)
		Operands[1] = VM.TargetedE2Context or 0 -- 0 aka no context selected
	end

	myCPUExtension:InstructionFromLuaFunc("E2_READ_HANDLE", 1, readTargetContext, {"W1"}, {
		Version = 0.42,
		Description = "Reads the currently targeted E2 handle to Operand"
	})

	local function ExecuteE2(VM, Operands)
		if VM:checkE2ContextValid(Operands[2]) then
			local E2Context = VM.E2Contexts[Operands[2]]
			if not E2Context.E2Coroutine then
				E2Context.E2Coroutine = coroutine.create(function() E2Context.E2Func(E2Context.context) end)
			end
			local ret_value
			if E2Context.ZCPUFuncRequest then
				if E2Context.ZCPUFuncRequest.completed then
					ret_value = E2Context.ZCPUFuncRequest.ret_value
				else
					Operands[1] = 1
					E2Context.StatusMsg = "Attempted to continue execution without handling func request"
					return
				end
			end
			local success, msg = coroutine.resume(VM.E2Contexts[Operands[2]].E2Coroutine,ret_value)
			if not success then
				E2Context.StatusMsg = msg
				Operands[1] = 1
				return
			end
			if coroutine.status(E2Context.E2Coroutine) == "dead" then
				E2Context.E2Coroutine = false
			end
			Operands[1] = 0
			if istable(msg) then
				if msg.zcpu_info then
					E2Context.ZCPUFuncRequest = msg
				end
			end
			return
		end
		Operands[1] = 1
	end

	myCPUExtension:InstructionFromLuaFunc("E2_EXEC_HANDLE", 2, ExecuteE2, {"W1"}, {
		Version = 0.42,
		Description = "Runs E2 handle from Operand 2 and writes 0 to Operand 1 on success or writes 1 if an error occured in execution"
	})

	local function DestroyE2(VM, Operands)
		if VM:checkE2ContextValid(Operands[1]) then
			if VM.E2Contexts[VM.Operands[1]].E2Coroutine then
				coroutine.close(VM.E2Contexts[VM.Operands[1]].E2Coroutine)
			end
			VM.E2Contexts[VM.Operands[1]] = false
		end
	end
	
	myCPUExtension:InstructionFromLuaFunc("E2_FREE_HANDLE", 2, DestroyE2, {}, {
		Version = 0.42,
		Description = "Destroys the E2 with the specified handle, freeing that space for another E2"
	})

	local function writeNumberVariable(VM, Operands)
		if VM:checkE2ContextValid(VM.TargetedE2Context) then
			local str = VM:ReadString(Operands[1])
			if str then
				VM.E2Contexts[VM.TargetedE2Context].context.GlobalScope[str] = Operands[2]
			end
		end
	end

	myCPUExtension:InstructionFromLuaFunc("E2_WRITE_NUM", 2, writeNumberVariable, {}, {
		Version = 0.42,
		Description = "Writes number value to variable in targeted E2 handle using Operand 1 as Variable Name and Operand 2 as value"
	})

	local function readNumberVariable(VM, Operands)
		if VM:checkE2ContextValid(VM.TargetedE2Context) then
			local str = VM:ReadString(Operands[2])
			if str then
				if isnumber(VM.E2Contexts[VM.TargetedE2Context].context.GlobalScope[str]) then
					Operands[1] = VM.E2Contexts[VM.TargetedE2Context].context.GlobalScope[str]
				else
					Operands[1] = 0
				end
			end
		end
	end
	myCPUExtension:InstructionFromLuaFunc("E2_READ_NUM", 2, readNumberVariable, {"W1"}, {
		Version = 0.42,
		Description = "Reads number value from targeted E2 handle to Operand 1 using Operand 2 as Variable Name"
	})

	local function writeCPUWirelink(VM, Operands)
		if VM:checkE2ContextValid(VM.TargetedE2Context) then
			local str = VM:ReadString(Operands[1])
			if str then
				VM.E2Contexts[VM.TargetedE2Context].context.GlobalScope[str] = VM.Entity
			end
		end
	end
	myCPUExtension:InstructionFromLuaFunc("E2_LINK_MEM", 1, writeCPUWirelink, {}, {
		Version = 0.42,
		Description = "Writes CPU Entity to variable using Operand 1 as Variable Name, can be used to set a Wirelink variable to CPU."
	})

	include("e2_type_serialization.lua")(myCPUExtension)
	include("e2_cpu_interop.lua")(myCPUExtension)
end

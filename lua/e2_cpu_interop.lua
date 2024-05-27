-- Functionality for creating E2 functions from ZCPU
local _, E2TypeLib = include("e2_type_serialization.lua")

-- ripped from expression2/core/functions.lua, they don't export this anywhere :(
local function splitTypeFast(sig)
	local i, r, count, len = 1, {}, 0, #sig
	while i <= len do
		count = count + 1
		if string.sub(sig, i, i) == "x" then
			r[count] = string.sub(sig, i, i + 2)
			i = i + 3
		else
			r[count] = string.sub(sig, i, i)
			i = i + 1
		end
	end
	return r
end

local function generateE2FuncRequest(args, signature, ret_type, zcpu_info, VM)
	local buff = {}
	local argptrs = {}
	local extraptrs_required = 0
	local buffpos = 0
	local last_size = 0
	print("req args")
	PrintTable(args)
	local splitsig = splitTypeFast(signature)
	for ind, i in ipairs(args) do
		last_size = E2TypeLib.writeBuffer(buff, E2TypeLib.typeToBuffer(i, splitsig[ind], VM), buffpos + 1)
		PrintTable(buff)
		argptrs[ind] = buffpos
		if last_size > 1 then
			extraptrs_required = extraptrs_required + 1
		end
		buffpos = buffpos + last_size
	end
	print("Final buff")
	PrintTable(buff)
	local funcRequest = {
		zcpu_info=zcpu_info,
		argptrs=argptrs,
		ret_type=ret_type,
		extraptrs_required=extraptrs_required,
		signature=splitsig,
		buff=buff,
		size=buffpos -- does not take into account the number of ptrs required for vars
	}
	return funcRequest
	-- PrintTable(buff)
end

-- wrap into a coroutine yield and wait
local function createZCPUE2InteropFunc(e2context, zsig, VM)
	-- these will be on a per e2 basis
	local sig = table.concat(zsig.args)
	local function fn(...)
		-- generate a call request, convert all the arg types to a buffer first
		-- then we yield and the cpu can check what function we wanted to have called
		-- the cpu can get the size in bytes of the arguments, then if it decides it has enough space
		-- it can then use a special instruction to get the args to stack and call the function
		-- the function should be made to C-call standard with EAX as return value (or ptr)
		local args = table.Pack(...)
		-- 	PrintTable(...)
		print("zcpu function called by e2")
		local ret = coroutine.yield(generateE2FuncRequest(args[1], sig, zsig.ret_type, zsig.ZCPU, VM)) -- wait for zcpu to let us know we're ready
		print("ZCPU call completed, ret value: ",ret)
		return ret
	end
	return E2Lib.Lambda.new(sig, zsig.ret_type, fn)
end

-- theoretical structure for a function sig
-- [1] flags: 1 = E2, 2 = Far call(zcpu)
-- [2] num of args
-- [3] offset ptr to arg type[] or 0 if none
-- [4] return type or 0 if none
-- [5 ZCPU] CS [5 E2] func name string
-- [6 ZCPU] IP
-- array of arg types

local function getFuncSignatureBufferSize(VM, address)
	local argnum = VM:ReadCell(address+1)
	local argptr = VM:ReadCell(address+2)
	if argnum > 0 and argptr > 0 then
		return argptr+argnum
	end
		-- we have no args so we gotta check if this is an E2 func, else return metadata size
			local flags = VM:ReadCell(address)
			if bit.band(flags,1) ~= 0 then
				-- we are an E2 function, get string length starting from +4
				local str = VM:ReadString(address+4)
				return (#str)+5 -- strlen+1 because vm:readstring eschews null terminator
				else
				return 6 -- size of ZCPU func meta with no args
			end
end

local function funcSignatureToBuffer(funcSignature,VM)
	print("func sig time!!")
	local buff = {}
	local flags = 0
	if not funcSignature.ZCPU then
		flags = flags + 1 -- other flag parsing later, just mark it as E2
		local nameString = table.Pack(string.byte(funcSignature.E2Name))
		table.insert(nameString,0)
		buff[3] = E2TypeLib.writeBuffer(buff,nameString,5)+4 -- ptr = metadata + strlen
	else
		flags = funcSignature.ZCPU.flags
		buff[3] = 6
		buff[5] = funcSignature.ZCPU.CS
		buff[6] = funcSignature.ZCPU.IP
	end
	buff[1] = flags
	local args = splitTypeFast(funcSignature.sig.arg_sig)
	PrintTable(args)
	buff[2] = #args
	if funcSignature.sig.ret_type then
		buff[4] = VM.E2TypeInfo.TypeNames[funcSignature.ret_type].id or 0
	else
		buff[4] = 0
	end
	local wptr = buff[3]
	if #args == 0 then
		buff[3] = 0
	else
		for ind,i in ipairs(args) do
			buff[wptr+ind] = VM.E2TypeInfo.TypeNames[i].id
		end	
	end
	PrintTable(buff)
	return buff
end

local function bufferToFuncSignature(buff,VM,E2Context)
	local sigInfo = {args={}}
	local flags = buff[1]
	local isE2 = bit.band(flags,1)
	if isE2 ~= 0 then
		-- use this e2 name as a lookup for where to get func
		-- transfer of funcs between 2 e2s will not work however
		-- because it has to exist in the e2
		-- side note, we do this because we cannot serialize real lua functions safely
		-- because if it's a number, it can be altered easily to replace it with another.
		sigInfo.E2Name = E2TypeLib.readStringFromNumberBuffer(buff,5)
	else
		sigInfo.ZCPU = {
			CS = buff[5],
			IP = buff[6],
			flags=flags
		}
	end
	if buff[4] > 0 then 
		sigInfo.ret_type = VM.E2TypeInfo.TypeIDs[buff[4]].name
	end
	if buff[2] > 0 and buff[3] > 0 then
		local argbuffer = {}
		E2TypeLib.copyBufferSection(argbuffer,buff,1,buff[3]+1,buff[2])
		PrintTable(buff)
		print(buff[3])
		PrintTable(argbuffer)
		for ind,i in ipairs(argbuffer) do
			print("tick")
			sigInfo.args[ind] = VM.E2TypeInfo.TypeIDs[i].name
		end
	end
	if sigInfo.E2Name then
		-- lookup if its an e2 userfunc first
		local userfunc = E2Context.context.funcs[sigInfo.E2Name]
		if userfunc then
			-- needs to be in format E2Name(nnn)
			return E2Lib.Lambda.new(table.concat(sigInfo.args),sigInfo.ret_type,userfunc)
		end
		-- check if it's a lambda var
		local lambdavar = E2Context.context.GlobalScope[sigInfo.E2Name]
		if lambdavar then
			return E2Lib.Lambda.new(table.concat(sigInfo.args),sigInfo.ret_type,lambdavar.fn)
		end
		-- check if it's a global e2 function
		local e2globalfunc = wire_expression2_funcs[sigInfo.E2Name]
		if e2globalfunc then
			return E2Lib.Lambda.new(table.concat(sigInfo.args),sigInfo.ret_type,e2globalfunc.fn)
		end
		-- generate a function that will cause an error on call
			return E2Lib.Lambda.new(nil,nil,
			function(self)
				self:ForceThrow("Couldn't find E2 func from name "..tostring(sigInfo.E2Name).." while deserializing func signature")
				end
			)
	end
	PrintTable(sigInfo)
	return createZCPUE2InteropFunc(E2Context,sigInfo, VM)
end

local function ex(myCPUExtension)
	local function readE2Lambda(VM, Operands)
		if VM:checkE2ContextValid(VM.TargetedE2Context) then
			local str = VM:ReadString(Operands[2])
			if str then
				local fnsig = {E2Name=str} -- strip 0 from name
				print(str)
				fnsig.sig = VM.E2Contexts[VM.TargetedE2Context].context.GlobalScope[str]
				if fnsig.sig then
					fnsig.ZCPU = fnsig.sig.ZCPU
					local sig = funcSignatureToBuffer(fnsig, VM)
					for ind,i in ipairs(sig) do
						VM:WriteCell(Operands[1]+ind-1,i)
					end
				end
			end
		end
	end
	myCPUExtension:InstructionFromLuaFunc("E2_READ_FUNCTION_VAR", 2, readE2Lambda, {"W1"}, {
		Version = 0.42,
		Description = "Read the signature of an E2 Function with Operand 2 as name and Operand 1 as ptr to where to store data"
	})
	-- one for writing a lambda var, one for overwriting a users global function
	local function writeZCPULinkedE2Lambda(VM, Operands)
		if VM:checkE2ContextValid(VM.TargetedE2Context) then
			local str = VM:ReadString(Operands[1])
			if str then
				local size = getFuncSignatureBufferSize(VM,Operands[2])
				print(size)
				local buffer = {}
				local address = Operands[2]
				for i=0,size-1,1 do
					table.insert(buffer,VM:ReadCell(address+i))
				end
				local E2Context = VM.E2Contexts[VM.TargetedE2Context]
				local sig = bufferToFuncSignature(buffer,VM,E2Context)
				VM.E2Contexts[VM.TargetedE2Context].context.GlobalScope[str] = sig
			end
		end
	end
	myCPUExtension:InstructionFromLuaFunc("E2_WRITE_FUNCTION_VAR", 2, writeZCPULinkedE2Lambda, {}, {
		Version = 0.42,
		Description = "Write a function to function variable with Operand 1 as the variable name and Operand 2 is a ptr to a function signature."
	})
	local function linkZCPUFunction(VM, Operands)
		VM.E2Contexts[VM.TargetedE2Context].context.funcs["first()"] = function() print("hook succ") end
	end
	myCPUExtension:InstructionFromLuaFunc("E2_LINK_GLOBAL_FUNCTION", 2, linkZCPUFunction, {}, {
		Version = 0.42,
		Description = "Write a function to replace a user-written function in the script.\nOnly works without the @strict directive\nOperand 1 is the function name to replace and Operand 2 is a ptr to a function signature."
	})
	local function handleZCPUFuncRequest(VM, Operands)
		-- push args to stack, put num of args in ECX, hook return, and then jump to function
		if VM:checkE2ContextValid(Operands[1]) then
			local E2Context = VM.E2Contexts[Operands[1]]
			local FuncRequest = E2Context.ZCPUFuncRequest
			if FuncRequest and not FuncRequest.completed then
				PrintTable(FuncRequest)
				local c_call_args = {} -- needs to be values, or ptrs to values, make sure that ptrs to values have ESP added to them
				for ind,i in ipairs(FuncRequest.argptrs) do
					local varsize = (FuncRequest.argptrs[ind+1] or FuncRequest.size)-i
					-- get diff between ptr 1 and next ptr or known end of buffer
					if varsize > 1 then
						local buff = {}
						E2TypeLib.copyBufferSection(buff,FuncRequest.buff,0,i,varsize+1)
						print("Copying large var to buffer")
						PrintTable(buff)
						-- we need to actually reverse this cause push will push it onto the stack and then stack decrements
						-- so if we were to push 600,200,100(left to right) trying to read directly from the ptr might end up with
						-- 100,200,600
						for i=varsize,1,-1 do
							VM:Push(buff[i])
						end
						-- VM.ESP is the ptr to the current free index on stack, so the last element altered is ESP+1
						table.insert(c_call_args,VM.ESP+1) -- push ptr to first element of var on stack
					else
						table.insert(c_call_args,FuncRequest.buff[i+1])
					end
				end
				print("C Call generation done, here are the args")
				PrintTable(c_call_args)
				-- HLZasm funcs appear to need these reversed too, and don't use C call right to left args like I initially thought.
				for i=#c_call_args,1,-1 do
					VM:Push(c_call_args[i])
				end
				-- Allows for semi-variadic handling of args where ECX is the number of args, for funcs like printf(char*,...) in C
				-- This IS part of the C Calling from HLZasm, but I've never seen a user made func that used it.
				VM.ECX = #c_call_args
				-- Generate a return to give value of EAX
				local function extractReturnValue(VM)
					FuncRequest.ret_value = E2TypeLib.readTypeFromMemory(VM,VM.EAX,FuncRequest.ret_type)
					FuncRequest.completed = true
				end
				VM:GenerateHookedReturn(extractReturnValue)
				-- check if far call or near call
				if bit.band(FuncRequest.zcpu_info.flags,2) ~= 0 then
					-- Far call, use the CS provided by func signature
					VM:Push(VM.CS)
					VM:Push(VM.IP)
					VM:Jump(FuncRequest.zcpu_info.IP,VM.CS)
				else
					-- Near call, use current CS for jump
					VM:Push(VM.IP)
					VM:Jump(FuncRequest.zcpu_info.IP,VM.CS)
				end
			end
		end
	end
	myCPUExtension:InstructionFromLuaFunc("E2_HANDLE_FUNC_REQUEST", 1, handleZCPUFuncRequest, {"CB"}, {
		Version = 0.42,
		Description = "Places arguments in the stack, the number of arguments in ECX, and then calls the requested CS and IP from the E2 handle in Operand 1. Upon return or jumping back will save the value in EAX as the return value to E2, the E2 will continue from the point it made the call on next execution."
	})
end
return ex

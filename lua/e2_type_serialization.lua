-- * Functionality for handling transfer and management of E2 variables

-- size lookup for lua / gmod types that have a type that isn't just a table internally
	local primitiveSizeLookup = {
		-- lowercase names are identifiable via type()
		-- uppercase numbers cannot be identified by type
		n = 1, -- number / normal
		xwl = 1, -- WIRELINK (this and ent can be expressed as just the entity's id)
		e = 1, -- entity
		xv2 = 2, -- VECTOR2
		c = 2, -- COMPLEX
		v = 3, -- vector3
		a = 3, -- angle
		q = 4, -- QUATERNION
		xv4 = 4, -- VECTOR4
		xm2 = 4, -- MATRIX2
		m = 9, -- MATRIX
		xm4 = 16 -- MATRIX4
	}

	local function identifyType(var)
		local t = type(var) -- Check if it's a primitive type or not
		if not t then
			return ""
		end
		if t == "table" then -- var is a table, will need probing to discover its true type
			if var.ntypes or var.stypes then
				return "t" -- var is a typed E2 table, it has a list of type indices
			end
			return "r" -- Type unknown, possibly an array because it had no identifiable metadata
		end
		if t == "number" then
			return "n" -- normal
		end
		local wire_type = wire_expression_types[t:upper()]
		if wire_type then
			return wire_type[1]
		end
		return "" -- empty type name, translates to type ID 0
	end

-- it is significantly easier to reuse the table serialization for this
	-- plus the table serialization and deserialization seems to be obscenely fast
	-- since the cpu can read and write a small table at 2.1MHz about 262494.0135885443 times per second
	local function E2ArraytoTable(array)
		local e2table = E2Lib.newE2Table()
		for ind,i in ipairs(array) do 
			e2table.n[ind] = i
			e2table.ntypes[ind] = identifyType(i)
			e2table.size = e2table.size + 1
		end
		PrintTable(array)
		return e2table
	end
	local function sumTypeArray(VM, arr)
		local sum = 0
		local singleCellTypes = 0
		for ind, i in ipairs(arr) do
			local primitive = primitiveSizeLookup[i]
			if primitive then
				if primitive == 1 then
					singleCellTypes = singleCellTypes + 1
				end
				sum = sum + primitive
			else
				local E2TypeDefault = VM.E2TypeInfo.TypeNames[i][2]
				if type(E2TypeDefault) == "table" then
					sum = sum + #E2TypeDefault -- If type has a default value to initialize to, uses that default value's size
				end
			end
		end
		return sum, singleCellTypes
	end
	local function sumTypeDict(VM, dict)
		-- just convert the typedict to a typearray and use sumTypeArray after we sum the size of the keys
		local typearr = {}
		local stringSizes = 0
		for k, v in pairs(dict) do
			stringSizes = stringSizes + (#k + 1) -- + 1 because it needs a null terminator after each
			table.insert(typearr, v)
		end
		return stringSizes, sumTypeArray(VM, typearr)
	end
	local function getE2TableDataSize(VM, table)
		local metadatasize = 6 -- metadata amount for storing the table's offsets, unchanging
		local numindex_size = (#table.ntypes) * 2 -- size required for storing the numeric indexes, without their values
		local numindex_value_size, numindex_single_cell_types = sumTypeArray(VM, table.ntypes)
		numindex_value_size = numindex_value_size - numindex_single_cell_types
		local strindex_size = 0 -- requires 3 bytes per string index because it has ptr to name as well as type and value arr
		for _, _ in pairs(table.stypes) do
			strindex_size = strindex_size + 3
		end
		local str_name_size, strindex_value_size, strindex_single_cell_types = sumTypeDict(VM, table.stypes)
		strindex_value_size = strindex_value_size - strindex_single_cell_types
		local sum = metadatasize + numindex_size + numindex_value_size + strindex_size + str_name_size + strindex_value_size
		return sum, numindex_size, numindex_value_size, strindex_size, strindex_value_size, str_name_size
	end
	local function numberToBuffer(var)
		return {var}
	end
	local function entToBuffer(var)
		return {var:EntIndex()}
	end
	local function stringToBuffer(var)
		local size = #var
		local buff = {}
		if size == 0 then
			return {0}
		end
		-- string.byte will throw if over 8000, clamps total string size in memory to be 8000
		-- including the null terminator
		buff = table.Pack(string.byte(var, 1, math.Clamp(size, 1, 7999)))
		table.insert(buff, 0) -- add null terminator
		return buff
	end
	local function rawNumberArrayToBuffer(var)
		-- this type is just an indexed list of numbers so it's already a buffer pretty much
		return var
	end

	local function vector3ToBuffer(var)
		return {var.x, var.y, var.z} -- both angles and vectors use these keys so this should work on both
	end
	local typeConversionLookup = {
		n = numberToBuffer, -- normal / number
		e = entToBuffer, -- entity
		s = stringToBuffer, -- string
		xwl = entToBuffer, -- WIRELINK
		xv2 = rawNumberArrayToBuffer, -- VECTOR2
		c = rawNumberArrayToBuffer, -- COMPLEX
		q = rawNumberArrayToBuffer, -- QUATERNION
		xv4 = rawNumberArrayToBuffer, -- VECTOR4
		xm2 = rawNumberArrayToBuffer, -- MATRIX2
		m = rawNumberArrayToBuffer, -- MATRIX
		xm4 = rawNumberArrayToBuffer, -- MATRIX4
		a = vector3ToBuffer, -- angle
		v = vector3ToBuffer -- vector3
	}
	local function typeToBuffer(var, type, VM)
		local typeConverter = typeConversionLookup[type]
		if typeConverter then
			return typeConverter(var,VM)
		else
			return {0}
		end
	end

	local function writeBuffer(dest, buffer, startind)
		if not startind then
			startind = 1
		end
		local written_bytes = 0
		for ind, i in ipairs(buffer) do
			dest[startind + (ind - 1)] = i
			written_bytes = written_bytes + 1
		end
		return written_bytes
	end
	local function copyBufferSection(dest, src, dest_start, src_start, max_write)
		for ind = 0, max_write - 1, 1 do
			dest[dest_start + ind] = src[src_start + ind]
		end
	end

	local function bufferToNumber(buff)
		return buff[1]
	end
	local function bufferToEnt(buff)
		return Entity(buff[1])
	end
	local function bufferToString(buff)
		-- strip 0 from null terminated string if it exists
		if (buff[#buff] == 0) then
			buff[#buff] = nil
		end
		return string.char(buff)
	end
	local function bufferToRawNumberArray(buff)
		-- a buffer is just a raw number array but this is useful for the lookup
		return buff
	end
	local function bufferToAngle(buff)
		return Angle(buff[1], buff[2], buff[3])
	end
	local function bufferToVector3(buff)
		return Vector(buff[1], buff[2], buff[3])
	end
	local bufferToTypeLookup = {
		n = bufferToNumber, -- normal / number
		e = bufferToEnt, -- entity
		s = bufferToString, -- string
		xwl = bufferToEnt, -- WIRELINK
		xv2 = bufferToRawNumberArray, -- VECTOR2
		c = bufferToRawNumberArray, -- COMPLEX
		q = bufferToRawNumberArray, -- QUATERNION
		xv4 = bufferToRawNumberArray, -- VECTOR4
		xm2 = bufferToRawNumberArray, -- MATRIX2
		m = bufferToRawNumberArray, -- MATRIX
		xm4 = bufferToRawNumberArray, -- MATRIX4
		a = bufferToAngle, -- angle
		v = bufferToVector3 -- vector3
	}
	local function bufferToType(VM, buff, type)
		local typeConversion = bufferToTypeLookup[type]
		if typeConversion then
			return typeConversion(buff,VM)
		end
		return nil
	end

	local function readStringFromNumberBuffer(buff, startind)
		local str = {}
		local ind = startind or 1
		while (buff[ind] ~= 0) do
			if buff[ind] == nil then
				break
			end
			table.insert(str, buff[ind])
			ind = ind + 1
		end
		return string.char(unpack(str))
	end
	-- get size of an e2 table in buffer form
	local function getE2TableBufferSize(VM, ptr)
		-- * Strategy:
		-- * 1: Search for the last pointer in ntypes or stypes, find the largest pointer and add type size to it
		-- * since s vars usually come last, we should check that section first for the last ptr in its set
		-- * 2: If there are no ptr types in s types we have to check n types
		-- * if there are no ptr types in n types but we have ANY s vars, just return the svartypesptr + svars
		-- * 3: At this point, if there are no s vars, we can just sum the size of the metadata (6 bytes) + number of n vars * 2
		local ntypesptr = VM:ReadCell(ptr + 3)
		local stypesptr = VM:ReadCell(ptr + 5)
		local svars, nvars = 0, 0
		local lastptr = 0
		local lastptrsize = 0
		if stypesptr > 0 then
			svars = math.min(VM:ReadCell(ptr + 1), 65536)
			local svarptr = VM:ReadCell(ptr + 4)
			if svarptr > 0 then
				for i = 0, svars - 1, 1 do
					local type = VM:ReadCell(ptr + stypesptr + i)
					type = VM.E2TypeInfo.TypeIDs[type]
					if type then
						local primitive = primitiveSizeLookup[type.name] or 0
						if primitive > 1 then
							local sptr = VM:ReadCell(ptr + svarptr + i)
							if sptr > lastptr then
								lastptr = sptr
								lastptrsize = primitive
							end
						end
					end
				end
			end
		end
		if lastptr ~= 0 then
			return lastptr + lastptrsize
		end
		-- So there were no pointers in the svars, or we had no svars, so we have to now check nvars
		if ntypesptr > 0 then
			nvars = math.min(VM:ReadCell(ptr + 0), 65536)
			local nvarptr = VM:ReadCell(ptr + 2)
			if nvarptr > 0 then
				for i = 0, nvars - 1, 1 do
					local type = VM:ReadCell(ptr + ntypesptr + i)
					type = VM.E2TypeInfo.TypeIDs[type]
					if type then
						local primitive = primitiveSizeLookup[type.name] or 0
						if primitive > 1 then
							local sptr = VM:ReadCell(ptr + nvarptr + i)
							if sptr > lastptr then
								lastptr = sptr
								lastptrsize = primitive
							end
						end
					end
				end
			end
		end
		if lastptr ~= 0 then
			return lastptr + lastptrsize
		end
		-- No pointers in the nvars, or we had no nvars, stypesptr is thus the last 
		if svars > 0 then
			lastptr = stypesptr
			lastptrsize = svars - 1
		end
		if lastptr ~= 0 then
			return lastptr + lastptrsize
		end
		-- Every other trick has failed, we have no n ptrs, no s vars, the table must only have 1 byte n vars
		-- so its size can be estimated as such
		return 6 + nvars * 2
	end
	local function E2TabletoBuffer(e2table,VM)
		-- TODO: Optimize value/type writing, if we know the number of values and they're all forced to 1 byte
		-- TODO: then calculating an offset between the two should be simple, so we don't have to run through both types
		-- TODO: and values separately when we're already indexing types to check real var sizes, goes for both S values and N values
		local buff = {}
		buff[1] = #e2table.n -- number of n vars
		buff[2] = e2table.size - buff[1] -- size is #nvars + #svars so we can reverse this to avoid having to count svars with a loop

		-- skipping writing ptrs to sections until we actually know where they'll be
		-- data starts at buff[7]
		local write_ptr = 7
		local deferred_writes = {} -- ptr = index to main write buffer where the position of buffer needs to be stored on write
		if buff[2] > 0 then
			-- there are string keys, we need to defer writes of keynames until after nvars and ntypes
			-- but store the pointers to the keys here
			for k, _ in pairs(e2table.s) do
				local keybuff = typeToBuffer(k, "s")
				table.insert(deferred_writes, {
					ptr = write_ptr,
					buff = keybuff
				})
				write_ptr = write_ptr + 1
			end
		end
		if buff[1] > 0 then
			buff[3] = write_ptr -- pointer to start of n
			for ind, i in ipairs(e2table.n) do
				local typebuff = typeToBuffer(i, e2table.ntypes[ind])
				if #typebuff > 1 then
					-- defer writing this until we're at the additional section
					table.insert(deferred_writes, {
						ptr = write_ptr,
						buff = typebuff
					})
				else
					buff[write_ptr] = typebuff[1]
				end
				write_ptr = write_ptr + 1
			end
			buff[4] = write_ptr
			for _, i in ipairs(e2table.ntypes) do
				-- write ntypes as numbers to buffer
				if VM.E2TypeInfo.TypeNames[i] then
					buff[write_ptr] = VM.E2TypeInfo.TypeNames[i].id
				else
					buff[write_ptr] = 0 -- type isn't in typenames so 0 to indicate
				end
				write_ptr = write_ptr + 1
			end
		else
			-- no n or ntypes so ptr to 0 indicates nonexistence.
			buff[3] = 0
			buff[4] = 0
		end
		-- we can now write the key strings to buffer and populate the char** waiting at buff[7]
		if buff[2] > 0 then
			for i = 1, buff[2], 1 do
				local write = table.remove(deferred_writes, 1)
				-- convert the write_ptr of a deferred write to a 0 indexing system
				buff[write.ptr] = write_ptr - 1
				write_ptr = write_ptr + writeBuffer(buff, write.buff, write_ptr)
			end
			-- write s values
			buff[5] = write_ptr
			for k, i in pairs(e2table.s) do
				local typebuff = typeToBuffer(i, e2table.stypes[k])
				if #typebuff > 1 then
					-- defer writing this until we're at the additional section
					table.insert(deferred_writes, {
						ptr = write_ptr,
						buff = typebuff
					})
				else
					buff[write_ptr] = typebuff[1]
				end
				write_ptr = write_ptr + 1
			end
			-- write s types
			buff[6] = write_ptr
			for _, i in pairs(e2table.stypes) do
				if VM.E2TypeInfo.TypeNames[i] then
					buff[write_ptr] = VM.E2TypeInfo.TypeNames[i].id
				else
					buff[write_ptr] = 0 -- type isn't in typenames so 0 to indicate
				end
				write_ptr = write_ptr + 1
			end
		else
			-- no s or stypes so ptr to 0 indicates nonexistence
			buff[5] = 0
			buff[6] = 0
		end
		-- write remaining deferred writes to the additional section
		for ind, i in ipairs(deferred_writes) do
			-- convert the write_ptr of a deferred write to a 0 indexing system
			buff[i.ptr] = write_ptr - 1
			write_ptr = write_ptr + writeBuffer(buff, i.buff, write_ptr)
		end
		return buff
	end

	local function buffertoE2Table(buff,VM)
		local e2table = E2Lib.newE2Table()
		-- Parse n values and ntypes
		if buff[1] > 0 then
			local nptr = buff[3]
			local ntypeptr = buff[4]
			for ind = 0, buff[1] - 1, 1 do
				local type = VM.E2TypeInfo.TypeIDs[buff[ntypeptr + ind]]
				local value
				if type then
					local size = primitiveSizeLookup[type.name]
					local tempBuffer = {}
					local dptr = nptr + ind
					if size then
						if size > 1 then
							dptr = buff[nptr + ind]
						end
						copyBufferSection(tempBuffer, buff, 1, dptr, size)
						value = bufferToType(VM, tempBuffer, type.name)
					else
						if type.name == "s" then
							local dptr = buff[nptr + ind]
							if dptr > 0 then
								value = readStringFromNumberBuffer(buff, dptr)
							end
						end
					end
					if value ~= nil then
						table.insert(e2table.n, value)
						table.insert(e2table.ntypes, type.name)
						e2table.size = e2table.size + 1
					end
				end
			end
		end
		-- parse s values and stypes
		if buff[2] > 0 then
			local sptr = buff[5]
			local stypeptr = buff[6]
			for ind = 0, buff[2] - 1, 1 do
				local key = readStringFromNumberBuffer(buff, buff[7 + ind] + 1) -- convert the ptr from a 0 index to a 1 index
				local type = VM.E2TypeInfo.TypeIDs[buff[stypeptr + ind]]
				local value
				if type then
					local size = primitiveSizeLookup[type.name]
					local tempBuffer = {}
					local dptr = sptr + ind
					if size then
						if size > 1 then
							dptr = buff[sptr + ind]
						end
						copyBufferSection(tempBuffer, buff, 1, dptr, size)
						value = bufferToType(VM, tempBuffer, type.name)
					else
						if type.name == "s" then
							local dptr = buff[sptr + ind]
							if dptr > 0 then
								value = readStringFromNumberBuffer(buff, dptr)
							end
						end
					end
					if value ~= nil then
						e2table.s[key] = value
						e2table.stypes[key] = type.name
						e2table.size = e2table.size + 1
					end
				end
			end
		end
		return e2table
	end

	local function E2ArrayToBuffer(e2array,VM)
		return E2TabletoBuffer(VM, E2ArraytoTable(e2array))
	end
	local function bufferToE2Array(buff,VM)
		return buffertoE2Table(buff,VM).n
	end
	-- TODO: Store this type conversion stuff on VM so it can be added to by other files
	typeConversionLookup["t"] = E2TabletoBuffer
	bufferToTypeLookup["t"] = buffertoE2Table
	typeConversionLookup["r"] = E2ArrayToBuffer
	bufferToTypeLookup["r"] = bufferToE2Array
	-- Funcs for getting size of type from memory, given pointer and VM
	local typeMemorySizeFuncs = {
		t=getE2TableBufferSize,
		r=getE2TableBufferSize,
	}

	local function readTypeFromMemory(VM,ptr,type)
		-- get size of type, first check if it's primitive
		local primitive = primitiveSizeLookup[type]
		local buff = {}
		if primitive then
			if primitive > 1 then
				for i=ptr,ptr+primitive,1 do
					table.insert(buff,VM:ReadCell(i))
				end
			else
				-- ptr was not in fact a ptr but just the value
				return bufferToType({ptr},type)
			end
		else
			local sizeFunc = typeMemorySizeFuncs[type]
			if sizeFunc then
				local size = sizeFunc(VM,ptr)
				for i=ptr,ptr+size,1 do
					table.insert(buff,VM:ReadCell(i))
				end
			end
		end
		return bufferToType(buff,type)
	end

local function ext(myCPUExtension)
	-- Returns the size of a variable type, or -1 if it cannot be converted
	local function readVariableSize(VM, Operands)
		if VM:checkE2ContextValid(VM.TargetedE2Context) then
			local str = VM:ReadString(Operands[2])
			if str then
				local var = VM.E2Contexts[VM.TargetedE2Context].context.GlobalScope[str]
				if not var then
					Operands[1] = -1 -- couldn't fetch var
				end
				local e2type = identifyType(var)
				local primitive = primitiveSizeLookup[e2type]
				if primitive then
					Operands[1] = primitive
					return
				end
				if e2type == "t" then
					Operands[1] = getE2TableDataSize(VM, var)
					return
				end
				-- type not able to be read as it either couldn't be read or has no method to gauge its size, return 0 to indicate
				Operands[1] = 0
				return
			end
		end
		Operands[1] = -2 -- context invalid
	end
	myCPUExtension:InstructionFromLuaFunc("E2_GET_SIZE", 2, readVariableSize, {"W1"}, {
		Version = 0.42,
		Description = "Get size of variable in open e2 handle by name in operand 2 and put its size in operand 1"
	})
	local function readVariableType(VM, Operands)
		if VM:checkE2ContextValid(VM.TargetedE2Context) then
			local str = VM:ReadString(Operands[2])
			if str then
				local var = VM.E2Contexts[VM.TargetedE2Context].context.GlobalScope[str]
				if not var then
					Operands[1] = -1 -- couldn't fetch var
				end
				local e2type = identifyType(var)
				local typeID = VM.E2TypeInfo.TypeNames[e2type].id
				Operands[1] = typeID
				return
			end
		end
		Operands[1] = -2 -- context invalid
	end
	myCPUExtension:InstructionFromLuaFunc("E2_GET_TYPE", 2, readVariableType, {"W1"}, {
		Version = 0.42,
		Description = "Get type of variable using name in Operand 2, writes type enum into Operand 1 or 0 if failed"
	})
	local function readE2Table(VM, Operands)
		if VM:checkE2ContextValid(VM.TargetedE2Context) then
			local str = VM:ReadString(Operands[2])
			if str then
				local buff = E2TabletoBuffer(VM, VM.E2Contexts[VM.TargetedE2Context].context.GlobalScope[str])
				local address = Operands[1]
				for ind, i in ipairs(buff) do
					-- If the ZVM errors on write it'll return false
					-- However, it returns nothing / nil on success.
					if VM:WriteCell(address + ind - 1, i) ~= nil then
						break
					end
				end
			end
		end
	end
	myCPUExtension:InstructionFromLuaFunc("E2_READ_TABLE", 2, readE2Table, {}, {
		Version = 0.42,
		Description = "Read a table from E2 by name in Operand 2 to Memory Address in Operand 1"
	})
	local function writeE2Table(VM, Operands)
		if VM:checkE2ContextValid(VM.TargetedE2Context) then
			local str = VM:ReadString(Operands[1])
			if str then
				local address = Operands[2]
				local size = getE2TableBufferSize(VM, address)
				local buff = {}
				for i = 0, size - 1, 1 do
					table.insert(buff, VM:ReadCell(address + i))
				end
				local e2table = buffertoE2Table(buff,VM)
				VM.E2Contexts[VM.TargetedE2Context].context.GlobalScope[str] = e2table
			end
		end
	end
	myCPUExtension:InstructionFromLuaFunc("E2_WRITE_TABLE", 2, writeE2Table, {}, {
		Version = 0.42,
		Description = "Write a table to E2 by name in Operand 1 from Memory Address in Operand 2"
	})


	local function readE2Array(VM, Operands)
		if VM:checkE2ContextValid(VM.TargetedE2Context) then
			local str = VM:ReadString(Operands[2])
			if str then
				local e2array = VM.E2Contexts[VM.TargetedE2Context].context.GlobalScope[str]
				local type = identifyType(e2array)
				if type ~= "r" then
					return
				end
				local buff = E2ArrayToBuffer(e2array,VM)
				local address = Operands[1]
				for ind, i in ipairs(buff) do
					-- If the ZVM errors on write it'll return false
					-- However, it returns nothing / nil on success.
					if VM:WriteCell(address + ind - 1, i) ~= nil then
						break
					end
				end
			end
		end
	end
	myCPUExtension:InstructionFromLuaFunc("E2_READ_ARRAY", 2, readE2Array, {}, {
		Version = 0.42,
		Description = "Read an array from E2 by name in Operand 2 to Memory Address in Operand 1, note that this may lose information about types"
	})
	local function writeE2Array(VM, Operands)
		if VM:checkE2ContextValid(VM.TargetedE2Context) then
			local str = VM:ReadString(Operands[1])
			if str then
				local address = Operands[2]
				local size = getE2TableBufferSize(VM, address)
				local buff = {}
				for i = 0, size - 1, 1 do
					table.insert(buff, VM:ReadCell(address + i))
				end
				VM.E2Contexts[VM.TargetedE2Context].context.GlobalScope[str] = bufferToE2Array(buff,VM)
			end
		end
	end
	myCPUExtension:InstructionFromLuaFunc("E2_WRITE_ARRAY", 2, writeE2Array, {}, {
		Version = 0.42,
		Description = "Write an array to E2 by name in Operand 1 from Memory Address in Operand 2"
	})


end

return ext,{
	primitiveSizeLookup = primitiveSizeLookup,
	identifyType = identifyType,
	E2ArraytoTable = E2ArraytoTable,
	sumTypeArray = sumTypeArray,
	sumTypeDict = sumTypeDict,
	getE2TableDataSize = getE2TableDataSize,
	typeToBuffer = typeToBuffer,
	writeBuffer = writeBuffer,
	copyBufferSection = copyBufferSection,
	bufferToType = bufferToType,
	readStringFromNumberBuffer = readStringFromNumberBuffer,
	getE2TableBufferSize = getE2TableBufferSize,
	E2TabletoBuffer = E2TabletoBuffer,
	buffertoE2Table = buffertoE2Table,
	readTypeFromMemory = readTypeFromMemory
}

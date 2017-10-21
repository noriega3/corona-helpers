-- @usage local utility = require("utility.utility")
local _assert 		= assert
local _pairs 		= pairs
local _tonumber 	= tonumber
local _type 		= type
local _stringgsub	= string.gsub
local _stringsub	= string.sub
local _stringlen	= string.len
local _stringfind	= string.find
local _stringformat	= string.format
local _stringbyte	= string.byte
local _stringupper	= string.upper
local _mathfloor	= math.floor
local _mathrandom	= math.random
local _getmetatable	= getmetatable
local _setmetatable	= setmetatable
local _tablesort	= table.sort
local _tableinsert	= table.insert
local _ipairs		= ipairs
local _ioopen		= io.open
local utility		= {}
local lfs 			= require("lfs")
local composer 		= require("composer")
local _system		= system
local _timerPerformWithDelay	= timer.performWithDelay
local _composerGetSceneName 	= composer.getSceneName
local _composerGetVariable 		= composer.getVariable
local platform 			= _system.getInfo("platform")
utility.platform 		= platform
utility.isAndroid 		= platform == "android"
utility.isWindows 		= platform == "win32"
utility.isIOS 			= platform == "ios"
utility.isMacOS 		= platform == "macos"
utility.isSimulator 	= _system.getInfo("environment") == "simulator"

--==================================================
--Math related
--==================================================

--- Rounds to the idp or 0 if no idp passed
-- @param num the number
-- @param[opt] idp how many decimal points (default: 0)
-- @usage utility.round(46,2) -- 50.00
function utility.round(num, idp)
	if(not _tonumber(num)) then return error('Cannot round a nil number') end
    local mult = 10^(idp or 0)
    return _mathfloor(num * mult + 0.5) / mult
end

--==================================================
--String Manipulation
--==================================================

--- Changes the first character of the string to uppercase
-- @param strToChange the string to change
-- @treturn string
-- @usage utility.firstToUpper("hellolowercase") -- Hellolowercase
function utility.firstToUpper(strToChange)
    return (strToChange:_stringgsub("^%l", _stringupper))
end

--- Urlencodes the string
-- @param str the string to change
function utility.escape(str)
    if(str) then
        str = _stringgsub(str, "\n", "\r\n")
        str = _stringgsub(str, "([^%w ])",
        function (c) return _stringformat ("%%%02X", _stringbyte(c)) end)
        str = _stringgsub(str, " ", "+")
    end
    return str
end

--- Adds commas to a number
-- @param amount the number that you want commas added to
function utility.commaValue(amount)
    if (amount < 1000) then return amount end

    local k
    while true do
        amount, k = _stringgsub(amount, "^(-?%d+)(%d%d%d)", '%1,%2')
        if (k==0) then
            break
        end
    end
    return amount
end

--- Adds a decimal to the number amount fed in
-- @param amount the number to pass in
-- @param[opt] decimal how many many decimal points, default: 2
-- @return the amount with decimal points and commas for every thousands
-- @usage utility.numberFormat("30000", 4) -- 30,000.0000
function utility.numberFormat(amount, decimal, noSpace)

    if(not _tonumber(amount)) then return amount end
    amount = _tonumber(amount)
    decimal = decimal or 2  -- default 2 decimal places
    local unit
    local famount
    local conversions = {
        --{greaterthan, divide by, String to go at the end, max decimal places}
        [1] = {1000000000, 1000000000, "B", 2},
        [2] = {100000000, 1000000, "M", 0},
        [3] = {10000000, 1000000, "M", 1},
        [4] = {1000000, 1000000, "M", 2},
        [5] = {100000, 1000, "K", 0},
        [6] = {10000, 1000, "K", 1},
        [7] = {1000, 1000, "K", 2},
    }

    for i=1, #conversions do
        if (amount >= conversions[i][1]) then
            famount = amount/(conversions[i][2])
            if (conversions[i][4] == 2)then
                famount = _mathfloor(100*famount)
                famount = famount / 100
            elseif(conversions[i][4] == 1)then
                famount = _mathfloor(10*famount)
                famount = famount / 10
            elseif(conversions[i][4] == 0)then
                famount = _mathfloor(famount)
            end
            unit = conversions[i][3]
            decimal = conversions[i][4]
           break
        end
    end

    local formatted
    -- comma to separate the thousands
    if (famount)then
        if (famount>1000) then
            formatted = utility.commaValue(_mathfloor(famount))
        else
            formatted = famount
        end
    else
        formatted = _stringformat("%"..decimal.."d", amount)
    end

	if (unit) then
        local space = noSpace and "" or " "
		formatted = formatted..space..unit
    end

    formatted = utility.trimSpaces(formatted)

    if(not formatted) then
        return amount
    end

    return formatted
end

---Splits a string into an array/table based on seperator
--@param sep string
--@param str string
--@param limit number
--@return array, indexes
function utility.split(sep, str, limit)
    if not sep or sep == "" then return false end
    if not str then return false end
    limit = limit or math.huge
    if limit == 0 or limit == 1 then return {str},1 end

    local r = {}
    local n, init = 0, 1

    while true do
        local s,e = _stringfind(str, sep, init, true)
        if not s then break end
        r[#r+1] = _stringsub(str, init, s - 1)
        init = e + 1
        n = n + 1
        if n == limit - 1 then break end
    end

    if init <= _stringlen(str) then
        r[#r+1] = _stringsub(str, init)
    else
        r[#r+1] = ""
    end
    n = n + 1

    if limit < 0 then
        for i=n, n + limit + 1, -1 do r[i] = nil end
        n = n + limit
    end

    return r, n
end

--- Finds the specified text in the string, and replaces it with the value provided
-- @param needle the piece of text to find
-- @param replace what are we replacing the text with
-- @param haystack the string to search in
-- @return new string with the replacements
-- @usage utility.findReplace("f", "D", "343fdvcc") -- "343Ddvcc"
function utility.findReplace(needle, replace, haystack)
    return _stringgsub(haystack, needle, replace)
end

--- Trims whitespace from string
-- @param str piece of string with spaces
-- @usage utility.trimSpaces(" dflkdjflk dflkj ") -- "dfkdjflk dfkj"
function utility.trimSpaces(str)
    return str:match'^%s*(.*%S)' or ''
end

--- Encodes the string provided
-- @param stringToEncode the string to url encode
-- @return the new string url encoded
-- @usage utility.urlEncode("lkjlka 3034390") -- "lkjlka+3034390"
function utility.urlEncode(stringToEncode)
    if(stringToEncode) then
        stringToEncode = _stringgsub(stringToEncode, "\n", "\r\n")
        stringToEncode = _stringgsub(stringToEncode, "([^%w ])", function (c) return _stringformat("%%%02X", _stringbyte(c)) end)
        stringToEncode = _stringgsub(stringToEncode, " ", "+")
    end
    return stringToEncode
end

--- Checks if the string is a valid email address
-- @param stringToCheck
-- @return true|false
-- @usage utility.isValidEmail("flkjdlfk@gmail.com")
function utility.isValidEmail(str)

    if(not str) then
        return nil, "No text entered"
    end

    if (_type(str) ~= 'string') then
        return false, "Email contains invalid characters"
    end

    if(str == "") then
        return nil, "No text entered"
    end

    local lastAt = str:find("[^%@]+$")
    local localPart = str:sub(1, (lastAt - 2)) -- Returns the substring before '@' symbol
    local domainPart = str:sub(lastAt, #str) -- Returns the substring after '@' symbol
    -- we werent able to split the email properly
    if localPart == nil then
        return nil, "Local name is invalid"
    end

    if domainPart == nil then
        return nil, "Domain is invalid"
    end
    -- local part is maxed at 64 characters
    if #localPart > 64 then
        return nil, "Local name must be less than 64 characters"
    end
    -- domains are maxed at 253 characters
    if #domainPart > 253 then
        return nil, "Domain must be less than 253 characters"
    end
    -- somthing is wrong
    if lastAt >= 65 then
        return nil, "Invalid @ symbol usage"
    end
    -- quotes are only allowed at the beginning of a the local name
    local quotes = localPart:find("[\"]")
    if _type(quotes) == 'number' and quotes > 1 then
        return nil, "Invalid usage of quotes"
    end
    -- no @ symbols allowed outside quotes
    if localPart:find("%@+") and quotes == nil then
        return nil, "Invalid @ symbol usage in local part"
    end
    -- only 1 period in succession allowed
    if domainPart:find("%.%.") then
        return nil, "Too many periods in domain"
    end
    -- just a general match
    if not str:match('[%w]*[%p]*%@+[%w]*[%.]?[%w]*') then
        return nil, "Not a valid email address"
    end
    -- all our tests passed, so we are ok
    return true
end

--- Checks if the string is a valid password
-- 6-10 characters, At least one alpha AND one number, The following special chars are allowed (0 or more): !@#$%
-- @param str
--
function utility.isValidPassword(str)

    if(_type(str) ~= 'string') then
        return nil, "Password contains invalid characters"
    end

    if(#str < 6 or #str > 10) then
        return nil, "Password must be 6-10 characters."
    end

    if(not str:match('^(?=.*\d+)(?=.*[a-zA-Z])[0-9a-zA-Z!@#$%]{6,10}$')) then
        return nil, "Password must have one alpha and one number"
    end

    return true
end

--==================================================
--Table related
--==================================================
--- Gets the table length even as an object type table
-- @param table/object
function utility.tableLength(tbl)
	local count = 0
	for _ in _pairs(tbl) do count = count + 1 end
	return count
end

--- Creates a table and shuffles it based on min/max inputs
-- @param min starting value
-- @param max ending value
-- @return a shuffled table thats within the range of min and max
function utility.createShuffleTableMinMax(min, max)
    local tableRange = {}

    --Create a table with the range specified
    for x = min, max do
        tableRange[#tableRange+1] = x
    end

    --Handles shuffling
    local iterations = #tableRange
    local j

    for x = iterations, 2, -1 do
        j = _mathrandom(x)
        tableRange[x], tableRange[j] = tableRange[j], tableRange[x]
    end

    return tableRange
end

--- Merges a table together into one
--@param t1 table to merge into
--@param t2 table to merge from
--@param canOverwrite boolean can t2 make values
--@return table merged table of t1/t2 combined,
function utility.mergeTables(t1, t2, canOverwrite)
    for k, v in _pairs(t2) do
        if(_type(v) == "table") and (_type(t1[k] or false) == "table") then
            utility.mergeTables(t1[k], t2[k], canOverwrite)
        else
        	if(not t1[k] or canOverwrite) then
            	t1[k] = v
            end
        end
    end
    return t1
end

--- Checks the values and sees if the needle is in the haystack table
-- @param needle what we need to find in the table
-- @param haystack the table to search in
-- @param indexOrTable retrieve a table up to the amount, or return all indexes as a table
-- @treturn boolean of if it exists
-- @usage utility.inTable(43, {2,4,5,43,2}) -- index number found at or false
function utility.inTable(needle, haystack, indexOrTable)
    local valid = {}
    local valueToMatch = needle
    for i = 1, #haystack do
        local stackValue = haystack[i]
        local isMatching = valueToMatch == stackValue
        if(isMatching) then
            valid[#valid+1] = i
        end
    end

    local numMatching = #valid
    if numMatching>0 then
        if(indexOrTable) then
            if(_type(indexOrTable) == "number" and indexOrTable <= numMatching) then
                return valid[indexOrTable]
            end
            return valid
        end
        return true
    end
    return false
end

--- Shuffles the referenced table
-- @param table what table are we shuffling
-- @usage utility.shuffleTable({4,3,2,6,7}) -- {3,7,6,2,4}
function utility.shuffleTable(table)
    _assert(_type(table) == "table", "shuffleTable() expected a table, got nil")
    for i = #table, 2, -1 do
        local n = _mathrandom(i)
        table[i], table[n] = table[n], table[i]
    end

    return table
end

--- Returns the table in the order as defined
-- @param tbl the table to sort
-- @param[opt] order function(tbl,firstval,secondval) end in which to sort the table (pass in 3 variables) (default: basic sort)
-- @treturn table
-- @usage utility.sortTable({1,2,3,4,5,6}, x < y) -- ordered sorted table
function utility.sortTable(tbl, order)
    -- collect the keys
    local keys = {}
    for k in _pairs(tbl) do keys[#keys+1] = k end

    -- if order function given, sort by it by passing the table and keys a, b,
    -- otherwise just sort the keys
    if order then
        _tablesort(keys, function(a,b) return order(tbl, a, b) end)
    else
        _tablesort(keys)
    end

    -- return the iterator function
    local i = 0
    return function()
        i = i + 1
        if keys[i] then
            return keys[i], tbl[keys[i]]
        end
    end
end

--- Empties the table, and optionally nils the entire table after clearing children
-- @param tbl the table to clear
-- @param[opt] toNil if you wish to remove the table completely after child removal
-- @usage utility.emptyTable({"df","dfdF"}, true) -- nil
function utility.emptyTable(tbl, toNil)
    for k in _pairs(tbl) do
        tbl[k] = nil
    end

    --Nil the table too
    if(toNil) then
        tbl = nil
    end
end

--- Copies the table, and does not reference the table that was fed in
-- @param tbl the table to copy
-- @param[opt] isComplex if the table has metatables, and/or recursive, set to true
-- @usage local copiedTable = utility.copyTable(tableOne, true) -- outputs the copiedTable without reference to tableOne
function utility.copyTable(tbl, isComplex)

    --For simple copy
    local function copySimple(tbl)
        --If is a table
        if _type(tbl) ~= 'table' then
            return tbl
        end

        local res = {}
        for k, v in _pairs(tbl) do
            res[copySimple(k)] = copySimple(v)
        end

        return res
    end

    --For complex copy
    local function copyComplex(obj, seen)

        -- Handle non-tables and previously-seen tables.
        if _type(obj) ~= 'table' then
            return obj
        end

        if seen and seen[obj] then
            return seen[obj]
        end

        -- New table; mark it as seen an copy recursively.
        local s = seen or {}
        local res = _setmetatable({}, _getmetatable(obj))
        s[obj] = res
        for k, v in _pairs(obj) do
            res[copyComplex(k, s)] = copyComplex(v, s)
        end

        return res
    end

    --If a complex table flag was set
    if(isComplex) then
        return copyComplex(tbl)
    else
        return copySimple(tbl)
    end

end

--- Sort table by numberic keys that are out of order
-- @t table that has numberical keys
function utility.sortTableByKey(t)
	local tkeys = {}
	local returnTable = {}
	-- populate the table that holds the keys
	for k in _pairs(t) do _tableinsert(tkeys, k) end
	-- sort the keys
	_tablesort(tkeys)
	for _, k in _ipairs(tkeys) do
		returnTable[k] = t[k]
	end
	return returnTable
end

---Gets a table value based on a string format of "tblkey.nestedtableKey"
--@param str string to extract table references
--@param tbl the table to get the data from
--@param sep string to seperate table references from param str
--@return mixed value or boolean false
function utility.getTableValueByString(str, tbl, sep)
	--seperator defaults to .
	sep = sep or "."
	local tabledKeys = utility.split(sep, str)
	local selectedTbl = tbl
	local key

	local isExist = function(key, nestedTbl) return _type(nestedTbl[key]) ~= "nil" end

	for x=1, #tabledKeys do
		--set key to search for in table
		key = tabledKeys[x]

		--check table for existance
		if(isExist(key, selectedTbl)) then

			--append
			selectedTbl = selectedTbl[key]

		else
			--return false
			return false

		end
	end

	--return the final value when all is good.
	return selectedTbl
end

--- Compares a table to ensure that table is clean or dirty
--@param o1 table 1
--@param o2 table 2
--@param ignore_mt ignore metatables that exist
--@return true or false
function utility.compareTables(o1, o2, ignore_mt)
	if o1 == o2 then return true end
	local o1Type = _type(o1)
	local o2Type = _type(o2)
	if o1Type ~= o2Type then return false end
	if o1Type ~= 'table' then return false end

	if not ignore_mt then
		local mt1 = _getmetatable(o1)
		if mt1 and mt1.__eq then
			--compare using built in method
			return o1 == o2
		end
	end

	local keySet = {}

	for key1, value1 in _pairs(o1) do
		local value2 = o2[key1]
		if value2 == nil or utility.compareTables(value1, value2, ignore_mt) == false then
			return false
		end
		keySet[key1] = true
	end

	for key2, _ in _pairs(o2) do
		if not keySet[key2] then return false end
	end
	return true
end

--==================================================
--File Functions
--==================================================

--- Checks if the filename exists in the path provided
-- @param fname file path to the file
-- @param[opt] path path to the supposed file, default: system.DocumentsDirectory
-- @treturn boolean
-- @usage utility.fileExists("path/to/file/filename.exe") -- true/false
function utility.fileExists(fname, path)
    local path = path or _system.DocumentsDirectory
    local results = false

    local filePath = _system.pathForFile(fname, path)

    -- filePath will be 'nil' if file doesn't exist and the path is 'system.ResourceDirectory'
    if(filePath) then
        filePath = _ioopen(filePath, "r")
    end

    if(filePath) then
        filePath:close()
        results = true
    end

    return results
end

--- Checks if the module (that will be called by require) exists
-- @param name
-- @param boolean
function utility.moduleExists(name)
    if package.loaded[name] then
        return true
    else
        for _, searcher in _ipairs(package.searchers or package.loaders) do
            local loader = searcher(name)
            if _type(loader) == 'function' then
                package.preload[name] = loader
                return true
            end
        end
        return false
    end
end

--- Checks if the folder exists and if it does not exist, creates it
-- @param folderName folder path
-- @param[opt] path path to the supposed file, default: system.DocumentsDirectory
-- @usage utility.createSubFolderIfNotExists("file/path/to/tomato")
function utility.createSubFolderIfNotExists(folderName, path)
    local path = path or _system.DocumentsDirectory

    -- Get raw path to documents directory
    local docs_path = _system.pathForFile("", path)

    -- Change current working directory
    if(lfs.chdir(docs_path))then
        lfs.mkdir(folderName)
    end
end


--- Checks if various getVariables or overlay is open  (ie. chat /popup menu or an overlay box)
--
function utility.areOverlaysOpen()
	return _composerGetSceneName("overlay") or _composerGetVariable("isChatOpen") or _composerGetVariable("isMainMenuShown")
end

--- Waits for an overlay to be closed then executes the onOverlayClosed function.  (change to coroutine if corona updates lua to 5.2 for greater flexibility)
-- @param onOverlayClosed
-- @return timerobject|false - in case so if we need to cancel it.
-- @usage utility.waitForOverlay(function() --[[do something after overlay is finally closed--]] end)
function utility.waitForOverlay(onOverlayClosed)
    local onOverlayClosed = onOverlayClosed and onOverlayClosed or function() end
    local isOverlayShown = utility.areOverlaysOpen()

    --return false as first var when we aren't going to return the timer object
    if(not isOverlayShown) then
        onOverlayClosed()
        onOverlayClosed = nil
        return nil
    end

	--This is mainly for the slots, because garbage collection is currently stopped.
	collectgarbage("collect")

    -- we localize the timer function within so we can use variable within this function.
    -- TODO: Do this with all timers. this will prevent errors on timers in the future when we need to reference this parent's variables b/c its now a child of the parent. (aka lua closure)
    -- TODO: "NOTE: to pass a parameter to the listener, use a Lua closure, as in the following example:" - corona
    -- TODO: cont: https://docs.coronalabs.com/api/library/timer/performWithDelay.html
    local timerFunct = function(event)
        isOverlayShown = utility.areOverlaysOpen()
        if(not isOverlayShown) then
            onOverlayClosed()
            onOverlayClosed = nil
            timer.cancel(event.source)
            event.source = nil
        end
    end
    return _timerPerformWithDelay(100, timerFunct, -1)
end

--- Waits for an scene to be shown then executes the onOverlayClosed function.  (change to coroutine if corona updates lua to 5.2 for greater flexibility)
-- @param onSceneShown
-- @return timerobject|false - in case so if we need to cancel it.
-- @usage utility.waitForSceneShow(function() --[[do something after overlay is finally open and shown--]] end)
function utility.waitForSceneShow(sceneName, onSceneShown)
	onSceneShown = onSceneShown and onSceneShown or function() end
	local isSceneShown = _composerGetSceneName("current") == sceneName

	--return false as first var when we aren't going to return the timer object
	if(not isSceneShown) then
		onSceneShown()
		onSceneShown = nil
		return nil
	end

	--This is mainly for the slots, because garbage collection is currently stopped.
	collectgarbage("collect")

	-- we localize the timer function within so we can use variable within this function.
	-- TODO: Do this with all timers. this will prevent errors on timers in the future when we need to reference this parent's variables b/c its now a child of the parent. (aka lua closure)
	-- TODO: "NOTE: to pass a parameter to the listener, use a Lua closure, as in the following example:" - corona
	-- TODO: cont: https://docs.coronalabs.com/api/library/timer/performWithDelay.html
	local timerFunct = function(event)
		isSceneShown = _composerGetSceneName("current") == sceneName
		if(not isSceneShown) then
			onSceneShown()
			onSceneShown = nil
			timer.cancel(event.source)
			event.source = nil
		end
	end
	return _timerPerformWithDelay(100, timerFunct, -1)
end

--- Waits for an property to be true (change to coroutine if corona updates lua to 5.2 for greater flexibility)
-- @param object
-- @param name
-- @return proceeds with the rest of the lua function it was first delcared in
-- @link https://coronalabs.com/blog/2015/02/10/tutorial-using-coroutines-in-corona/
-- @usage utility.waitUntilPropertyTrue(aNewRectObject, "isVisible")
function utility.waitUntilPropertyTrue( object, name, update)
	while not object[name] do
		if update then
			update()
		end
		coroutine.yield()
	end
end

---Converts a hex table to an table of rgb
--@param hex table
--@return table
function utility.hex2rgb(hex)
    hex = hex:_stringgsub("#","")
    return {_tonumber("0x"..hex:sub(1,2))/255, _tonumber("0x"..hex:sub(3,4))/255, _tonumber("0x"..hex:sub(5,6))/255}
end

return utility

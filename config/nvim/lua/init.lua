local home = os.getenv("HOME")

--- Check if a file or directory exists in this path
function exists(file)
  local ok, err, code = os.rename(file, file)
  if not ok then
    if code == 13 then
      -- Permission denied, but it exists
      return true
    end
  end
  return ok, err
end

--- Check if a directory exists in this path
function isdir(path)
  -- "/" works on both Unix and Windows
  return exists(path.."/")
end

local function create_backup_dir()
  local cache_dir = home .. '/.cache/vim/'
  local data_dir = {cache_dir..'backup',cache_dir..'session',cache_dir..'swap',cache_dir..'tags',cache_dir..'undo'}
  if not isdir(cache_dir) then
    os.execute("mkdir -p " .. data_dir)
  end
  for k,v in pairs(data_dir) do
    if not isdir(v) then
      os.execute("mkdir -p " .. v)
    end
  end
end

create_backup_dir()

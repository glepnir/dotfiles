is_mac     = jit.os == 'OSX'
is_linux   = jit.os == 'Linux'
is_windows = jit.os == 'Windows'

home        = os.getenv("HOME")
vim_path    = home .. '/.config/nvim'
cache_dir   = home .. '/.cache/vim/'
map_dir     = vim_path .. 'maps/'
modules_dir = vim_path .. '/modules'

local valid_modes = {
  n = 'n'; v = 'v'; x = 'x'; i = 'i';
  o = 'o'; t = 't'; c = 'c'; s = 's';
  -- :map! and :map
  ['!'] = '!'; [' '] = '';
}

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

function map_cmd(cmd_string, buflocal)
  return { ("<Cmd>%s<CR>"):format(cmd_string), noremap = true; buffer = buflocal;}
end

function map_call(cmd_string, buflocal)
  return { ("%s<CR>"):format(cmd_string), noremap = true; buffer = buflocal;}
end

function map_no_cr(cmd_string, buflocal)
  return { (":%s"):format(cmd_string), noremap = true; buffer = buflocal;}
end

function dump(o)
   if type(o) == 'table' then
      local s = '{ '
      for k,v in pairs(o) do
         if type(k) ~= 'number' then k = '"'..k..'"' end
         s = s .. '['..k..'] = ' .. dump(v) .. ','
      end
      return s .. '} '
   else
      return tostring(o)
   end
end

function readAll(file)
    local f = assert(io.open(file, "rb"))
    local content = f:read("*all")
    f:close()
    return content
end

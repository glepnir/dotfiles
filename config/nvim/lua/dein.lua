require 'global'

local dein  = {}
local repos = {}

function readAll(file)
    local f = assert(io.open(file, "rb"))
    local content = f:read("*all")
    f:close()
    return content
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

function dein.parse_repos()
  if is_mac then
    cmd = [[ruby -e 'require "json"; require "yaml"; print JSON.generate YAML.load $stdin.read']]
  end
  local p = io.popen('find "'..modules_dir..'" -name "*.yaml"')
  for file in p:lines() do
    repo = vim.api.nvim_eval(vim.fn.system(cmd,readAll(file)))
    table.insert(repos,repo)
  end
  for k,v in pairs(repos) do
    print(dump(v)["repo"])
  end
end

function dein.load_repos()
  local dein_dir = cache_dir ..'dein/repos/github.com/Shougo/dein.vim'
  local cmd = '!git clone https://github.com/Shougo/dein.vim' .. dein_dir

  if vim.fn.has('vim_starting') then
    vim.api.nvim_set_option('dein#auto_recache',1)
    vim.api.nvim_set_option('dein#install_max_processes',12)
    vim.api.nvim_set_option('dein#install_progress_type',"title")
    vim.api.nvim_set_option('dein#enable_notification',1)
    vim.api.nvim_set_option('dein#install_log_filename',cache_dir ..'dein.log')

    if not string.match(vim.o.runtimepath,'/dein.vim') then
      if not isdir(dein_dir) then
        os.execute(cmd)
      end
    end

    vim.o.runtimepath = vim.o.runtimepath + dein_dir
  end

  if vim.api.nvim_call_function('dein#load_state',{cache_dir..'dein'}) then
    vim.api.nvim_call_function('dein#begin',{cache_dir..'dein'})
    for index,repo in pairs(repos) do
      vim.api.nvim_call_function('dein#add',{repos['repo']})
    end
  end
end

dein.parse_repos()
dump(repos)

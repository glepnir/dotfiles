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

-- TODO(ashkan) @feature Disable noremap if the rhs starts with <Plug>
function nvim_apply_mappings(mappings, default_options)
  -- May or may not be used.
  local current_bufnr = vim.api.nvim_get_current_buf()
  for key, options in pairs(mappings) do
    options = vim.tbl_extend("keep", options, default_options or {})
    local bufnr = current_bufnr
    -- TODO allow passing bufnr through options.buffer?
    -- protect against specifying 0, since it denotes current buffer in api by convention
    if type(options.buffer) == 'number' and options.buffer ~= 0 then
      bufnr = options.buffer
    end
    local mode, mapping = key:match("^(.)(.+)$")
    assert(mode, "nvim_apply_mappings: invalid mode specified for keymapping "..key)
    assert(valid_modes[mode], "nvim_apply_mappings: invalid mode specified for keymapping. mode="..mode)
    mode = valid_modes[mode]
    local rhs = options[1]
    -- Remove this because we're going to pass it straight to nvim_set_keymap
    options[1] = nil
    if type(rhs) == 'function' then
      -- Use a value that won't be misinterpreted below since special keys
      -- like <CR> can be in key, and escaping those isn't easy.
      local escaped = escape_keymap(key)
      local key_mapping
      if options.dot_repeat then
        local key_function = rhs
        rhs = function()
          key_function()
          -- -- local repeat_expr = key_mapping
          -- local repeat_expr = mapping
          -- repeat_expr = vim.api.nvim_replace_termcodes(repeat_expr, true, true, true)
          -- nvim.fn["repeat#set"](repeat_expr, nvim.v.count)
          nvim.fn["repeat#set"](nvim.replace_termcodes(key_mapping, true, true, true), nvim.v.count)
        end
        options.dot_repeat = nil
      end
      if options.buffer then
        -- Initialize and establish cleanup
        if not LUA_BUFFER_MAPPING[bufnr] then
          LUA_BUFFER_MAPPING[bufnr] = {}
          -- Clean up our resources.
          vim.api.nvim_buf_attach(bufnr, false, {
            on_detach = function()
              LUA_BUFFER_MAPPING[bufnr] = nil
            end
          })
        end
        LUA_BUFFER_MAPPING[bufnr][escaped] = rhs
        -- TODO HACK figure out why <Cmd> doesn't work in visual mode.
        if mode == "x" or mode == "v" then
          key_mapping = (":<C-u>lua LUA_BUFFER_MAPPING[%d].%s()<CR>"):format(bufnr, escaped)
        else
          key_mapping = ("<Cmd>lua LUA_BUFFER_MAPPING[%d].%s()<CR>"):format(bufnr, escaped)
        end
      else
        LUA_MAPPING[escaped] = rhs
        -- TODO HACK figure out why <Cmd> doesn't work in visual mode.
        if mode == "x" or mode == "v" then
          key_mapping = (":<C-u>lua LUA_MAPPING.%s()<CR>"):format(escaped)
        else
          key_mapping = ("<Cmd>lua LUA_MAPPING.%s()<CR>"):format(escaped)
        end
      end
      rhs = key_mapping
      options.noremap = true
      options.silent = true
    end
    if options.buffer then
      options.buffer = nil
      vim.api.nvim_buf_set_keymap(bufnr, mode, mapping, rhs, options)
    else
      vim.api.nvim_set_keymap(mode, mapping, rhs, options)
    end
  end
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

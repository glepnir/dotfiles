require 'global'

mapping = {}

function mapping:new()
  instance = {}
  setmetatable(instance, self)
  self.__index = self
  self.define = {}
  return instance
end

function mapping:load_define()
  self.define = {
    -- Vim map
    ["n|<C-x>k"]         = map_recursive_cr('BD'),
    ["n|<C-s>"]          = map_recursive_cu('write'),
    ["n|Y"]              = map_recursive('y$'),
    ["n|]w"]             = map_recursive_cu('WhiteSpaceNext'),
    ["n|[w"]             = map_recursive_cu('WhiteSpacePrev'),
    ["n|]b"]             = map_recursive_cu('bp'),
    ["n|[b"]             = map_recursive_cu('bn'),
    ["n|<Space>cw"]      = map_recursive_cu([[silent! keeppatterns %substitute/\s\+$//e]]),
    ["n|<C-h>"]          = map_recursive('<C-w>h'),
    ["n|<C-l>"]          = map_recursive('<C-w>l'),
    ["n|<C-j>"]          = map_recursive('<C-w>j'),
    ["n|<C-k>"]          = map_recursive('<C-w>k'),
    ["n|<C-w>["]         = map_not_recursive_cr('vertical resize -3'),
    ["n|<C-w>]"]         = map_not_recursive_cr('vertical resize +3'),
    ["n|<Leader>ss"]     = map_recursive_cu('SessionSave'),
    ["n|<Leader>sl"]     = map_recursive_cu('SessionLoad'),
    -- Plugin map
    ["n|<Leader>tf"]     = map_recursive_cu('DashboardNewFile'),
    ["n|<Leader>bc"]     = map_recursive_cr('Bonly'),
    ["n|<Leader>bx"]     = map_recursive_cr('Bw'),
    ["n|<Leader>,0,9"]   = '<Plug>BuffetSwitch(+)',
    -- Plugin Defx
    ["n|<Leader>e"]      = map_recursive_cu([[Defx -resume -toggle -buffer-name=tab`tabpagenr()`]]),
    ["n|<Leader>F"]      = map_recursive_cu([[Defx -resume -buffer-name=tab`tabpagenr()` -search=`expand('%:p')`]]),
    -- Plugin MarkdownPreview
    ["n|<Leader>om"]     = map_recursive_cu('MarkdownPreview'),
    -- Plugin Floaterm
    ["n|<Leader>t"]      = map_recursive_cu('FloatermToggle'),
    ["n|<Leader>g"]      = map_recursive_cu('FloatermNew height=0.7 width=0.8 lazygit'),
    -- Plugin DadbodUI
    ["n|<Leader>od"]     = map_recursive_cr('DBUIToggle'),
    -- Plugin Coc-Clap
    ["n|<Leader>ce"]     = map_recursive_cr('Clap coc_diagnostics'),
    ["n|<Leader>;"]      = map_recursive_cr('Clap coc_extensions'),
    ["n|<Leader>,"]      = map_recursive_cr('Clap coc_commands'),
    ["n|<Leader>cs"]     = map_recursive_cr('Clap coc_symbols'),
    ["n|<Leader>cS"]     = map_recursive_cr('Clap coc_services'),
    ["n|<Leader>ct"]     = map_recursive_cr('Clap coc_outline'),
    -- Plugin Clap
    ["n|<Leader>tc"]     = map_recursive_cu('Clap colors'),
    ["n|<Leader>bb"]     = map_recursive_cu('Clap bufers'),
    ["n|<Leader>fa"]     = map_recursive_cu('Clap grep'),
    ["n|<Leader>fb"]     = map_recursive_cu('Clap marks'),
    ["n|<C-x><C-f>"]     = map_recursive_cu('Clap filer'),
    ["n|<Leader>ff"]     = map_recursive_cu('Clap files ++finder=rg --ignore --hidden --files'),
    ["n|<Leader>fg"]     = map_recursive_cu('Clap gfiles'),
    ["n|<Leader>fw"]     = map_recursive_cu('Clap grep ++query=<Cword>'),
    ["n|<Leader>fh"]     = map_recursive_cu('Clap history'),
    ["n|<Leader>fW"]     = map_recursive_cu('Clap windows'),
    ["n|<Leader>fl"]     = map_recursive_cu('Clap loclist'),
    ["n|<Leader>fu"]     = map_recursive_cu('Clap git_diff_files'),
    ["n|<Leader>fv"]     = map_recursive_cu('Clap grep ++query=@visual'),
    ["n|<Leader>oc"]     = map_recursive_cu('Clap dotfiles'),
    ["n|<LocalLeader>g"] = map_recursive_cu('Clap gosource'),
    ["n|j"]              = map_not_recursive('<Plug>(accelerated_jk_gj)'),
    ["n|k"]              = map_not_recursive('<Plug>(accelerated_jk_gk)'),
    -- Plugin QuickRun
    ["n|<Leader>cr"]     = map_recursive_cr('QuickRun'),
    -- Plugin Vista
    ["n|<Leader>i"]      = map_recursive_cu('Vista!!'),
    -- Plugin Easymotion
    ["n|gsj"]            = map_not_recursive('<Plug>(easymotion-w)'),
    ["n|gsk"]            = map_not_recursive('<Plug>(easymotion-b)'),
    ["n|gsf"]            = map_not_recursive('<Plug>(easymotion-overwin-f)'),
    ["n|gss"]            = map_not_recursive('<Plug>(easymotion-overwin-f2)'),
    -- Plugin Mundo
    ["n|<Leader>m"]      = map_recursive_cu('MundoToggle'),
    -- Plugin SplitJoin
    ["n|sj"]  = map_not_recursive('SplitjoinJoin'),
    ["n|sk"]  = map_not_recursive('SplitjoinSplit'),
    -- Plugin dsf
    ["n|dsf"] = map_not_recursive('<Plug>DsfDelete'),
    ["n|csf"] = map_not_recursive('<Plug>DsfChange'),
    -- Plugin go-nvim
    ["n|gcg"] = map_recursive_cr('GoAutoComment'),

  -- Insert
    ["i|<C-w>"]       = map_recursive('<C-[>diwa'),
    ["i|<C-h>"]       = map_recursive('<BS>'),
    ["i|<C-d>"]       = map_recursive('<Del>'),
    ["i|<C-k>"]       = map_recursive('<ESC>d$a'),
    ["i|<C-u>"]       = map_recursive('<C-G>u<C-U>'),
    ["i|<C-b>"]       = map_recursive('<Left>'),
    ["i|<C-f>"]       = map_recursive('<Right>'),
    ["i|<C-a>"]       = map_recursive('<ESC>^i'),
    ["i|<C-o>"]       = map_recursive('<Esc>o'),
    ["i|<C-s>"]       = map_not_recursive('<Esc>:w<CR>'),
    ["i|<C-q>"]       = map_not_recursive('<Esc>:wq<CR>'),
    ["i|<C-e>"]       = map_recursive_expr([[pumvisible() ? "\<C-e>" : "\<End>"]]),
  -- command line
    ["c|<C-b>"] = map_recursive('<Left>'),
    ["c|<C-f>"] = map_recursive('<Right>'),
    ["c|<C-a>"] = map_recursive('<Home>'),
    ["c|<C-e>"] = map_recursive('<End>'),
    ["c|<C-d>"] = map_recursive('<Del>'),
    ["c|<C-h>"] = map_recursive('<BS>'),
    ["c|<C-t>"] = map_recursive([[<C-R>=expand("%:p:h") . "/" <CR>]]),
  };
end

function mapping:load_mapping()
  self:load_define()
  for key,value in pairs(self.define) do
    local mode,keymap = key:match("([^|]*)|?(.*)")
    if type(value) == 'table' then
      rhs = value[1]
      options = vim.tbl_extend("keep",value[2],default_options or {})
      vim.api.nvim_set_keymap(mode,keymap,rhs,options)
    elseif type(value) == 'string' then
      local k,min,max = keymap:match("([^,]+),([^,]+),([^,]+)")
      for i=tonumber(min),tonumber(max) do
        key = (k.."%s"):format(i)
        rhs = value:gsub("+",i)
        vim.api.nvim_set_keymap(mode,key,rhs,{noremap = true})
      end
    end
  end
end

function map_recursive(cmd_string)
  return {cmd_string,{noremap = true}}
end

function map_not_recursive(cmd_string)
  return {cmd_string,{noremap = false}}
end

function map_recursive_cr(cmd_string)
  return {(":%s<CR>"):format(cmd_string),{noremap = true}}
end

function map_not_recursive_cr(cmd_string)
  return {(":%s<CR>"):format(cmd_string),{noremap = false}}
end

function map_recursive_cu(cmd_string)
  return {(":<C-u>%s<CR>"):format(cmd_string),{noremap = true}}
end

function map_recursive_expr(cmd_string)
  return {cmd_string,{noremap = true,expr = true}}
end

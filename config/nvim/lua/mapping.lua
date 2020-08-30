require 'global'

mapping = {}

function mapping:new()
  instance = {}
  setmetatable(instance, self)
  self.__index = self
  self.normal  = {}
  self.insert  = {}
  self.command = {}
  return instance
end

function mapping:define()
  self.normal = {
    -- Vim map
    ["<C-x>k"]         = map_recursive_cr('BD'),
    ["<C-s>"]          = map_recursive_cu('write'),
    ["Y"]              = map_recursive('y$'),
    ["]w"]             = map_recursive_cu('WhiteSpaceNext'),
    ["[w"]             = map_recursive_cu('WhiteSpacePrev'),
    ["]b"]             = map_recursive_cu('bp'),
    ["[b"]             = map_recursive_cu('bn'),
    ["<Space>cw"]      = map_recursive_cu([[silent! keeppatterns %substitute/\s\+$//e]]),
    ["<C-h>"]          = map_recursive('<C-w>h'),
    ["<C-l>"]          = map_recursive('<C-w>l'),
    ["<C-j>"]          = map_recursive('<C-w>j'),
    ["<C-k>"]          = map_recursive('<C-w>k'),
    ["<C-w>["]         = map_not_recursive_cr('vertical resize -3'),
    ["<C-w>]"]         = map_not_recursive_cr('vertical resize +3'),
    ["<Leader>ss"]     = map_recursive_cu('SessionSave'),
    ["<Leader>sl"]     = map_recursive_cu('SessionLoad'),
    -- Plugin map
    ["<Leader>tf"]     = map_recursive_cu('DashboardNewFile'),
    ["<Leader>bc"]     = map_recursive_cr('Bonly'),
    ["<Leader>bx"]     = map_recursive_cr('Bw'),
    ["<Leader>,0,9"]   = '<Plug>BuffetSwitch(+)',
    -- Plugin Defx
    ["<Leader>e"]      = map_recursive_cu([[Defx -resume -toggle -buffer-name=tab`tabpagenr()`]]),
    ["<Leader>F"]      = map_recursive_cu([[Defx -resume -buffer-name=tab`tabpagenr()` -search=`expand('%:p')`]]),
    -- Plugin MarkdownPreview
    ["<Leader>om"]     = map_recursive_cu('MarkdownPreview'),
    -- Plugin Floaterm
    ["<Leader>t"]      = map_recursive_cu('FloatermToggle'),
    ["<Leader>g"]      = map_recursive_cu('FloatermNew height=0.7 width=0.8 lazygit'),
    -- Plugin DadbodUI
    ["<Leader>od"]     = map_recursive_cr('DBUIToggle'),
    -- Plugin Coc-Clap
    ["<Leader>ce"]     = map_recursive_cr('Clap coc_diagnostics'),
    ["<Leader>;"]      = map_recursive_cr('Clap coc_extensions'),
    ["<Leader>,"]      = map_recursive_cr('Clap coc_commands'),
    ["<Leader>cs"]     = map_recursive_cr('Clap coc_symbols'),
    ["<Leader>cS"]     = map_recursive_cr('Clap coc_services'),
    ["<Leader>ct"]     = map_recursive_cr('Clap coc_outline'),
    -- Plugin Clap
    ["<Leader>tc"]     = map_recursive_cu('Clap colors'),
    ["<Leader>bb"]     = map_recursive_cu('Clap bufers'),
    ["<Leader>fa"]     = map_recursive_cu('Clap grep'),
    ["<Leader>fb"]     = map_recursive_cu('Clap marks'),
    ["<C-x><C-f>"]     = map_recursive_cu('Clap filer'),
    ["<Leader>ff"]     = map_recursive_cu('Clap files ++finder=rg --ignore --hidden --files'),
    ["<Leader>fg"]     = map_recursive_cu('Clap gfiles'),
    ["<Leader>fw"]     = map_recursive_cu('Clap grep ++query=<Cword>'),
    ["<Leader>fh"]     = map_recursive_cu('Clap history'),
    ["<Leader>fW"]     = map_recursive_cu('Clap windows'),
    ["<Leader>fl"]     = map_recursive_cu('Clap loclist'),
    ["<Leader>fu"]     = map_recursive_cu('Clap git_diff_files'),
    ["<Leader>fv"]     = map_recursive_cu('Clap grep ++query=@visual'),
    ["<Leader>oc"]     = map_recursive_cu('Clap dotfiles'),
    ["<LocalLeader>g"] = map_recursive_cu('Clap gosource'),
    ["j"]              = map_not_recursive('<Plug>(accelerated_jk_gj)'),
    ["k"]              = map_not_recursive('<Plug>(accelerated_jk_gk)'),
    -- Plugin QuickRun
    ["<Leader>cr"]     = map_recursive_cr('QuickRun'),
    -- Plugin Vista
    ["<Leader>i"]      = map_recursive_cu('Vista!!'),
    -- Plugin Easymotion
    ["gsj"]            = map_not_recursive('<Plug>(easymotion-w)'),
    ["gsk"]            = map_not_recursive('<Plug>(easymotion-b)'),
    ["gsf"]            = map_not_recursive('<Plug>(easymotion-overwin-f)'),
    ["gss"]            = map_not_recursive('<Plug>(easymotion-overwin-f2)'),
    -- Plugin Mundo
    ["<Leader>m"]      = map_recursive_cu('MundoToggle'),
    -- Plugin SplitJoin
    ["sj"]  = map_not_recursive('SplitjoinJoin'),
    ["sk"]  = map_not_recursive('SplitjoinSplit'),
    -- Plugin dsf
    ["dsf"] = map_not_recursive('<Plug>DsfDelete'),
    ["csf"] = map_not_recursive('<Plug>DsfChange'),
    -- Plugin go-nvim
    ["gcg"] = map_recursive_cr('GoAutoComment')

  };
  self.insert = {
    ["<C-w>"]       = map_recursive('<C-[>diwa'),
    ["<C-h>"]       = map_recursive('<BS>'),
    ["<C-d>"]       = map_recursive('<Del>'),
    ["<C-k>"]       = map_recursive('<ESC>d$a'),
    ["<C-u>"]       = map_recursive('<C-G>u<C-U>'),
    ["<C-b>"]       = map_recursive('<Left>'),
    ["<C-f>"]       = map_recursive('<Right>'),
    ["<C-a>"]       = map_recursive('<ESC>^i'),
    ["<C-o>"]       = map_recursive('<Esc>o'),
    ["<C-s>"]       = map_not_recursive('<Esc>:w<CR>'),
    ["<C-q>"]       = map_not_recursive('<Esc>:wq<CR>'),
    ["<C-e>"]       = map_recursive_expr([[pumvisible() ? "\<C-e>" : "\<End>"]]),
  };
  self.command = {
    ["<C-b>"] = map_recursive('<Left>'),
    ["<C-f>"] = map_recursive('<Right>'),
    ["<C-a>"] = map_recursive('<Home>'),
    ["<C-e>"] = map_recursive('<End>'),
    ["<C-d>"] = map_recursive('<Del>'),
    ["<C-h>"] = map_recursive('<BS>'),
    ["<C-t>"] = map_recursive([[<C-R>=expand("%:p:h") . "/" <CR>]]),
  };
end

function nvim_define_map(map,default_options)
  local mode = {
    normal = 'n';insert = 'i'; command = 'c';
  }
  for mo,cfg in pairs(map) do
    for key,value in pairs(cfg) do
      if type(value) == 'table' then
        rhs = value[1]
        options = vim.tbl_extend("keep",value[2],default_options or {})
        vim.api.nvim_set_keymap(mode[mo],key,rhs,options)
      elseif type(value) == 'string' then
        local k,min,max = key:match("([^,]+),([^,]+),([^,]+)")
        for i=tonumber(min),tonumber(max) do
          key = (k.."%s"):format(i)
          rhs = value:gsub("+",i)
          vim.api.nvim_set_keymap(mode[mo],key,rhs,{noremap = true})
        end
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

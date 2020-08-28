require 'global'

vim_map = {}

function vim_map.load_vim_map()
  local mappings = {
    -- normal map
    ["nY"]         = {"y$",noremap = true},
    ["n<C-x>k"]    = map_call('BD'),
    ["n<C-s>"]     = map_cmd('write'),
    ["n]w"]        = map_cmd("WhitespaceNext"),
    ["n[w"]        = map_cmd("WhitespacePrev"),
    -- ["n<Space>cw"] = map_cmd("keeppatterns %substitute/\s\+$//e"),

    -- ["i<C-w>"] = {"<C-[>diwa",noremap = true},
    -- ["i<C-h>"] = {"<BS>",noremap = true},
    -- ["i<C-d>"] = {"<Del>",noremap =true},
    -- ["i<C-k>"] = {"<ESC>d$a"},

    -- ["i<C-u>"]       = {"<C-G>u<C-U>",noremap =true},
    -- ["i<C-b>"]       = {"<Left>",noremap =true},
    -- ["i<C-f>"]       = {"<Right>",noremap = true },
    -- ["i<C-a>"]       = {"<ESC>^i",noremap = true},
    -- ["i<expr><C-e>"] = [[pumvisible() ? "\<C-e>" : "\<End>"]],
    -- ["i<C-O>"]       = {"<Esc>o",noremap =true},
    -- ["i<C-S>"]        = {"<esc>:w<CR>"},
    -- ["i<C-Q>"]        = {"<esc>:wq<CR>"},
    --
    -- ["c<C-p>"] = {"<Up>",noremap = true},
    -- ["c<C-b>"] = {"<Left>",noremap = true},
    -- ["c<C-f>"] = {"<Right>", noremap = true},
    -- ["c<C-a>"] = {"<Home>", noremap = true},
    -- ["c<C-e>"] = {"<End>", noremap = true},
    -- ["c<C-d>"] = {"<Del>", noremap =true},
    -- ["c<C-h>"] = {"<BS>", noremap = true},
    -- ["c<C-t>"] = [[<C-R>=expand("%:p:h") . "/" <CR>]],
  }

  nvim_apply_mappings(mappings, {silent = true})
end

require 'global'

options = {}

function options.setOptions(items)
  for k, v in pairs(items) do
    if type(v) == 'table' then
      local values = ''
      for k2, v2 in pairs(v) do
        if k2 == 1 then
          values = values .. v2
        else
          values = values .. ',' .. v2
        end
      end
      vim.api.nvim_command('set ' .. k .. '=' .. values)
    else
      vim.api.nvim_set_option(k,v)
    end
  end
end

function options.load_options()
  local items = {
    mouse          = "nv";
    report         = 0;
    errorbells     = true;
    visualbell     = true;
    hidden         = true;
    fileformats    = {'unix','mac','dos'};
    magic          = true;
    virtualedit    = "block";
    synmaxcol      = 2500;
    formatoptions  = "1jcroql";
    encoding       = "utf-8";
    viewoptions    = "folds,cursor,curdir,slash,unix";
    sessionoptions = "curdir,help,tabpages,winsize";
    clipboard      = "unnamedplus";
    wildignorecase = true;
    wildignore     = {'.git','.hg','.svn','*.pyc','*.o','*.out','*.jpg','*.jpeg','*.png','*.gif','*.zip','**/tmp/**','*.DS_Store','**/node_modules/**','**/bower_modules/**'};
    backup         = false;
    writebackup    = false;
    undofile       = true;
    swapfile       = false;
    directory      = cache_dir .. "swag/";
    undodir        = cache_dir .. "undo/";
    backupdir      = cache_dir .. "backup/";
    viewdir        = cache_dir .. "view/";
    spellfile      = cache_dir .. "spell/en.uft-8.add";
    history        = 2000;
    shada          = "!,'300,<50,@100,s10,h";
    backupskip     = {'/tmp/*','$TMPDIR/*','$TMP/*','$TEMP/*','*/shm/*','/private/var/*','.vault.vim'};

    textwidth      = 80;
    expandtab      = false;
    tabstop        = 2;
    shiftwidth     = 2;
    softtabstop    = -1;
    smarttab       = true;
    autoindent     = true;
    shiftround     = true;
    breakindentopt = "shift:2,min:20";

    timeout        = true;
    ttimeout       = true;
    timeoutlen     = 500;
    ttimeoutlen    = 10;
    updatetime     = 100;
    redrawtime     = 1500;

    ignorecase     = true;
    smartcase      = true;
    infercase      = true;
    incsearch      = true;
    wrapscan       = true;

    complete       = ".,w,b,k";
    inccommand     = "nosplit";

    grepformat     = "%f:%l:%c:%m";
    grepprg        = [[rg\ --hidden\ --vimgrep\ --smart-case\ --]];

    wrap           = false;
    linebreak      = true;
    breakat        = [[\ \	;:,!?]];
    startofline    = false;
    whichwrap      = "h,l,<,>,[,],~";
    splitbelow     = true;
    splitright     = true;
    switchbuf      = "useopen";
    backspace      = "indent,eol,start";
    diffopt        = {'filler','iwhite','internal','algorithm:patience'};
    completeopt    = {'menu','menuone','noselect','noinsert'};
    jumpoptions    = "stack";

    showmode       = false;
    shortmess      = "aoOTIcF";
    scrolloff      = 2;
    sidescrolloff  = 5;
    ruler          = false;
    list           = true;

    showtabline    = 2;
    winwidth       = 30;
    winminwidth    = 10;
    pumheight      = 15;
    helpheight     = 12;
    previewheight  = 12;

    number         = true;
    showcmd        = false;
    cmdheight      = 2;
    cmdwinheight   = 5;
    equalalways    = false;
    laststatus     = 2;
    colorcolumn    = "100";
    display        = "lastline";

    foldenable     = true;
    foldmethod     = "indent";
    foldlevelstart = 99;

    signcolumn     = "yes";
    showbreak      = "↳  ";
    listchars      = {"tab:»·","nbsp:+","trail:·","extends:→","precedes:←"};
    conceallevel   = 2;
    concealcursor  = "niv";
    termguicolors  = true;
    pumblend       = 10;
    winblend       = 10;
  };
  if is_mac then
    vim.g.clipboard = {
      name = "macOS-clipboard",
      copy = {
        ["+"] = "pbcopy",
        ["*"] = "pbcopy",
      },
      paste = {
        ["+"] = "pbpaste",
        ["*"] = "pbpaste",
      },
      cache_enabled = 0
    }
    vim.g.python_host_prog = '/usr/bin/python'
    vim.g.python3_host_prog = '/usr/local/bin/python3'
  end
  options.setOptions(items);
end

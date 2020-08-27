local api = vim.api
local M = {}
local data_dir = os.getenv("HOME") ..'/.cache/vim'

-- set options
function setOptions(options)
  for k, v in pairs(options) do
    if v == true or v == false then
      vim.api.nvim_command('set ' .. k)
    elseif type(v) == 'table' then
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
      vim.api.nvim_command('set ' .. k .. '=' .. v)
    end
  end
end

function M.load_options()
  local options = {
    mouse = "nv";
    report = 0;
    errorbells = true;
    visualbell = true;
    hidden = true;
    fileformats = {'unix','mac','dos'};
    magic = true;
    virtualedit = "block";
    synmaxcol = 2500;
    formatoptions = "1jcroql";
    encoding = "utf-8";
    viewoptions = "folds,cursor,curdir,slash,unix";
    sessionoptions = "curdir,help,tabpages,winsize";
    clipboard = "unnamedplus";
    wildignorecase = true;
    wildignore = {'.git','.hg','.svn','*.pyc','*.o','*.out','*.jpg','*.jpeg','*.png','*.gif','*.zip','**/tmp/**','*.DS_Store','**/node_modules/**','**/bower_modules/**'};
    nobackup = true;
    nowritebackup = true;
    undofile = true;
    noswapfile = true;
    directory = data_dir .. "/swag/";
    undodir = data_dir .. "/undo/";
    backupdir = data_dir .. "/backup/";
    viewdir = data_dir .."/view/";
    spellfile = data_dir .."/spell/en.uft-8.add";
    history = 2000;
    shada = "!,'300,<50,@100,s10,h";
    backupskip = {'/tmp/*','$TMPDIR/*','$TMP/*','$TEMP/*','*/shm/*','/private/var/*','.vault.vim'};

    textwidth = 80;
    noexpandtab = true;
    tabstop = 2;
    shiftwidth = 2;
    softtabstop = -1;
    smarttab = true;
    autoindent = true;
    shiftround = true;
    breakindentopt = "shift:2,min:20";

    timeout = true;
    ttimeout = true;
    timeoutlen = 500;
    ttimeoutlen = 10;
    updatetime = 100;
    redrawtime = 1500;

    ignorecase = true;
    smartcase = true;
    infercase = true;
    incsearch = true;
    wrapscan = true;

    complete = ".,w,b,k";
    inccommand = "nosplit";

    grepformat = "%f:%l:%c:%m";
    grepprg = [[rg\ --hidden\ --vimgrep\ --smart-case\ --]];

    nowrap = true;
    linebreak = true;
    breakat = [[\ \	;:,!?]];
    nostartofline = true;
    whichwrap = "h,l,<,>,[,],~";
    splitbelow = true;
    splitright = true;
    switchbuf = "useopen";
    backspace = "indent,eol,start";
    diffopt = {'filler','iwhite','internal','algorithm:patience'};
    completeopt = {'menu','menuone','noselect','noinsert'};
    jumpoptions = "stack";

    noshowmode = true;
    shortmess = "aoOTIcF";
    scrolloff = 2;
    sidescrolloff = 5;
    noruler = true;
    list = true;

    showtabline = 2;
    winwidth = 30;
    winminwidth = 10;
    pumheight =15;
    helpheight = 12;
    previewheight = 12;

    number  = true;
    noshowcmd = true;
    cmdheight = 2;
    cmdwinheight = 5;
    noequalalways = true;
    laststatus = 2;
    colorcolumn = "+0";
    display = "lastline";

    foldenable = true;
    foldmethod = "indent";
    foldlevelstart = 99;

    signcolumn = "yes";
    showbreak = "↳  ";
    listchars = {"tab:»·","nbsp:+","trail:·","extends:→","precedes:←"};
    conceallevel = 2;
    concealcursor = "niv";
    termguicolors = true;
    pumblend = 10;
    winblend = 10;
  };
  setOptions(options);
  vim.g.clipboard = [[{'name':'macOS-clipboard','copy':{'*':'pbcopy','+':'pbcopy'},'paste':{'+':'pbpaste','*':'pbpaste'},'cache_enabled':0}]];
end

M.load_options()

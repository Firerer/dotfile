local opt = vim.opt

opt.updatetime = 50 -- faster completion

opt.nu = true
opt.number = true -- set numbered lines
opt.relativenumber = true -- set relative numbered lines
opt.numberwidth = 2 -- set number column width to 2 {default 4}

opt.wrap = false -- display lines as one long line

-- indent
opt.tabstop = 2 -- insert 2 spaces for a tab
opt.softtabstop = 2
opt.shiftwidth = 2 -- the number of spaces inserted for each indentation
opt.expandtab = true -- convert tabs to spaces
opt.smartindent = true -- make indenting after '{' etc

opt.hlsearch = false -- highlight all matches on previous search pattern
opt.incsearch = true

opt.termguicolors = true -- set term gui colors (most terminals support this)
opt.colorcolumn = "80" -- fixes indentline for now

opt.clipboard = "unnamedplus" -- allows neovim to access the system clipboard
opt.cmdheight = 1 -- more space in the neovim command line for displaying messages
opt.conceallevel = 0 -- so that `` is visible in markdown files
opt.fileencoding = "utf-8" -- the encoding written to a file
opt.foldmethod = "manual" -- folding set to "expr" for treesitter based folding
opt.foldexpr = "" -- set to "nvim_treesitter#foldexpr()" for treesitter based folding
opt.guifont = "monospace:h17" -- the font used in graphical neovim applications
opt.hidden = true -- required to keep multiple buffers and open multiple buffers
opt.ignorecase = true -- ignore case in search patterns
opt.mouse = "a" -- allow the mouse to be used in neovim
opt.pumheight = 10 -- pop up menu height
opt.showmode = false -- we don't need to see things like -- INSERT -- anymore
opt.showtabline = 2 -- always show tabs
opt.smartcase = true -- smart case
opt.splitbelow = true -- force all horizontal splits to go below current window
opt.splitright = true -- force all vertical splits to go to the right of current window
opt.title = true -- set the title of window to the value of the titlestring
-- opt.opt.titlestring = "%<%F%=%l/%L - nvim" -- what the title of the window will be set to
opt.writebackup = false -- if a file is being edited by another program (or was written to file while editing with another program) it is not allowed to be edited
opt.cursorline = true -- highlight the current line
opt.signcolumn = "yes" -- always show the sign column otherwise it would shift the text each time

opt.scrolloff = 8 -- minimal number of screen lines to keep above and below the cursor.
opt.sidescrolloff = 8 -- minimal number of screen lines to keep left and right of the cursor.

---  SETTINGS  ---
vim.opt.shortmess:append "c" -- don't show redundant messages from ins-completion-menu
vim.opt.shortmess:append "I" -- don't show the default intro message
vim.opt.whichwrap:append "<,>,[,],h,l"

opt.swapfile = false -- creates a swapfile
opt.backup = false -- creates a backup file
opt.undofile = true -- enable persistent undo

vim.cmd [[ colorscheme desert ]]

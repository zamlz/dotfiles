--[[--------------------------------------------------------------------------
  ______     __                              
 /_  __/__  / /__  ______________  ____  ___ 
  / / / _ \/ / _ \/ ___/ ___/ __ \/ __ \/ _ \
 / / /  __/ /  __(__  ) /__/ /_/ / /_/ /  __/
/_/  \___/_/\___/____/\___/\____/ .___/\___/ 
                               /_/           
--------------------------------------------------------------------------]]--

require('common.keybinder')

-- FIXME: Configure more of telescope
nmap('<leader>ff', ":Telescope find_files<CR>")
nmap('<leader>sg', ":Telescope live_grep<CR>")
nmap('<leader>bb', ":Telescope buffers<CR>")
nmap('<leader>th', ":Telescope help_tags<CR>")

-- LSP stuff
nmap('<leader>lr', ":Telescope lsp_references theme=dropdown<CR>")

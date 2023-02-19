--[[--------------------------------------------------------------------------
    _   __           _______ __
   / | / /__  ____  / ____(_) /_
  /  |/ / _ \/ __ \/ / __/ / __/
 / /|  /  __/ /_/ / /_/ / / /_
/_/ |_/\___/\____/\____/_/\__/

--------------------------------------------------------------------------]]--

require('common.keybinder')
nmap("<leader>g", ":Neogit<CR>")

return {
    'TimUntersberger/neogit',
    dependencies = { 'nvim-lua/plenary.nvim', 'sindrets/diffview.nvim' },
    opts = {
        integrations = {
            diffview = true
        }
    }
}

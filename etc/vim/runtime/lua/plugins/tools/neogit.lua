--[[--------------------------------------------------------------------------
    _   __           _______ __
   / | / /__  ____  / ____(_) /_
  /  |/ / _ \/ __ \/ / __/ / __/
 / /|  /  __/ /_/ / /_/ / / /_
/_/ |_/\___/\____/\____/_/\__/

--------------------------------------------------------------------------]]--

return {
    'TimUntersberger/neogit',
    keys = {
        { "<leader>g", "<cmd>Neogit<cr>", desc = "NeoGit" },
    },
    dependencies = { 'nvim-lua/plenary.nvim', 'sindrets/diffview.nvim' },
    opts = {
        integrations = {
            diffview = true
        }
    }
}

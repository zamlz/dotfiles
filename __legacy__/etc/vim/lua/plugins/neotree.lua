--[[--------------------------------------------------------------------------
    _   __           __               
   / | / /__  ____  / /_________  ___ 
  /  |/ / _ \/ __ \/ __/ ___/ _ \/ _ \
 / /|  /  __/ /_/ / /_/ /  /  __/  __/
/_/ |_/\___/\____/\__/_/   \___/\___/ 
                                      
--------------------------------------------------------------------------]]--

-- Unless you are still migrating, remove the deprecated commands from v1.x
vim.g.neo_tree_remove_legacy_commands = 1

return {
    "nvim-neo-tree/neo-tree.nvim",
    keys = {
        { "<leader>d", "<cmd>Neotree toggle<cr>", desc = "NeoTree" },
    },
    branch = "v2.x",
    dependencies = { 
        "nvim-lua/plenary.nvim",
        "nvim-tree/nvim-web-devicons",
        "MunifTanjim/nui.nvim",
    }
}

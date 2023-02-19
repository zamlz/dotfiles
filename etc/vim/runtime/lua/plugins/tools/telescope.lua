--[[--------------------------------------------------------------------------
  ______     __                              
 /_  __/__  / /__  ______________  ____  ___ 
  / / / _ \/ / _ \/ ___/ ___/ __ \/ __ \/ _ \
 / / /  __/ /  __(__  ) /__/ /_/ / /_/ /  __/
/_/  \___/_/\___/____/\___/\____/ .___/\___/ 
                               /_/           
--------------------------------------------------------------------------]]--

return {
    'nvim-telescope/telescope.nvim',
    keys = {
        -- FIXME: Configure more of telescope
        { "<leader>ff", "<cmd>Telescope find_files<cr>", desc = "Find Files" },
        { "<leader>sg", "<cmd>Telescope live_grep<cr>", desc = "Grep" },
        { "<leader>bb", "<cmd>Telescope buffers<cr>", desc = "Buffers" },
        { "<leader>th", "<cmd>Telescope help_tags<cr>", desc = "Help Tags" },
        -- LSP stuff
        { "<leader>lr", "<cmd>Telescope lsp_references theme=dropdown<cr>", desc = "LSP References" },
    },
    tag = '0.1.0',
    dependencies = 'nvim-lua/plenary.nvim'
}

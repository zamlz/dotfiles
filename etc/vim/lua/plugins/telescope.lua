--[[--------------------------------------------------------------------------
  ______     __                              
 /_  __/__  / /__  ______________  ____  ___ 
  / / / _ \/ / _ \/ ___/ ___/ __ \/ __ \/ _ \
 / / /  __/ /  __(__  ) /__/ /_/ / /_/ /  __/
/_/  \___/_/\___/____/\___/\____/ .___/\___/ 
                               /_/           
--------------------------------------------------------------------------]]--

vim.api.nvim_set_hl(0, 'TelescopeSelection', {ctermbg='black'})
vim.api.nvim_set_hl(0, 'TelescopePreviewLine', {ctermbg='black'})

return {
    'nvim-telescope/telescope.nvim',
    lazy = false,
    keys = {
        -- FIXME: Configure more of telescope
        { "<leader>ff", "<cmd>Telescope find_files<cr>", desc = "Find Files" },
        { "<leader>sg", "<cmd>Telescope live_grep<cr>", desc = "Grep" },
        { "<leader>bb", "<cmd>Telescope buffers<cr>", desc = "Buffers" },
        { "<leader>th", "<cmd>Telescope help_tags<cr>", desc = "Help Tags" },
    },
    tag = '0.1.0',
    dependencies = {
        'nvim-lua/plenary.nvim'
    },
}

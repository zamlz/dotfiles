--[[--------------------------------------------------------------------------
    _   __
   / | / /__  ____  _________ _
  /  |/ / _ \/ __ \/ ___/ __ `/
 / /|  /  __/ /_/ / /  / /_/ /
/_/ |_/\___/\____/_/   \__, /
                      /____/
--------------------------------------------------------------------------]]--

return {
    'nvim-neorg/neorg',
    run = ":Neorg sync-parsers",
    dependencies = "nvim-lua/plenary.nvim",
    ft = 'norg',
    opts = {
        load = {
            ["core.defaults"] = {}, -- Loads default behaviour
            ["core.norg.dirman"] = { -- Manages Neorg workspaces
                config = {
                    workspaces = {
                        docs = "~/usr/documents",
                        work = "~/usr/work"
                    },
                },
                index = "index.norg",
                open_last_workspace = true
            },
            ["core.norg.concealer"] = {}, -- Adds pretty icons to your documents
        },
    }
}

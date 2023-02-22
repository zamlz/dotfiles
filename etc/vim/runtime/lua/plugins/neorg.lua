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
    -- ft = 'norg',
    opts = {
        load = {
            -- Loads default behaviour
            ["core.defaults"] = {},
            -- Adds pretty icons to your documents
            ["core.norg.concealer"] = {
                config = {
                    icon_preset = 'diamond'
                }
            },
            -- Manages Neorg workspaces
            ["core.norg.dirman"] = {
                config = {
                    workspaces = {
                        notes= "~/usr/notes",
                        work = "~/usr/work",
                    },
                    default_workspace = 'notes'
                },
                index = "index.norg",
                open_last_workspace = true
            },
        },
    }
}

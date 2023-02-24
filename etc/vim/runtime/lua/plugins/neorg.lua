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
    keys = {
        { "<leader>nn", "<cmd>Neorg index<cr>", desc = "Neorg Index" },
    },
    opts = {
        load = {
            -- Loads default behaviour
            ["core.defaults"] = {},
            -- Default keybinds for now
            ["core.keybinds"] = {
                config = {
                    default_keybinds = true,
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
            -- Configure a journal
            ["core.norg.journal"] = {},
            -- Adds pretty icons to your documents
            ["core.norg.concealer"] = {
                config = {
                    icon_preset = 'diamond'
                }
            },
            -- QOL plugin for copying codeblocks
            ["core.clipboard.code-blocks"] = {},
            -- List iteration helper
            ["core.itero"] = {},
            -- Enable exporting to other file formats
            ["core.export"] = {},
        },
    }
}

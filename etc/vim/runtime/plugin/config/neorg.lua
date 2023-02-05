--[[--------------------------------------------------------------------------
    _   __
   / | / /__  ____  _________ _
  /  |/ / _ \/ __ \/ ___/ __ `/
 / /|  /  __/ /_/ / /  / /_/ /
/_/ |_/\___/\____/_/   \__, /
                      /____/
--------------------------------------------------------------------------]]--

require('neorg').setup {
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

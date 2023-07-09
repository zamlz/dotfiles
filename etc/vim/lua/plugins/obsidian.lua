--[[--------------------------------------------------------------------------
   ____  __         _     ___           
  / __ \/ /_  _____(_)___/ (_)___ _____ 
 / / / / __ \/ ___/ / __  / / __ `/ __ \
/ /_/ / /_/ (__  ) / /_/ / / /_/ / / / /
\____/_.___/____/_/\__,_/_/\__,_/_/ /_/ 
                                        
--------------------------------------------------------------------------]]--

return  {
    'epwalsh/obsidian.nvim',
    lazy = true,
    event = { "BufReadPre " .. vim.fn.expand "~" .. "/usr/notes/**.md" },
    dependencies = {
        -- Required.
        "nvim-lua/plenary.nvim",
        -- Optional, for completion.
        "hrsh7th/nvim-cmp",
        -- Optional, for search and quick-switch functionality.
        "nvim-telescope/telescope.nvim",
    },
    opts = {
        dir = "~/usr/notes",
        -- Optional, set the log level for Obsidian. This is an integer corresponding to one of the log
        -- levels defined by "vim.log.levels.*" or nil, which is equivalent to DEBUG (1).
        log_level = vim.log.levels.DEBUG,
        -- Optional, completion.
        completion = {
            -- If using nvim-cmp, otherwise set to false
            nvim_cmp = true,
            -- Trigger completion at 2 chars
            min_chars = 2,
            -- Where to put new notes created from completion. Valid options are
            --  * "current_dir" - put new notes in same directory as the current buffer.
            --  * "notes_subdir" - put new notes in the default notes subdirectory.
            new_notes_location = "notes_subdir"
        },

        -- Optional, for templates (see below).
        templates = {
            subdir = "templates",
            date_format = "%Y-%m-%d",
            time_format = "%H:%M",
        },

        -- Optional, set to true if you use the Obsidian Advanced URI plugin.
        -- https://github.com/Vinzent03/obsidian-advanced-uri
        use_advanced_uri = false,
        -- Optional, set to true to force ':ObsidianOpen' to bring the app to the foreground.
        open_app_foreground = false,

        -- Optional, by default commands like `:ObsidianSearch` will attempt to use
        -- telescope.nvim, fzf-lua, and fzf.nvim (in that order), and use the
        -- first one they find. By setting this option to your preferred
        -- finder you can attempt it first. Note that if the specified finder
        -- is not installed, or if it the command does not support it, the
        -- remaining finders will be attempted in the original order.
        finder = "telescope.nvim",

        -- Optional, by default when you use `:ObsidianFollowLink` on a link to an external
        -- URL it will be ignored but you can customize this behavior here.
        follow_url_func = function(url)
            -- Open the URL in the default web browser.
            vim.fn.jobstart({"xdg-open", url})
        end,

        -- Optional, customize how names/IDs for new notes are created.
        note_id_func = function(title)
            -- Create note IDs in a Zettelkasten format with a timestamp and a suffix.
            -- In this case a note with the title 'My new note' will given an ID that looks
            -- like '1657296016-my-new-note', and therefore the file name '1657296016-my-new-note.md'
            local suffix = ""
            if title ~= nil then
                -- If title is given, transform it into valid file name.
                suffix = title:gsub(" ", "-"):gsub("[^A-Za-z0-9-]", ""):lower()
            else
                -- If title is nil, just add 4 random uppercase letters to the suffix.
                for _ = 1, 4 do
                    suffix = suffix .. string.char(math.random(65, 90))
                end
            end
            return tostring(os.time()) .. "-" .. suffix
        end,

        -- Optional, alternatively you can customize the frontmatter data.
        note_frontmatter_func = function(note)
            -- This is equivalent to the default frontmatter function.
            local out = { id = note.id, aliases = note.aliases, tags = note.tags }
            -- `note.metadata` contains any manually added fields in the frontmatter.
            -- So here we just make sure those fields are kept in the frontmatter.
            if note.metadata ~= nil and require("obsidian").util.table_length(note.metadata) > 0 then
                for k, v in pairs(note.metadata) do
                    out[k] = v
                end
            end
            return out
        end,
    },
    config = function(_, opts)
        require("obsidian").setup(opts)
        -- standard '/' will search if no other key is pressed
        vim.keymap.set("n", "<leader>/", "<cmd>ObsidianSearch<CR>")
        vim.keymap.set("n", "<leader>/b", "<cmd>ObsidianBacklinks<CR>")
        -- quick switch to journal entries for today and tommorow
        vim.keymap.set("n", "<leader>/d", "<cmd>ObsidianToday<CR>")
        vim.keymap.set("n", "<leader>/D", "<cmd>ObsidianYesterday<CR>")
        -- This actually opens up the note file you're on in the Obsidian app
        vim.keymap.set("n", "<leader>/o", "<cmd>ObsidianOpen<CR>")
        -- creates a totally new document with a random name, kinda useless
        -- atm because it's very different name
        vim.keymap.set("n", "<leader>/n", "<cmd>ObsidianNew<CR>")
        -- Link creation
        vim.keymap.set("v", "<leader>/l", "<cmd>ObsidianLinkNew<CR>")
        vim.keymap.set("v", "<leader>/L", "<cmd>ObsidianLink<CR>")
        -- Uses vim standard gf to follow links
        vim.keymap.set("n", "gf", function()
            if require("obsidian").util.cursor_on_markdown_link() then
                return "<cmd>ObsidianFollowLink<CR>"
            else
                return "gf"
            end
        end, { noremap = false, expr = true })
    end,
}

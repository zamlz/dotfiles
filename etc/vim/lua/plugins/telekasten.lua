--[[--------------------------------------------------------------------------
  ______     __     __              __
 /_  __/__  / /__  / /______ ______/ /____  ____
  / / / _ \/ / _ \/ //_/ __ `/ ___/ __/ _ \/ __ \
 / / /  __/ /  __/ ,< / /_/ (__  ) /_/  __/ / / /
/_/  \___/_/\___/_/|_|\__,_/____/\__/\___/_/ /_/

--------------------------------------------------------------------------]]--

-- custom colorscheme
vim.cmd("hi tkLink ctermfg=DarkBlue cterm=bold,underline guifg=blue gui=bold,underline")
vim.cmd("hi tkBrackets ctermfg=gray guifg=gray")
vim.cmd("hi tkHighlight ctermbg=yellow ctermfg=darkred cterm=bold guibg=yellow guifg=darkred gui=bold")
vim.cmd("hi link CalNavi CalRuler")
vim.cmd("hi tkTagSep ctermfg=gray guifg=gray")
vim.cmd("hi tkTag ctermfg=175 guifg=#d3869B")

-- Launch panel if nothing is typed after <leader>z
vim.keymap.set("n", "<leader>/", "<cmd>Telekasten panel<CR>")

-- Most used functions
vim.keymap.set("n", "<leader>/f", "<cmd>Telekasten find_notes<CR>")
vim.keymap.set("n", "<leader>/g", "<cmd>Telekasten search_notes<CR>")
vim.keymap.set("n", "<leader>/d", "<cmd>Telekasten goto_today<CR>")
vim.keymap.set("n", "<leader>/z", "<cmd>Telekasten follow_link<CR>")
vim.keymap.set("n", "<leader>/n", "<cmd>Telekasten new_note<CR>")
vim.keymap.set("n", "<leader>/c", "<cmd>Telekasten show_calendar<CR>")
vim.keymap.set("n", "<leader>/b", "<cmd>Telekasten show_backlinks<CR>")
vim.keymap.set("n", "<leader>/I", "<cmd>Telekasten insert_img_link<CR>")

-- Call insert link automatically when we start typing a link
vim.keymap.set("i", "[[", "<cmd>Telekasten insert_link<CR>")

-- specify the notes directory
local home = vim.fn.expand("~/usr/notes")

return {
    'renerocksai/telekasten.nvim',
    dependencies = {'nvim-telescope/telescope.nvim'},
    opts = {
        -- dir names for special notes (absolute path or subdir name)
        home         = home,
        dailies      = home .. '/journal/daily',
        weeklies     = home .. '/journal/weekly',
        templates    = home .. '/templates',


        -- if true, telekasten will be enabled when opening a note within the configured home
        take_over_my_home = true,

        -- template for new notes (new_note, follow_link)
        -- set to `nil` or do not specify if you do not want a template
        template_new_note   = home .. '/templates/new_note.md',
        template_new_daily  = home .. '/templates/daily.md',
        template_new_weekly = home .. '/templates/weekly.md',

        -- image (sub)dir for pasting
        -- dir name (absolute path or subdir name)
        -- or nil if pasted images shouldn't go into a special subdir
        image_subdir = "data",

        -- markdown file extension
        extension    = ".md",

        -- auto-set telekasten filetype: if false, the telekasten filetype will not be used
        --                               and thus the telekasten syntax will not be loaded either
        auto_set_filetype = true,

        -- Generate note filenames. One of:
        -- "title" (default) - Use title if supplied, uuid otherwise
        -- "uuid" - Use uuid
        -- "uuid-title" - Prefix title by uuid
        -- "title-uuid" - Suffix title with uuid
        new_note_filename = "uuid-title",
        -- file uuid type ("rand" or input for os.date such as "%Y%m%d%H%M")
        uuid_type = "%Y%m%d%H%M",
        -- UUID separator
        uuid_sep = "-",
        -- space substitution
        filename_space_subst = '-',

        -- following a link to a non-existing note will create it
        follow_creates_nonexisting = true,
        dailies_create_nonexisting = true,
        weeklies_create_nonexisting = true,

        -- image link style
        -- wiki:     ![[image name]]
        -- markdown: ![](image_subdir/xxxxx.png)
        image_link_style = "markdown",

        -- Default sort option: 'filename', 'modified'
        sort = "filename",

        -- Make syntax available to markdown buffers and telescope previewers
        install_syntax = true,

        -- tag notation: '#tag', ':tag:', 'yaml-bare'
        tag_notation = "yaml-bare",

        -- when linking to a note in subdir/, create a [[subdir/title]] link
        -- instead of a [[title only]] link
        subdirs_in_links = true,

        -- command palette theme: dropdown (window) or ivy (bottom panel)
        command_palette_theme = "dropdown",

        -- tag list theme:
        -- get_cursor: small tag list at cursor; ivy and dropdown like above
        show_tags_theme = "dropdown",

        -- Previewer for media files (images mostly)
        -- "telescope-media-files" if you have telescope-media-files.nvim installed
        -- "catimg-previewer" if you have catimg installed
        -- "viu-previewer" if you have viu installed
        media_previewer = "telescope-media-files",

        -- integrate with calendar-vim
        plug_into_calendar = true,
        calendar_opts = {
            -- calendar week display mode: 1 .. 'WK01', 2 .. 'WK 1', 3 .. 'KW01', 4 .. 'KW 1', 5 .. '1'
            weeknm = 4,
            -- use monday as first day of week: 1 .. true, 0 .. false
            calendar_monday = 1,
            -- calendar mark: where to put mark for marked days: 'left', 'right', 'left-fit'
            calendar_mark = 'left-fit',
        },

        -- telescope actions behavior
        close_after_yanking = false,
        insert_after_inserting = true,

        -- template_handling
        -- What to do when creating a new note via `new_note()` or `follow_link()`
        -- to a non-existing note
        -- - prefer_new_note: use `new_note` template
        -- - smart: if day or week is detected in title, use daily / weekly templates (default)
        -- - always_ask: always ask before creating a note
        template_handling = "smart",

        -- path handling:
        --   this applies to:
        --     - new_note()
        --     - new_templated_note()
        --     - follow_link() to non-existing note
        --
        --   it does NOT apply to:
        --     - goto_today()
        --     - goto_thisweek()
        --
        --   Valid options:
        --     - smart: put daily-looking notes in daily, weekly-looking ones in weekly,
        --              all other ones in home, except for notes/with/subdirs/in/title.
        --              (default)
        --
        --     - prefer_home: put all notes in home except for goto_today(), goto_thisweek()
        --                    except for notes with subdirs/in/title.
        --
        --     - same_as_current: put all new notes in the dir of the current note if
        --                        present or else in home
        --                        except for notes/with/subdirs/in/title.
        new_note_location = "smart",

        -- should all links be updated when a file is renamed
        rename_update_links = true,
    }
}

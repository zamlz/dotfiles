--[[--------------------------------------------------------------------------
   ____                                       __   
  / __ \_________ _      ____ ___  ____  ____/ /__ 
 / / / / ___/ __ `/_____/ __ `__ \/ __ \/ __  / _ \
/ /_/ / /  / /_/ /_____/ / / / / / /_/ / /_/ /  __/
\____/_/   \__, /     /_/ /_/ /_/\____/\__,_/\___/ 
          /____/                                   
--------------------------------------------------------------------------]]--

return {
    'nvim-orgmode/orgmode',
    dependencies = {
        { 'akinsho/org-bullets.nvim' },
        { 'dhruvasagar/vim-table-mode' },
        { 'nvim-treesitter/nvim-treesitter', lazy = true },
    },
    config = function()
        -- load ts grammar -> load ts -> load org mode
        require('orgmode').setup_ts_grammar()
        require('nvim-treesitter.configs').setup({
            highlight = {
                enable = true,
                additional_vim_regex_highlighting = { 'org' },
            },
            ensure_installed = { 'org' },
        })
        require('orgmode').setup({
            org_agenda_files = '~/usr/org/**/*',
            org_default_notes_file = '~/usr/org/refile.org',
            org_todo_keywords = {'TODO(t)', 'NEXT(n)', '|', 'DONE(d)'},
            org_todo_keyword_faces = {
                TODO = ':weight bold :foreground white :background darkred',
                NEXT = ':weight bold :foreground white :background darkblue',
                DONE = ':weight bold :foreground white :background darkgreen',
            },
            win_split_mode = 'float',
            org_startup_folded = 'showeverything',
            org_indent_mode = 'indent',
            org_hide_emphasis_markers = true,
            org_ellipsis = ' >',
            org_agenda_start_on_weekday = 0,
            calendar_week_start_day = 0,
        })
        require('org-bullets').setup { concealcursor = true }
    end,
}

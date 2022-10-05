--[[--------------------------------------------------------------------------
    __                __    _          
   / /   __  ______ _/ /   (_)___  ___ 
  / /   / / / / __ `/ /   / / __ \/ _ \
 / /___/ /_/ / /_/ / /___/ / / / /  __/
/_____/\__,_/\__,_/_____/_/_/ /_/\___/ 
--------------------------------------------------------------------------]]--

require('lualine').setup {
    options = {
        icons_enabled = true,
        theme = 'powerline_dark',
        --component_separators = { left = '\ue0b1', right = '\ue0b3' },
        --section_separators = { left = '\ue0b0', right = '\ue0b2' },
        disabled_filetypes = {
            statusline = {},
            winbar = {},
        },
        ignore_focus = {},
        always_divide_middle = true,
        globalstatus = false,
        refresh = {
            statusline = 1000,
            tabline = 1000,
            winbar = 1000,
        }
    },
    sections = {
        lualine_a = {'mode'},
        lualine_b = {'branch', 'diff', 'diagnostics'},
        lualine_c = {'filename'},
        lualine_x = {'encoding', 'fileformat', 'filetype'},
        lualine_y = {'progress'},
        lualine_z = {'location'}
    },
    inactive_sections = {
        lualine_a = {},
        lualine_b = {},
        lualine_c = {'filename'},
        lualine_x = {'location'},
        lualine_y = {},
        lualine_z = {}
    },
    tabline = {},
    winbar = {},
    inactive_winbar = {},
    extensions = {}
}

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
        theme = 'codedark',
        component_separators = { left = '', right = ''},
        section_separators = { left = '', right = ''},
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
        lualine_b = {
            'branch',
            {
                'filename',
                path = 1,
            }
        },
        lualine_c = {'diff', 'diagnostics'},
        lualine_x = {'encoding', 'filetype'},
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
    tabline = {
        lualine_a = {
            {
                'buffers',
                symbols = {
                    modified = ' ●',
                    alternate_file = '',
                    directory =  ''
                },
            }
        },
        lualine_b = {},
        lualine_c = {},
        lualine_x = {},
        lualine_y = {},
        lualine_z = {'tabs'}
    },
    winbar = {
        --[[ FIXME: What is winbar?
        lualine_a = {},
        lualine_b = {},
        lualine_c = {'filename'},
        lualine_x = {},
        lualine_y = {},
        lualine_z = {}    
        --]]
    },
    inactive_winbar = {
        --[[ FIXME: What is inactive winbar?
        lualine_a = {},
        lualine_b = {},
        lualine_c = {'filename'},
        lualine_x = {},
        lualine_y = {},
        lualine_z = {}
        --]]
    },
    extensions = {}
}

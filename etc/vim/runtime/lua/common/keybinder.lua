--[[--------------------------------------------------------------------------
    __ __           __    _           __         
   / //_/__  __  __/ /_  (_)___  ____/ /__  _____
  / ,< / _ \/ / / / __ \/ / __ \/ __  / _ \/ ___/
 / /| /  __/ /_/ / /_/ / / / / / /_/ /  __/ /    
/_/ |_\___/\__, /_.___/_/_/ /_/\__,_/\___/_/     
          /____/                                 
--------------------------------------------------------------------------]]--

-- Keybinding helper functions

function map(mode, shortcut, command)
    vim.api.nvim_set_keymap(
        mode,
        shortcut,
        command,
        { noremap = true, silent = true }
    )
end

function nmap(shortcut, command)
    map('n', shortcut, command)
end

function imap(shortcut, command)
    map('i', shortcut, command)
end

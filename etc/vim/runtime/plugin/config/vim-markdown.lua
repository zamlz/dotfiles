--[[--------------------------------------------------------------------------
 _    ________  ___   __  ___           __       __
| |  / /  _/  |/  /  /  |/  /___ ______/ /______/ /___ _      ______
| | / // // /|_/ /  / /|_/ / __ `/ ___/ //_/ __  / __ \ | /| / / __ \
| |/ // // /  / /  / /  / / /_/ / /  / ,< / /_/ / /_/ / |/ |/ / / / /
|___/___/_/  /_/  /_/  /_/\__,_/_/  /_/|_|\__,_/\____/|__/|__/_/ /_/
--------------------------------------------------------------------------]]--

-- Enable programming language syntax highlighting in markdown files
vim.g.markdown_syntax_conceal = 0
vim.g.markdown_minlines = 100
vim.g.markdown_fenced_languages = {
    'html',
    'yaml',
    'python',
    'bash=sh',
    'rust'
}

-- Let's also theme the markdown syntax

-- Headers
color_priority = {
    'darkred',
    'darkblue',
    'darkgreen',
    'darkcyan',
    'darkmagenta',
    'darkyellow'
}

for i = 1, 6 do
    vim.cmd("highlight markdownH"..i.." ctermfg="..color_priority[i])
    vim.cmd("highlight markdownH"..i.."Delimiter ctermfg="..color_priority[i])
end

vim.cmd("highlight markdownLinkText ctermfg=darkblue")
vim.cmd("highlight markdownUrl ctermfg=darkblue")

vim.cmd("highlight markdownCodeDelimiter ctermfg=darkred")

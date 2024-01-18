--[[--------------------------------------------------------------------------
    _   __      _              ______                    _ __  __
   / | / /   __(_)___ ___     /_  __/_______  ___  _____(_) /_/ /____  _____
  /  |/ / | / / / __ `__ \     / / / ___/ _ \/ _ \/ ___/ / __/ __/ _ \/ ___/
 / /|  /| |/ / / / / / / /    / / / /  /  __/  __(__  ) / /_/ /_/  __/ /
/_/ |_/ |___/_/_/ /_/ /_/    /_/ /_/   \___/\___/____/_/\__/\__/\___/_/

--------------------------------------------------------------------------]]--

-- Let's also theme the markdown syntax (using treesitter highlighting)
-- Headers
local color_priority = {
    'darkred',
    'darkblue',
    'darkgreen',
    'darkcyan',
    'darkmagenta',
    'darkyellow'
}
for i = 1, 6 do
    vim.api.nvim_set_hl(0, '@text.title.'..i..'.markdown', {ctermfg=color_priority[i]})
    vim.api.nvim_set_hl(0, '@text.title.'..i..'.marker.markdown', {ctermfg=color_priority[i]})
end
vim.api.nvim_set_hl(0, '@punctuation.bracket.markdown_inline', {ctermfg='darkmagenta'})
vim.api.nvim_set_hl(0, '@text.reference.markdown_inline', {ctermfg='darkmagenta'})
vim.api.nvim_set_hl(0, '@text.uri.markdown_inline', {ctermfg='darkmagenta'})
vim.api.nvim_set_hl(0, 'Todo', {ctermfg='darkyellow', ctermbg='black'})

return {
    'nvim-treesitter/nvim-treesitter',
    build = ':TSUpdate',
    event = { "BufReadPost", "BufNewFile" },
    opts = {
        highlight = {
            enable = true,
            additional_vim_regex_highlighting = { "markdown" },
        },
        indent = { enable = true},
        auto_install = true,
        sync_install = false,
        ensure_installed = {
            "awk",
            "bash",
            "beancount",
            "bibtex",
            "c",
            "cpp",
            "css",
            "diff",
            "dockerfile",
            "gitattributes",
            "gitcommit",
            "gitignore",
            "git_rebase",
            "help",
            "html",
            "ini",
            "java",
            "javascript",
            "json",
            "julia",
            "latex",
            "lua",
            "make",
            "markdown",
            "markdown_inline",
            "python",
            "regex",
            "rust",
            "scheme",
            "sql",
            "sxhkdrc",
            "toml",
            "typescript",
            "vim",
            "yaml"
        },
        ignore_install = {},
    },
    config = function(_, opts)
        require("nvim-treesitter.configs").setup(opts)
    end,
}


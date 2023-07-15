--[[--------------------------------------------------------------------------
    ________            __ ______
   / ____/ /___  ____ _/ //_  __/__  _________ ___
  / /_  / / __ \/ __ `/ __// / / _ \/ ___/ __ `__ \
 / __/ / / /_/ / /_/ / /_ / / /  __/ /  / / / / / /
/_/   /_/\____/\__,_/\__//_/  \___/_/  /_/ /_/ /_/

--------------------------------------------------------------------------]]--

return {
    'voldikss/vim-floaterm',
    keys = {
        { "<leader>t", "<CMD>FloatermNew --title=Terminal<CR>", desc = "Terminal Session" },
        { "<leader>p", "<CMD>FloatermNew --title=IPython ipython<CR>", desc = "IPython Shell" },
        { "<leader>g", "<CMD>FloatermNew --height=1.0 --width=1.0 --title=LazyGit lazygit<CR>", desc = "LazyGit" },
    },
}

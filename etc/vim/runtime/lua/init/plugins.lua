--[[--------------------------------------------------------------------------
_    ___              ____  __            _
| |  / (_)___ ___     / __ \/ /_  ______ _(_)___  _____
| | / / / __ `__ \   / /_/ / / / / / __ `/ / __ \/ ___/
| |/ / / / / / / /  / ____/ / /_/ / /_/ / / / / (__  )
|___/_/_/ /_/ /_/  /_/   /_/\__,_/\__, /_/_/ /_/____/
                                 /____/
--------------------------------------------------------------------------]]--

--[[
PLUGINS TO TRY:
    - vim-which-key
    - nvim-cmp
    - telescope
    - fzf
--]]

local ensure_packer = function()
  local fn = vim.fn
  local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
  if fn.empty(fn.glob(install_path)) > 0 then
    fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
    vim.cmd [[packadd packer.nvim]]
    return true
  end
  return false
end

local packer_bootstrap = ensure_packer()

return require('packer').startup(function(use)

  -- Packer can manage itself
  use 'wbthomason/packer.nvim'

  -- Aesthetic Status Bar
  use 'vim-airline/vim-airline'
  use 'vim-airline/vim-airline-themes'

  -- Git plugis
  use 'tpope/vim-fugitive'
  use 'airblade/vim-gitgutter'

  -- Undo Tree
  use 'mbbill/undotree'

  -- Nerd Tree
  use 'preservim/nerdtree'

  -- Encrypted File Support
  use 'jamessan/vim-gnupg'

  -- Language Server Support
  use 'neovim/nvim-lspconfig'

  -- Language Based Plugins
  use 'ledger/vim-ledger'
  use 'nathangrigg/vim-beancount'

  -- Automatically set up your configuration after cloning packer.nvim
  -- Put this at the end after all plugins
  if packer_bootstrap then
    require('packer').sync()
  end
end)

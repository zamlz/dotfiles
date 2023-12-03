{ inputs, lib, config, pkgs, ... }: {
  
  imports = [];

  # FIXME: When setting this to the current folder, the plugin system
  #        fails because it is unable to find a C compiler
  # FIXME: Is there a better way to represent the current folder?
  #xdg.configFile.nvim.source = ../neovim;
  
  programs.neovim = {
    enable = true;
    defaultEditor = true;
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;
    plugins = [
      pkgs.vimPlugins.nvim-treesitter.withAllGrammars
    ];
  };

}

{ inputs, lib, config, pkgs, ... }: {
  programs.fzf= {
    enable = true;
    enableZshIntegration = true;
    tmux.enableShellIntegration = true;
  };
}

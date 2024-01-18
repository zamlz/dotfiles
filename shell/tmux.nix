{ inputs, lib, config, pkgs, ... }: {
  programs.tmux = {
    enable = true;
    prefix = "C-b";
    mouse = true;
    historyLimit = 100000;
    baseIndex = 1;
    keyMode = "vi";
    terminal = "screen-256color";
    plugins = with pkgs; [
      tmuxPlugins.pain-control
    ];
    extraConfig = ''
      set -g set-titles on
      set -g set-titles-string '#T'
      bind r source-file ~/.config/tmux/tmux.conf \; display "Reloaded Tmux Config"
    '';
  };
}

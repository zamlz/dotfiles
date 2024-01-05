{ inputs, lib, config, pkgs, ... }: {

  imports = [];

  programs.waybar = {
    enable = true;
  };
}

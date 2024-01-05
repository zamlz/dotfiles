{ inputs, lib, config, pkgs, ... }: {

  imports = [
    ./sway.nix
    ./hyprland.nix
    ./waybar.nix
  ];
}

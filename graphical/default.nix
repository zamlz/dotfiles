{ inputs, lib, config, pkgs, ... }: {
  imports = [
    ./alacritty.nix
    ./xorg/herbstluftwm.nix
    ./xorg/picom.nix
    ./xorg/polybar.nix
    ./xorg/rofi.nix
    ./xorg/sxhkd.nix
    ./xorg/xinit.nix
    ./wayland
  ];
}


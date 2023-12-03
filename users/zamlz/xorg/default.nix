{ inputs, lib, config, pkgs, ... }: {
  imports = [
    ./herbstluftwm
    ./kitty.nix
    ./picom.nix
    ./polybar
    ./rofi
    ./sxhkd.nix
  ];
}

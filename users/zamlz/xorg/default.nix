{ inputs, lib, config, pkgs, ... }: {
  imports = [
    ./herbstluftwm
    ./kitty.nix
    ./picom.nix
    ./polybar.nix
    ./rofi
    ./sxhkd.nix
  ];
}

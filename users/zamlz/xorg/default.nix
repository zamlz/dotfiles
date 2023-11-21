{ inputs, lib, config, pkgs, ... }: {
  imports = [
    ./herbstluftwm.nix
    ./kitty.nix
    ./picom.nix
    ./polybar.nix
    ./rofi
    ./sxhkd.nix
  ];
}

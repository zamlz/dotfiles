{ inputs, lib, config, pkgs, ... }: let
  sxhkdTerminal = "kitty";
in {
  imports = [];
  services.sxhkd = {
    enable = true;
    keybindings = {
      "super + Return" = "${sxhkdTerminal}";
      # ROFI
      "super + e" = "rofi -show run";
      "super + w" = "rofi -show window";
    };
  };
}

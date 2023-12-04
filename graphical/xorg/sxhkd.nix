{ inputs, lib, config, pkgs, ... }: let
  sxhkdTerminal = "kitty";
in {
  imports = [];
  services.sxhkd = {
    enable = true;
    keybindings = {
      # Core utils
      "super + Return" = "${sxhkdTerminal}";
      "super + e" = "rofi -show run";
      "super + w" = "rofi -show window";
      # FIXME: This configuration should somehow be owned by password-store
      "super + p" = "$HOME/nix/graphical/xorg/rofi/password-store-dmenu.sh";
      "super + shift + p" = "$HOME/nix/graphical/xorg/rofi/password-store-dmenu.sh --qrcode";
      "super + ctrl + alt + Escape" = "$HOME/nix/graphical/xorg/rofi/system-manager.sh";
    };
  };
}

{ inputs, lib, config, pkgs, ... }: let
  sxhkdTerminal = "alacritty";
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
      "super + p" = "$HOME/.config/rofi/password-store-dmenu.sh";
      "super + shift + p" = "$HOME/.config/rofi/password-store-dmenu.sh --qrcode";
      "super + ctrl + alt + Escape" = "$HOME/.config/rofi/system-manager.sh";
    };
  };
}

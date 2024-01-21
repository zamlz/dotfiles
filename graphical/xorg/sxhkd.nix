{ inputs, lib, config, pkgs, ... }: let
  terminal = "${pkgs.alacritty}/bin/alacritty";
  rofi = "${pkgs.rofi}/bin/rofi";
in {
  imports = [];
  services.sxhkd = {
    enable = true;
    keybindings = {
      # Core utils
      "super + Return" = "${terminal}";
      "super + e" = "${rofi} -show run";
      "super + w" = "${rofi} -show window";
      # FIXME: This configuration should somehow be owned by password-store
      "super + p" = "$HOME/.config/rofi/password-store-dmenu.sh";
      "super + shift + p" = "$HOME/.config/rofi/password-store-dmenu.sh --qrcode";
      "super + ctrl + alt + Escape" = "$HOME/.config/rofi/system-manager.sh";
      # Multimedia and Physical Switches
      # "XF86MonBrightnessUp" = "";
      # "XF86MonBrightnessDown" = "";
      # "XF86AudioMute" = "";
      # "XF86AudioMicMute" = "";
      # "XF86AudioRaiseVolume" = "";
      # "XF86AudioLowerVolume" = "";
      # "XF86AudioPlay" = "";
      # "XF86AudioNext" = "";
      # "XF86AudioPrev" = "";
    };
  };
}

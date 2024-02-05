{ inputs, lib, config, pkgs, ... }: let
  terminal = "${pkgs.alacritty}/bin/alacritty";
  rofi = "${pkgs.rofi}/bin/rofi";
  rofiPasswordStoreScript = "$HOME/.config/rofi/password-store-dmenu.sh";
  rofiSystemManagerScript = "$HOME/.config/rofi/system-manager.sh";
  maimScreenshotScript = "$HOME/.config/sxhkd/maim-screenshot.sh";
in {
  xdg.configFile."sxhkd/maim-screenshot.sh".source = ./scripts/maim-screenshot.sh;
  services.sxhkd = {
    enable = true;
    keybindings = {
      # Core utils
      "super + Return" = "${terminal}";
      "super + e" = "${rofi} -show run";
      "super + w" = "${rofi} -show window";
      
      # FIXME: This configuration should somehow be owned by password-store?
      "super + p" = "${rofiPasswordStoreScript}";
      "super + shift + p" = "${rofiPasswordStoreScript} --qrcode";
      "super + ctrl + alt + Escape" = "${rofiSystemManagerScript}";
      
      # Screenshot tool:
      #   Interactively select a window or rectangle with the mouse to take a screen
      #   shot of it. It's important that these keybindings are prefaces with the =@=
      #   token as it implies that the command should be executed on key release as
      #   opposed to key press. Scrot and xclip here will not work properly unless they
      #   are on key release.
      "@Print" = "${maimScreenshotScript} -s";
      "@shift + Print" = "${maimScreenshotScript}";
      
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

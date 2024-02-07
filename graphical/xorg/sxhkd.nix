{ inputs, lib, config, pkgs, ... }: let
  terminal = "${pkgs.alacritty}/bin/alacritty";
  fzfLauncher = script: lineNum: columnNum: fontSize:
    let
      fontOption = "--option 'font.size=${builtins.toString fontSize}'";
      lineOption = "--option 'window.dimensions.lines=${builtins.toString lineNum}'";
      columnOption = "--option 'window.dimensions.columns=${builtins.toString columnNum}'";
    in
    "${terminal} --class 'fzf,fzf' ${fontOption} ${lineOption} ${columnOption} --command ${script}";
  fzfPasswordStoreScript = fzfLauncher "$HOME/.config/sxhkd/fzf-password-store.sh" 10 100 8;
  fzfSystemManagerScript = fzfLauncher "$HOME/.config/sxhkd/fzf-system-manager.sh" 6 40 8;
  maimScreenshotScript = "$HOME/.config/sxhkd/maim-screenshot.sh";
  rofi = "${pkgs.rofi}/bin/rofi";
in {
  xdg.configFile."sxhkd/maim-screenshot.sh".source = ./scripts/maim-screenshot.sh;
  xdg.configFile."sxhkd/fzf-password-store.sh".source = ../../scripts/fzf-password-store.sh;
  xdg.configFile."sxhkd/fzf-system-manager.sh".source = ../../scripts/fzf-system-manager.sh;
  services.sxhkd = {
    enable = true;
    keybindings = {
      # Core utils
      "super + Return" = "${terminal}";
      "super + e" = "${rofi} -show run";
      "super + w" = "${rofi} -show window";
      
      # FIXME: This configuration should somehow be owned by password-store?
      "super + p" = "${fzfPasswordStoreScript}";
      "super + shift + p" = "${fzfPasswordStoreScript} --qrcode";

      # System Controls
      "super + ctrl + alt + Escape" = "${fzfSystemManagerScript}";
      
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

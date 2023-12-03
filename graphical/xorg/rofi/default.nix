{ inputs, lib, config, pkgs, ... }: {
  imports = [];
  xdg.configFile."rofi/theme.rasi".source = ./theme.rasi;
  programs.rofi = let
    # FIXME: unclear why this does not work
    # defualt-theme-path = xdg.configHome + xdg.configFile."rofi/default-theme.rasi".target; 
    default-theme-path = "~/.config/rofi/theme.rasi";
  in {
    enable = true;
    theme = default-theme-path; 
  };
}

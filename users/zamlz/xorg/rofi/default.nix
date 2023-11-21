{ inputs, lib, config, pkgs, ... }: {
  imports = [];
  xdg.configFile."rofi/default-theme.rasi".source = ./default-theme.rasi;
  programs.rofi = let
    # FIXME: unclear why this does not work
    # defualt-theme-path = xdg.configHome + xdg.configFile."rofi/default-theme.rasi".target; 
    default-theme-path = "~/.config/rofi/default-theme.rasi";
  in {
    enable = true;
    theme = default-theme-path; 
  };
}

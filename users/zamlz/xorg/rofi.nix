{ inputs, lib, config, pkgs, ... }: {
  imports = [];
  xdg.configFile."rofi/default-theme.rasi".source = ../../../__legacy__/etc/rofi/default-theme.rasi;
  programs.rofi = {
    enable = true;
    # FIXME: can I get this to use xdg.configFile in the future?
    theme = "~/.config/rofi/default-theme.rasi"; 
  };
}

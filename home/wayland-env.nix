{ inputs, lib, config, pkgs, ... }: {

  imports = [];
  
  programs.foot = {
    enable = true;
    server.enable = true;
  };

  wayland.windowManager.sway = {
    enable = true;
    config = rec {
      modifier = "Mod4";  # super
      terminal = "foot";
    };
  };
}

{ inputs, lib, config, pkgs, ... }: {

  imports = [];

  wayland.windowManager.sway = {
    enable = true;
    config = rec {
      modifier = "Mod4";  # super
      terminal = "kitty";
    };
  };
}

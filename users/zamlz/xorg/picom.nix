{ inputs, lib, config, pkgs, ... }: {
  imports = [];
  services.picom = {
    enable = true;
    fade = true;
    fadeDelta = 5;
    shadow = true;
    vSync = true;
    settings = {
      # FIXME: need only selective dimming
      # inactive-dim = 0.3;
    };
  };
}

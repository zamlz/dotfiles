{ inputs, lib, config, pkgs, ... }: {
  xdg.configFile."xinit/rc.sh".source = ./scripts/xinitrc.sh;
}

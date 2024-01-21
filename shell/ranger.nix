{ inputs, lib, config, pkgs, ... }: {
  xdg.configFile."ranger/rc.conf".source = ./resources/ranger.conf;
}

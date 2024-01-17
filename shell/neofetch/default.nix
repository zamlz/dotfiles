{ inputs, lib, config, pkgs, ... }: {
  xdg.configFile."neofetch/ascii.txt".source = ./copland.txt;
  xdg.configFile."neofetch/config.conf".source = ./config.conf;
}

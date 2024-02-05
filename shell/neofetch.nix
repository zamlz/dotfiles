{ inputs, lib, config, pkgs, ... }: {
  #xdg.configFile."neofetch/ascii.txt".source = ./resources/copland.txt;
  xdg.configFile."neofetch/config.conf".source = ./resources/neofetch.conf;
}

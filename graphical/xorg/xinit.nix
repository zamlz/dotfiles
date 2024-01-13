{ inputs, lib, config, pkgs, ... }: {
  home.file.".xinitrc".source = ./xinitrc;
}

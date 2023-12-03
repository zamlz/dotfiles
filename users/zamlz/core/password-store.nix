{ inputs, lib, config, pkgs, ... }: {

  imports = [];

  programs.password-store = {
    enable = true;
    settings = {
      PASSWORD_STORE_DIR = "$HOME/usr/passwords";
    };
  };
}

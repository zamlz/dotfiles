{ inputs, lib, config, pkgs, ... }: {
  programs.password-store = {
    enable = true;
    settings = {
      PASSWORD_STORE_DIR = "$HOME/sys/passwords";
    };
  };
}

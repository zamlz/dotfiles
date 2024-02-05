{ inputs, lib, config, pkgs, ... }: {
  programs.password-store = {
    enable = true;
    package = pkgs.pass.withExtensions (exts: [ exts.pass-update ]);
    settings = {
      PASSWORD_STORE_DIR = "$HOME/sys/passwords";
    };
  };
}

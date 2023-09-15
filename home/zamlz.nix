{ inputs, lib, config, pkgs, ... }: {

  imports = [
    ./shell-env.nix
    ./xorg-env.nix
    ./wayland-env.nix
  ];

  nixpkgs = {
    config = {
      allowUnfree = true;
      # bugfix github issue #2942
      allowUnfreePredicate = (_: true);
    };
  };

  home.username = "zamlz";
  home.homeDirectory = "/home/zamlz";

  home.packages = with pkgs; [
    # CLI Tools
    cmatrix
    figlet
    htop
    lolcat
    neofetch
    pstree
    tree
    # GUI Tools
    qutebrowser
    feh
    iosevka
  ];

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
  # FIXME: What is this?
  systemd.user.startServices = "sd-switch";

  # This determines the Home Manager release that your configuration is
  # compatible with. DO NOT CHANGE
  home.stateVersion = "23.05";
}


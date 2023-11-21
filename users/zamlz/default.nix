{ inputs, lib, config, pkgs, ... }: {

  imports = [
    ./core
    ./xorg
    ./wayland
  ];

  nixpkgs = {
    config = {
      allowUnfree = true;
      # bugfix github issue #2942
      allowUnfreePredicate = (_: true);
    };
  };
  
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  home = {
    # This determines the Home Manager release that your configuration is
    # compatible with. DO NOT CHANGE
    stateVersion = "23.05";
    username = "zamlz";
    homeDirectory = "/home/zamlz";
    packages = with pkgs; [
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
      emacs29
      wmctrl
    ];
  };

  # FIXME: What is this?
  systemd.user.startServices = "sd-switch";

}


{ inputs, lib, config, pkgs, ... }: {

  imports = [
    ./shell
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
    username = "zamlz";
    homeDirectory = "/home/zamlz";
    # This determines the Home Manager release that your configuration is
    # compatible with. DO NOT CHANGE
    home.stateVersion = "23.05";
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
    ];
  };

  # FIXME: What is this?
  systemd.user.startServices = "sd-switch";

}


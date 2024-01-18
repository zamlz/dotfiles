{ inputs, lib, config, pkgs, ... }: {

  imports = [
    ../../shell # Shell Utilties
    ../../graphical # Graphical Utilties
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
      # System CLI Tools
      htop
      pciutils
      neofetch
      pstree
      ranger
      tree
      less
      ripgrep
      # Fun CLI Tools
      cmatrix
      figlet
      lolcat
      # Fonts
      iosevka
      # Desktop Environment Utilities
      feh
      wmctrl
      i3lock
      # GUI Apps
      firefox
      qutebrowser
      # wayland tools
      swww
      waypaper
      # Misc Experiments
      qtile
    ];
  };

  # FIXME: What is this?
  systemd.user.startServices = "sd-switch";
}

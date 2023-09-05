# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ inputs, lib, config, pkgs, ... }: {

  imports = [ 
    # Import your generated (nixos-generate-config) hardware configuration
    ./hardware-configuration.nix
  ];
  
  nixpkgs = {
    config.allowUnfree = true;
  };
  
  nix = {
    # This will add each flake input as a registry
    # to make nix3 commands consistent with your flake
    registry = lib.mapAttrs (_: value: { flake = value; }) inputs;

    # This will additionally add your inputs to the system's legacy channels
    nixPath = lib.mapAttrsToList (key: value: "${key}=${value.to.path}") config.nix.registry;

    settings = {
      experimental-features = [ "nix-command" "flakes" ];
      auto-optimise-store = true;
    };

    gc = {
      automatic = true;
      persistent = true;
      dates = "weekly";
      options = "--delete-older-than 30d";
    };
  };

  boot.loader = {
    systemd-boot.enable = true;
    efi.canTouchEfiVariables = true;
  };
  
  boot.initrd = {
    secrets = {
      "/crypto_keyfile.bin" = null;
    };
    luks.devices."luks-0224b369-12c1-4ea0-a732-6ee6ec2e1192" = {
      device = "/dev/disk/by-uuid/0224b369-12c1-4ea0-a732-6ee6ec2e1192";
      keyFile = "/crypto_keyfile.bin";
    }; 
  };

  networking = {
    hostName = "NAVI-CoplandOS";
    networkmanager.enable = true;
    firewall = {
      enable = true;
      allowedTCPPorts = [ 22 ];
      #allowedUDPPorts = [ ... ];
    };
  };

  time.timeZone = "America/Los_Angeles";
  i18n.defaultLocale = "en_US.UTF-8";

  console = {
    earlySetup = true;
    font = "${pkgs.terminus_font}/share/consolefonts/ter-128n.psf.gz";
    packages = with pkgs; [ terminus_font ];
    keyMap = "us";
  };

  environment = {
    systemPackages = with pkgs; [ git vim curl ];
    variables.EDITOR = "vim";
  };
  
  programs.zsh.enable = true;

  # User Accounts
  # -------------
  # (Don't forget to set a password with ‘passwd’!)
  users.users.zamlz = {
    isNormalUser = true;
    description = "Amlesh Sivanantham";
    initialPassword = "pleasechangeme";
    extraGroups = [
      "networkmanager"
      "wheel"
    ];
    shell = pkgs.zsh;
  };
  
  services.openssh = {
    enable = true;
    settings = {
      PermitRootLogin = "no";
      PasswordAuthentication = false;
    };
  };

  hardware = {
    opengl = {
      enable = true;
      driSupport = true;
    };
  };

  services.xserver = {
    enable = true;
    libinput.enable = true;
    displayManager.startx.enable = true;
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?

}

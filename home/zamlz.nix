{ inputs, lib, config, pkgs, ... }: {

  imports = [];

  nixpkgs = {
    config = {
      allowUnfree = true;
      # bugfix github issue #2942
      allowUnfreePredicate = (_: true);
    };
  };

  home.username = "zamlz";
  home.homeDirectory = "/home/zamlz";

  home.packages = [
    # CLI Tools
    pkgs.cmatrix
    pkgs.figlet
    pkgs.htop
    pkgs.lolcat
    pkgs.neofetch
    pkgs.pstree
    pkgs.tmux
    pkgs.tree
  ];

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
  
  programs.git = {
    enable = true;
    userName = "Amlesh Sivanantham (zamlz)";
    userEmail = "zamlz@pm.me";
    signing = {
      signByDefault = true;
      key = "0x882C395C3B28902C";
    };
    aliases = {
      root = "rev-parse --show-toplevel";
    };
    extraConfig = {
      init = {
        defaultBranch = "main";
      };
      pull = {
        ff = "only";
      };
    };
    diff-so-fancy.enable = true;
  };

  programs.gpg = {
    enable = true;
    homedir = "${config.xdg.configHome}/gnupg";
    mutableKeys = false;
    mutableTrust = false;
    publicKeys = [{ source = ./public.key; trust = 5; }];
    settings = {
      default-key = "0FA6 FF80 89E9 C767 0A22  54C7 9731 7FD0 FC2D B3CC";
      keyid-format = "0xlong";
    };
  };

  services.gpg-agent = {
    enable = true;
    enableScDaemon = true;
    enableSshSupport = true;
    enableZshIntegration = true;
    defaultCacheTtl = 600;
    defaultCacheTtlSsh = 600;
    # extraConfig = ''
    # fixed-list-mode
    # keyid-format 0xlong
    # with-fingerprint
    # personal-digest-preferences SHA512 SHA384 SHA256 SHA224
    # default-preference-list SHA512 SHA384 SHA256 SHA224 AES256 AES192 AES CAST5 BZIP2 ZLIB ZIP Uncompressed
    # use-agent
    # verify-options show-uid-validity
    # list-options show-uid-validity
    # cert-digest-algo SHA256
    # no-emit-version
    # '';
    maxCacheTtl = 7200;
    maxCacheTtlSsh = 7200;
    pinentryFlavor =  "tty"; # FIXME: use custom pinentry?
    sshKeys = [ "FA508B6D901BA2A59DE2B7E521EBE58F4CDD6C0D" ];
  };
  
  programs.neovim = {
    enable = true;
    defaultEditor = true;
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;
  };
  
  programs.zsh = {
    enable = true;
    autocd = true;
    dotDir = ".config/zsh";
    enableAutosuggestions = true;
    enableCompletion = true;
    defaultKeymap = "vicmd";
    enableSyntaxHighlighting = true;
    history = {
      extended = true;
      ignoreSpace = true;
    };
    shellAliases = {
      # Shortcuts for ls
      ls = "LC_COLLATE=C ls -F --color=always";
      ll = "ls -oh";
      la = "ls -lah";
      # Shell aliases to make using git easier.
      gg = "lazygit";
      gs = "echo \"origin = $(git remote get-url origin --push)\" | lolcat; git status";
      ga = "git add";
      gc = "git commit";
      gd = "git diff";
      gds = "git diff --staged";
      gl = "git log --graph";
      gls = "git log --graph --stat";
      gll = "git log --graph --stat -p";
      gllo = "git log --graph --pretty=oneline --abbrev-commit";
      glla = "git log --graph --pretty=oneline --abbrev-commit --all";
      gp = "git push";
      gf = "git fetch";
      gm = "git merge";
      gb = "git branch -av";
      gr = "git rev-parse --show-toplevel";
      grr = "git rev-parse --show-toplevel | xargs";
    };
  };

  xsession.enable = true;
  xsession.windowManager.herbstluftwm = {
    enable = true;
    extraConfig = ''
    herbstclient emit_hook reload
    herbstclient keyunbind --all
    herbstclient unrule --all
    herbstclient detect_monitors
    '';
    keybinds = {};
    mousebinds = {};
    rules = [
      "focus=on"
      "class~'(.*[Rr]xvt.*|.*[Tt]erm|Konsole)' focus=on"
      "class~'(Discord|DiscordCanary)' focus=off"
      "windowtype~'_NET_WM_WINDOW_TYPE_(DIALOG|UTILITY|SPLASH)' pseudotile=on"
      "windowtype='_NET_WM_WINDOW_TYPE_DIALOG' focus=on"
      "windowtype~'_NET_WM_WINDOW_TYPE_(NOTIFICATION|DOCK|DESKTOP)' manage=off"
    ];
    settings = {};
    tags = [ "λ" ];
  };

  systemd.user.startServices = "sd-switch";

  # This determines the Home Manager release that your configuration is
  # compatible with. DO NOT CHANGE
  home.stateVersion = "23.05";
}


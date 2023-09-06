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

  home.packages = with pkgs; [
    # CLI Tools
    cmatrix
    figlet
    htop
    lolcat
    neofetch
    pstree
    tree
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
    defaultKeymap = "viins";
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

  programs.tmux = {
    enable = true;
    prefix = "C-x";
    plugins = with pkgs; [
      tmuxPlugins.pain-control
    ];
  };

  # X11 Stuff

  services.sxhkd = let
    sxhkdTerminal = "xterm";
  in {
    enable = true;
    keybindings = {
      "super + Return" = "${sxhkdTerminal}";
      # ROFI
      "super + e" = "rofi -show run";
      "super + w" = "rofi -show window";
    };
  };

  xsession.windowManager.herbstluftwm =
  {
    enable = true;
    extraConfig = ''
      herbstclient emit_hook reload
      herbstclient keyunbind --all
      herbstclient unrule --all
      herbstclient detect_monitors
      etc/xorg/scripts/refresh_sxhkd.sh
    '';
    # mod4 is SUPER
    # You can use xev to identify X11 keys very easily!
    keybinds = let
      super = "Mod4";
      resizestep = "0.01";
    in {
      # FIXME: SXHKD WORKAROUND STUFF
      "${super}-Ctrl-Alt-Escape" = "quit";

      # Reload WM and Close Window
      "${super}-Ctrl-Alt-r" = "chain , spawn $HOME/etc/xorg/refresh.sh , reload";
      "${super}-Ctrl-r" = "reload";
      "${super}-q" = "close";
      
      # Window Focus
      "${super}-Left" = "focus left";
      "${super}-Down" = "focus down";
      "${super}-Up" = "focus up";
      "${super}-Right" = "focus right";
      "${super}-h" = "focus left";
      "${super}-j" = "focus down";
      "${super}-k" = "focus up";
      "${super}-l" = "focus right";

      # defining keybindings for cycling the focused client
      #"${super}-BackSpace" = "cycle_monitor";  # FIXME: this is used already
      "${super}-Tab" = "cycle +1";
      "${super}-Shift-Tab" = "cycle_frame +1";
      "${super}-c" = "cycle_all +1";
      "${super}-i" = "jumpto urgent";

      # Window Movement
      "${super}-Shift-Left" = "shift left";
      "${super}-Shift-Down" = "shift down";
      "${super}-Shift-Up" = "shift up";
      "${super}-Shift-Right" = "shift right";
      "${super}-Shift-h" = "shift left";
      "${super}-Shift-j" = "shift down";
      "${super}-Shift-k" = "shift up";
      "${super}-Shift-l" = "shift right";

      # Splitting Frames
      "${super}-u" = "split bottom 0.5";
      "${super}-o" = "split right 0.5";
      "${super}-Control-space" = "split explode";

      # Resizing Frames
      "${super}-Control-h" = "resize left +${resizestep}";
      "${super}-Control-j" = "resize down +${resizestep}";
      "${super}-Control-k" = "resize up +${resizestep}";
      "${super}-Control-l" = "resize right +${resizestep}";
      "${super}-Control-Left" = "resize left +${resizestep}";
      "${super}-Control-Down" = "resize down +${resizestep}";
      "${super}-Control-Up" = "resize up +${resizestep}";
      "${super}-Control-Right" = "resize right +${resizestep}";

      # Workspace Movement
      "${super}-grave" = "use_previous";
      "${super}-bracketright" = "use_index +1 --skip-visible";
      "${super}-bracketleft" = "use_index -1 --skip-visible";
      "${super}-slash" = "spawn $HOME/etc/herbstluftwm/scripts/tag_utils.sh goto";
      "${super}-Shift-slash" = "spawn $HOME/etc/herbstluftwm/scripts/tag_utils.sh move";
      "${super}-BackSpace" = "spawn $HOME/etc/herbstluftwm/scripts/tag_utils.sh remove";
      
      # Layout Control
      "${super}-r" = "remove";
      "${super}-s" = "floating toggle";
      "${super}-f" = "fullscreen toggle";
      "${super}-t" = "pseudotile toggle";

      # The following cycles through the available layouts within a frame, but skips
      # layouts, if the layout change wouldn't affect the actual window positions.
      # (I.e. if there are two windows within a frame, the grid layout is skipped.)
      "${super}-space" = ''
        or , and . compare tags.focus.curframe_wcount = 2 \
                 . cycle_layout +1 vertical horizontal max vertical grid \
           , cycle_layout +1
      '';

      # Tag Definitions (workspaces)
      "${super}-1" = "use_index 0";
      "${super}-2" = "use_index 1";
      "${super}-3" = "use_index 2";
      "${super}-4" = "use_index 3";
      "${super}-5" = "use_index 4";
      "${super}-6" = "use_index 5";
      "${super}-7" = "use_index 6";
      "${super}-8" = "use_index 7";
      "${super}-9" = "use_index 8";
      "${super}-0" = "use_index 9";
      "${super}-Shift-1" = "move_index 0";
      "${super}-Shift-2" = "move_index 1";
      "${super}-Shift-3" = "move_index 2";
      "${super}-Shift-4" = "move_index 3";
      "${super}-Shift-5" = "move_index 4";
      "${super}-Shift-6" = "move_index 5";
      "${super}-Shift-7" = "move_index 6";
      "${super}-Shift-8" = "move_index 7";
      "${super}-Shift-9" = "move_index 8";
      "${super}-Shift-0" = "move_index 9";
    };
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

  programs.rofi = {
    enable = true;
    theme = ../etc/rofi/default-theme.rasi;
  };

  # Wayland Stuff

  programs.foot = {
    enable = true;
    server.enable = true;
  };

  wayland.windowManager.sway = {
    enable = true;
    config = rec {
      modifier = "Mod4";  # super
      terminal = "foot";
    };
  };

  # FIXME: What is this?
  systemd.user.startServices = "sd-switch";

  # This determines the Home Manager release that your configuration is
  # compatible with. DO NOT CHANGE
  home.stateVersion = "23.05";
}


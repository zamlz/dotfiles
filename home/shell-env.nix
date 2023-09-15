{ inputs, lib, config, pkgs, ... }: {

  imports = [];

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
    extraConfig = ''
      fun! TrimWhitespace()
          let l:save = winsaveview()
          keeppatterns %s/\s\+$//e
          call winrestview(l:save)
      endfun
      command! TrimWhitespace call TrimWhitespace()
    '';
  };

  programs.ssh = {
    enable = true;
    matchBlocks = {
      # Since we are using GnuPG's GPG Agent as the SSH agent, when in a terminal,
      # ssh agent doesn't know that it has to change terminals (a bug in openssh). So
      # when it connects to gpg-agent, it uses the terminal it was last configured to
      # use. The following command when run in a terminal updates gpg-agent to use
      # the current terminal for openssh. However, now if we run some ssh related
      # command in the prior terminal, it will use the new terminal instead creating
      # the exact inverse of the problem. Therefore we attempt to fix this by running
      # this command before every SSH command.
      gpgAgentFix.match = "host * exec \"gpg-connect-agent --no-autostart UPDATESTARTUPTTY /bye\"";
    };
  };

  programs.tmux = {
    enable = true;
    prefix = "C-b";
    mouse = true;
    historyLimit = 100000;
    baseIndex = 1;
    keyMode = "vi";
    terminal = "screen-256color";
    plugins = with pkgs; [
      tmuxPlugins.pain-control
    ];
    extraConfig = ''
      set -g set-titles on
      set -g set-titles-string '#T'
      bind r source-file ~/etc/tmux/config \; display "Reloaded Tmux Config"
    '';
  };

  programs.zsh = {
    enable = true;
    dotDir = ".config/zsh";
    autocd = true;
    defaultKeymap = "viins";
    enableAutosuggestions = true;
    enableCompletion = true;
    enableSyntaxHighlighting = true;
    enableVteIntegration = true;
    history = {
      extended = true;
      ignoreSpace = true;
      ignorePatterns = [
        "rm *"
	"pkill *"
      ];
      save = 100000;
      path = ".local/share/zsh_history.db";
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
}

{ inputs, lib, config, pkgs, ... }: {
  xdg.configFile."zsh/prompt.zsh".source = ./resources/zsh-prompt.zsh;
  programs.zsh = {
    enable = true;
    dotDir = ".config/zsh";
    autocd = true;
    defaultKeymap = "viins";
    enableAutosuggestions = true;
    enableCompletion = true;
    syntaxHighlighting.enable = true;
    enableVteIntegration = true;
    history = {
      extended = true;
      ignoreSpace = true;
      ignorePatterns = [
        "rm *"
        "pkill *"
      ];
      save = 100000;
    };
    shellAliases = {
      # Shortcuts for ls
      ls = "LC_COLLATE=C ls -F --color=always";
      ll = "ls -oh";
      la = "ls -lah";
      # Shell aliases to make using git easier.
      gg = "lazygit";
      gs = "echo \"\\e[0;36morigin\\e[0m = \\e[0;34m$(git remote get-url origin --push)\\e[0m\"; git status";
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
      # make all vi/vim point to neovim
      vi = "nvim";
      vim = "nvim";
      # aliasing these guys to make them safer
      rm = "rm -I --preserve-root";
      mv = "mv -i";
      cp = "cp -i";
      # misc aliases that are useful/fun
      please = "sudo";
      weather = "curl wttr.in";
    };
    loginExtra = ''
      if [ -z "$DISPLAY" ] && [ "$(fgconsole 2>/dev/null)" -eq 1 ]; then
          # exec Hyprland
          exec startx $HOME/.config/xinit/rc.sh herbstluftwm
      elif [ -z "$DISPLAY" ] && [ "$(fgconsole 2>/dev/null)" -eq 2 ]; then
          exec qtile start -b wayland
      fi
    '';
    initExtra = ''
    source $HOME/.config/zsh/prompt.zsh
    
    # prepare the window id directory
    WINDIR=/tmp/.wid
    mkdir -p $WINDIR
    
    # Load window info for given Target Window ID (used with pwdcfw.sh)
    function load_window_info() {
        if [ -n "$DISPLAY" ] && [ -n "$TARGET_WINDOWID" ]; then
            source "$WINDIR/$TARGET_WINDOWID"
            cd $WINDOW_PWD
            unset TARGET_WINDOWID
        fi
    }
    
    # Save window info for given Window ID (used with pwdcfw.sh)
    function save_window_info() {
        if [ -n "$DISPLAY" ] && [ -n "$WINDOWID" ]; then
            WINDOWID_FILE="$WINDIR/$WINDOWID"
            echo "WINDOW_PWD='$(pwd)'" | tee $WINDOWID_FILE
        fi
    }

    precmd() {
        # load terminal window info if it exists
        load_window_info > /dev/null 2>&1
        export PROMPT=$(prompt_generate $?)
        # save terminal window info (creates id file)
        save_window_info > /dev/null 2>&1
    }
    '';
    localVariables = {
      LESS = "-R --no-init --quit-if-one-screen";
      # no longer creates __pycache__ folders in the same folder as *.py files
      PYTHONPYCACHEPREFIX = "$HOME/.cache/__pycache__";
    };
  };
}

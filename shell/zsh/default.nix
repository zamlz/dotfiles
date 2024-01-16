{ inputs, lib, config, pkgs, ... }: {

  imports = [];

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
      vi = "nvim";
      vim = "nvim";
    };
    loginExtra = ''
      if [ -z "$DISPLAY" ] && [ "$(fgconsole 2>/dev/null)" -eq 1 ]; then
          # exec Hyprland
          exec startx herbstluftwm
      elif [ -z "$DISPLAY" ] && [ "$(fgconsole 2>/dev/null)" -eq 2 ]; then
          exec qtile start -b wayland
      fi
    '';
    initExtra = ''
    source $HOME/nix/shell/zsh/prompt.zsh
    precmd() {
    	# load terminal window info if it exists
	# FIXME: enable load_window_info > /dev/null
	export PROMPT=$(prompt_generate $?)
    	# save terminal window info (creates id file)
	# FIXME: enable save_window_info > /dev/null 
    }
    '';
  };
}

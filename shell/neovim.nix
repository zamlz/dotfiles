{ inputs, lib, config, pkgs, ... }: let
  leaderAction = key: action: description: {
    key = "<leader>${key}";
    action = action;
    options.desc = description;
  };
  leaderFloatermSetup = key: title: exec: description: width: height:
    let
      action = "<CMD>FloatermNew --width=${width} --height=${height} --title=${title} ${exec}<CR>";
    in leaderAction key action description;
  leaderFloaterm = key: title: exec: description:
    leaderFloatermSetup key title exec description "0.6" "0.6";
  leaderFloatermMax = key: title: exec: description:
    leaderFloatermSetup key title exec description "1.0" "1.0";
in {
  programs.nixvim = {
    enable = true;
    enableMan = true;

    globals = {
      mapleader = " ";
      maplocalleader = " ";
    };

    colorscheme = "modus_vivendi";

    options = {
      # fringe line numbers
      number         = false;
      relativenumber = false;
      # cursor crosshair and soft-thresholds
      ruler        = true;
      cursorline   = true;
      cursorcolumn = false;
      colorcolumn  = [ 80 128 ];
      # text display
      wrap = false;
      # put and end to this tab vs. spaces war
      tabstop     = 4;    # visual spaces per tab character
      expandtab   = true; # expand <TAB> key to spaces in insert mode
      softtabstop = 4;    # number of spaces to insert for a tab
      shiftwidth  = 4;    # number of spaces used for each autoindent step
      # code concealing
      conceallevel = 2;
      #concealcursor = 'nc';
      # code folding
      foldenable     = false;
      foldmethod     = "expr";
      foldexpr       = "nvim_treesitter#foldexpr()";
      foldlevelstart = 10;
      foldnestmax    = 10;
      # dynamic configuration via source files
      modeline = true;
      # let's use the mouse scrolling cause we can
      mouse = "a";
      # rice out neovim
      syntax     = "on";
      lazyredraw = true;
      showmode   = false;
    };

    extraPlugins = with pkgs.vimPlugins; [
      vim-floaterm
      smartcolumn-nvim
      modus-themes-nvim
    ];

    plugins = {
      gitsigns.enable = true;
      which-key.enable = true;
      neogit.enable = true;
      
      lualine = {
        enable = true;
        globalstatus = true;
      };

      telescope = {
        enable = true;
        extraOptions = {
          pickers = {
            buffers = {
              theme = "ivy";
            };
            find_files = {
              theme = "ivy";
            };
            git_files = {
              theme = "ivy";
            };
            oldfiles = {
              theme = "ivy";
            };
            live_grep = {
              theme = "ivy";
            };
            builtin = {
              theme = "ivy";
            };
          };
        };
        keymaps = {
          "<leader>b" = {
            action = "buffers";
            desc = "Buffers";
          };
          "<leader>ff" = {
            action = "git_files";
            desc = "Telescope Git Files";
          };
          "<leader>f/" = {
            action = "find_files";
            desc = "Find Files";
          };
          "<leader>fr" = {
            action = "oldfiles";
            desc = "Recent Files";
          };
          "<leader>sg" = {
            action = "live_grep";
            desc = "Live Grep";
          };
          "<leader>tt" = {
            action = "builtin";
            desc = "Telescope";
          };
        };
      };
    };

    keymaps = [
      # useful so you don't have to press "shift" when trying to run commands
      { key = ";"; action = ":"; }
      # Git
      (leaderAction "g" "<CMD>Neogit<CR>" "Neogit")
      # Floaterm
      (leaderFloaterm "t" "Terminal" "" "Terminal")
      (leaderFloaterm "p" "IPython" "ipython" "IPython")
      (leaderFloatermMax "d" "Ranger" "ranger" "Ranger")
    ];
  };
}

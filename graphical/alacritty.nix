{ inputs, lib, config, pkgs, ... }: let
  gruvboxBlack = {
    foreground = "#ebdbb2";
    background = "#000000";
    black      = "#181818";
    red        = "#cc241d";
    green      = "#98971a";
    yellow     = "#d79921";
    blue       = "#458588";
    magenta    = "#b16286";
    cyan       = "#689d6a";
    white      = "#a89984";
    blackAlt   = "#928374";
    redAlt     = "#fb4934";
    greenAlt   = "#b8bb26";
    yellowAlt  = "#fabd2f";
    blueAlt    = "#83a598";
    magentaAlt = "#d3869b";
    cyanAlt    = "#8ec07c";
    whiteAlt   = "#ebdbb2";
  };
  modusVivendi = {
    background = "#000000";
    foreground = "#ffffff";
    black =      "#000000";
    red =        "#ff5f59";
    green =      "#44bc44";
    yellow =     "#d0bc00";
    blue =       "#2fafff";
    magenta =    "#feacd0";
    cyan =       "#00d3d0";
    white =      "#ffffff";
    blackAlt =   "#1e1e1e";
    redAlt =     "#ff5f5f";
    greenAlt =   "#44df44";
    yellowAlt =  "#efef00";
    blueAlt =    "#338fff";
    magentaAlt = "#ff66ff";
    cyanAlt =    "#00eff0";
    whiteAlt =   "#989898";
  # indexed_colors:
  #   - { index: 16, color: '0xfec43f' }
  #   - { index: 17, color: '0xff9580' }
  };
  colorScheme = modusVivendi;
in {
  imports = [];
  programs.alacritty = {
    enable = true;
    settings = {
      live_config_reload = true;

      colors = with colorScheme; {
        draw_bold_text_with_bright_colors = false;
        primary = {
          background = background;
          foreground = foreground;
        };
        cursor = {
          cursor = foreground;
          text = "CellForeground";
        };
        normal = {
          black = black;
          red = red;
          green = green;
          yellow = yellow;
          blue = blue;
          magenta = magenta;
          cyan = cyan;
          white = white;
        };
        bright = {
          black = blackAlt;
          red = redAlt;
          green = greenAlt;
          yellow = yellowAlt;
          blue = blueAlt;
          magenta = magentaAlt;
          cyan = cyanAlt;
          white = whiteAlt;
        };
      };

      cursor = {
        vi_mode_style = "Block";
        style = {
          blinking = "On";
          shape = "Beam";
        };
      };

      font = {
        size = 7.0;
        glyph_offset = {
          x = 0;
          y = 0;
        };
        normal = {
          family = "Iosevka";
          style = "Regular";
        };
        italic = {
          family = "Iosevka";
          style = "Italic";
        };
        bold = {
          family = "Iosevka";
          style = "Bold";
        };
        
        bold_italic = {
          family = "Iosevka";
          style = "Bold Italic";
        };
      };

      scrolling = {
        history = 100000;
        multiplier = 3;
      };

      selection = {
        save_to_clipboard = true;
        semantic_escape_chars = ",│`|:\"' ()[]{}<>\t";
      };

      window = {
        dynamic_padding = false;
        opacity = 1.0;
        padding = {
          x = 0;
          y = 0;
        };
      };
    };
  };
}

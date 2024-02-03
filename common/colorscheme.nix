let
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
in {
  defaultColorScheme = gruvboxBlack;
}

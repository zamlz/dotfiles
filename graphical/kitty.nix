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
  colorScheme = gruvboxBlack;
in {
  imports = [];
  programs.kitty = {
    enable = true;
    font = {
      name = "Iosevka Term";
      size = 10.0;
    };
    settings = {
      "scrollback_lines"   = 100000;
      "background_opacity" = "1.0";
      "foreground"         = "${colorScheme.foreground}";
      "background"         = "${colorScheme.background}";
      "color0"             = "${colorScheme.black}";
      "color1"             = "${colorScheme.red}";
      "color2"             = "${colorScheme.green}";
      "color3"             = "${colorScheme.yellow}";
      "color4"             = "${colorScheme.blue}";
      "color5"             = "${colorScheme.magenta}";
      "color6"             = "${colorScheme.cyan}";
      "color7"             = "${colorScheme.white}";
      "color8"             = "${colorScheme.blackAlt}";
      "color9"             = "${colorScheme.redAlt}";
      "color10"            = "${colorScheme.greenAlt}";
      "color11"            = "${colorScheme.yellowAlt}";
      "color12"            = "${colorScheme.blueAlt}";
      "color13"            = "${colorScheme.magentaAlt}";
      "color14"            = "${colorScheme.cyanAlt}";
      "color15"            = "${colorScheme.whiteAlt}";
    };
    shellIntegration.enableZshIntegration = true;
  };
}

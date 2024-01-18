{ inputs, lib, config, pkgs, ... }: let
  colorScheme = (import ../../../common/colorscheme.nix).defaultColorScheme;
in {
  imports = [];
  programs.rofi = {
    enable = true;
    location = "center";
    font = "Iosevka 12";
    terminal = "${pkgs.alacritty}/bin/alacritty";
    theme = let
      inherit (config.lib.formats.rasi) mkLiteral;
    in {
      "*" = {
        red =                         mkLiteral colorScheme.red;
        blue =                        mkLiteral colorScheme.blue;
        background =                  mkLiteral colorScheme.background;
        foreground =                  mkLiteral colorScheme.foreground;
        background-color =            mkLiteral "rgba ( 0, 0, 0, 0 % )";
        normal-background =           mkLiteral "var(background)";
        normal-foreground =           mkLiteral "var(foreground)";
        lightbg =                     mkLiteral "rgba ( 32, 32, 32, 100 % )";
        lightfg =                     mkLiteral "var(foreground)";
        selected-normal-background =  mkLiteral "var(lightfg)";
        selected-normal-foreground =  mkLiteral "var(lightbg)";
        alternate-normal-background = mkLiteral "var(lightbg)";
        alternate-normal-foreground = mkLiteral "var(foreground)";
        active-background =           mkLiteral "var(background)";
        active-foreground =           mkLiteral "var(blue)";
        selected-active-background =  mkLiteral "var(blue)";
        selected-active-foreground =  mkLiteral "var(background)";
        alternate-active-background = mkLiteral "var(lightbg)";
        alternate-active-foreground = mkLiteral "var(blue)";
        urgent-background =           mkLiteral "var(background)";
        urgent-foreground =           mkLiteral "var(red)";
        selected-urgent-background =  mkLiteral "var(red)";
        selected-urgent-foreground =  mkLiteral "var(background)";
        alternate-urgent-background = mkLiteral "var(lightbg)";
        alternate-urgent-foreground = mkLiteral "var(red)";
        separatorcolor =              mkLiteral "var(foreground)";
        border-color =                mkLiteral "var(foreground)";
        spacing =                     2;
      };
      "element" = {
        padding = mkLiteral "1px ";
        cursor =  mkLiteral "pointer";
        spacing = mkLiteral "5px ";
        border =  0;
      };
      "element normal.normal" = {
        background-color = mkLiteral "var(normal-background)";
        text-color =       mkLiteral "var(normal-foreground)";
      };
      "element normal.urgent" = {
        background-color = mkLiteral "var(urgent-background)";
        text-color =       mkLiteral "var(urgent-foreground)";
      };
      "element normal.active" = {
        background-color = mkLiteral "var(active-background)";
        text-color =       mkLiteral "var(active-foreground)";
      };
      "element selected.normal" = {
        background-color = mkLiteral "var(selected-normal-background)";
        text-color =       mkLiteral "var(selected-normal-foreground)";
      };
      "element selected.urgent" = {
        background-color = mkLiteral "var(selected-urgent-background)";
        text-color =       mkLiteral "var(selected-urgent-foreground)";
      };
      "element selected.active" = {
        background-color = mkLiteral "var(selected-active-background)";
        text-color =       mkLiteral "var(selected-active-foreground)";
      };
      "element alternate.normal" = {
        background-color = mkLiteral "var(alternate-normal-background)";
        text-color =       mkLiteral "var(alternate-normal-foreground)";
      };
      "element alternate.urgent" = {
        background-color = mkLiteral "var(alternate-urgent-background)";
        text-color =       mkLiteral "var(alternate-urgent-foreground)";
      };
      "element alternate.active" = {
        background-color = mkLiteral "var(alternate-active-background)";
        text-color =       mkLiteral "var(alternate-active-foreground)";
      };
      "element-text" = {
        background-color = mkLiteral "rgba ( 0, 0, 0, 0 % )";
        cursor =           mkLiteral "inherit";
        highlight =        mkLiteral "inherit";
        text-color =       mkLiteral "inherit";
      };
      "element-icon" = {
        background-color = mkLiteral "rgba ( 0, 0, 0, 0 % )";
        size =             mkLiteral "1.0000em ";
        cursor =           mkLiteral "inherit";
        text-color =       mkLiteral "inherit";
      };
      "window" = {
        padding =          5;
        background-color = mkLiteral "var(background)";
        border =           1;
        width =            1000;
        y-offset =         mkLiteral "-0%";
      };
      "mainbox" = {
        padding = 0;
        border =  0;
      };
      "message" = {
        padding =      mkLiteral "1px ";
        border-color = mkLiteral "var(separatorcolor)";
        border =       mkLiteral "2px dash 0px 0px ";
      };
      "textbox" = {
        text-color = mkLiteral "var(foreground)";
      };
      "listview" = {
        padding =      mkLiteral "2px 0px 0px ";
        scrollbar =    true;
        border-color = mkLiteral "var(separatorcolor)";
        spacing =      mkLiteral "2px ";
        fixed-height = false;
        dynamic =      true;
        border =       mkLiteral "2px dash 0px 0px ";
      };
      "scrollbar" = {
        width =        mkLiteral "4px ";
        padding =      0;
        handle-width = mkLiteral "8px ";
        border =       0;
        handle-color = mkLiteral "var(normal-foreground)";
      };
      "sidebar" = {
        border-color = mkLiteral "var(separatorcolor)";
        border =       mkLiteral "2px dash 0px 0px ";
      };
      "button" = {
        cursor =     mkLiteral "pointer";
        spacing =    0;
        text-color = mkLiteral "var(normal-foreground)";
      };
      "button selected" = {
        background-color = mkLiteral "var(selected-normal-background)";
        text-color =       mkLiteral "var(selected-normal-foreground)";
      };
      "num-filtered-rows" = {
        expand =     false;
        text-color = mkLiteral "rgba ( 128, 128, 128, 100 % )";
      };
      "num-rows" = {
        expand =     false;
        text-color = mkLiteral "rgba ( 128, 128, 128, 100 % )";
      };
      "textbox-num-sep" = {
        expand =     mkLiteral "false";
        str =        "/";
        text-color = mkLiteral "rgba ( 128, 128, 128, 100 % )";
      };
      "inputbar" = {
        padding =    mkLiteral "1px ";
        spacing =    mkLiteral "0px ";
        text-color = mkLiteral "var(normal-foreground)";
        children =   mkLiteral "[ prompt,textbox-prompt-colon,entry,num-filtered-rows,textbox-num-sep,num-rows,case-indicator ]";
      };
      "case-indicator" = {
        spacing =    0;
        text-color = mkLiteral "var(normal-foreground)";
      };
      "entry" = {
        text-color =        mkLiteral "var(normal-foreground)";
        cursor =            mkLiteral "text";
        spacing =           0;
        placeholder-color = mkLiteral "rgba ( 128, 128, 128, 100 % )";
        placeholder =       "Type to filter";
      };
      "prompt" = {
        spacing =    0;
        text-color = mkLiteral "var(normal-foreground)";
      };
      "textbox-prompt-colon" = {
        margin =     mkLiteral "0px 0.3000em 0.0000em 0.0000em ";
        expand =     false;
        str =        ":";
        text-color = mkLiteral "inherit";
      };
    };
  };
}

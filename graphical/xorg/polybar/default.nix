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
  xdg.configFile."polybar/kernel_info.sh" = {
    source = ./__legacy__/scripts/kernel_info.sh;
    executable = true;
  };
  services.polybar = {
    enable = true;
    # FIXME: What does this option even do?
    script = "polybar &";
    config = {
      "bar/top" = {
        width = "100%";
        height = "3%";
        radius = 0;
        font-0 = "Iosevka:size=12";
        separator = " ";
        background = "${colorScheme.background}";
        foreground = "${colorScheme.foreground}";
        padding = 2;
        module-margin = 1;
        border-size = 0;
        border-color = "${colorScheme.foreground}";
        bottom = false;
        modules-left = "kernel workspaces";
        modules-center= "";
        modules-right = "cpu memory battery1 battery0 wired wireless date";
      };
      # Remove battery duplication logic
      "module/battery0" = {
        type = "internal/battery";
        full-at = 99;
        low-at = 10;
        battery = "BAT0";
        adapter = "";
        poll-interval = 5;
        time-format = "%H:%M";
        format-background = "${colorScheme.background}";
        format-foreground = "${colorScheme.foreground}";
        format-charging = "[<label-charging>]";
        format-discharging = "[<label-discharging>]";
        format-full = "[<label-full>]";
        format-low = "[<label-low> <animation-low>]";
        label-full = "%percentage%!";
        label-full-foreground = "${colorScheme.white}";
        label-charging = "%percentage%* %time%";
        label-charging-foreground = "${colorScheme.greenAlt}";
        label-discharging = "%percentage%% %time%";
        label-discharging-foreground = "${colorScheme.green}";
        label-low = "%percentage%% %time%";
        label-low-foreground = "${colorScheme.red}";
        animation-low-0 = "(LOW BATTERY)";
        animation-low-1 = "( . . . . . )";
        animation-low-framerate = 400;
        animation-low-foreground = "${colorScheme.red}";
      };
      "module/battery1" = {
        type = "internal/battery";
        full-at = 99;
        low-at = 10;
        battery = "BAT1";
        adapter = "";
        poll-interval = 5;
        time-format = "%H:%M";
        format-background = "${colorScheme.background}";
        format-foreground = "${colorScheme.foreground}";
        format-charging = "[<label-charging>]";
        format-discharging = "[<label-discharging>]";
        format-full = "[<label-full>]";
        format-low = "[<label-low> <animation-low>]";
        label-full = "%percentage%!";
        label-full-foreground = "${colorScheme.white}";
        label-charging = "%percentage%* %time%";
        label-charging-foreground = "${colorScheme.greenAlt}";
        label-discharging = "%percentage%% %time%";
        label-discharging-foreground = "${colorScheme.green}";
        label-low = "%percentage%% %time%";
        label-low-foreground = "${colorScheme.red}";
        animation-low-0 = "(LOW BATTERY)";
        animation-low-1 = "( . . . . . )";
        animation-low-framerate = 400;
        animation-low-foreground = "${colorScheme.red}";
      };
      "module/date" = {
        type = "internal/date";
        internal = 1;
        date = "%Y-%m-%d";
        time = "%r";
        date-alt = "%B %d, %Y (%A)";
        time-alt = "%l:%M:%S %p";
        label = "%date% %time%";
        label-foreground = "${colorScheme.blue}";
      };
      "module/workspaces" = {
        type = "internal/xworkspaces";
        pin-workspaces = false;
        enable-click = true;
        enable-scroll = true;
        format = "<label-state>";
        format-background = "${colorScheme.background}";
        format-foreground = "${colorScheme.foreground}";
        label-monitor = "%name%";
        label-monitor-background = "${colorScheme.background}";
        label-monitor-foreground = "${colorScheme.red}";
        label-active = " [%index%:%name%] ";
        label-active-background = "${colorScheme.background}";
        label-active-foreground = "${colorScheme.blue}";
        label-occupied = " [%index%:%name%] ";
        label-occupied-background = "${colorScheme.background}";
        label-occupied-foreground = "${colorScheme.white}";
        label-urgent = " [%index%:%name%] ";
        label-urgent-background = "${colorScheme.background}";
        label-urgent-foreground = "${colorScheme.yellow}";
        label-empty = " [%index%:%name%] ";
        label-empty-background = "${colorScheme.background}";
        label-empty-foreground = "#484848";
      };
      "module/kernel" = {
        type = "custom/script";
        # FIXME: can I get this to use xdg.configFile in the future?
        exec = "~/.config/polybar/kernel_info.sh";
        interval = 90;
        format = "<label>";
        label-foreground = "${colorScheme.foreground}";
      };
      "module/cpu" = {
        type = "internal/cpu";
        interval = 1;
        warn-percentage = 50;
        format = "";
        format-warn = "<label-warn>";
        label-warn = "cpu:%percentage%%";
        label-warn-foreground = "${colorScheme.red}";
      };
      "module/memory" = {
        type = "internal/memory";
        interval = 1;
        warn-percentage = 75;
        format = "";
        format-warn = "<label-warn>";
        label-warn = "mem:%percentage_used%%(%percentage_swap_used%%)";
        label-warn-foreground = "${colorScheme.red}";
      };
      "module/wireless" = {
        type = "internal/network";
        interface-type = "wireless";
        label-connected = "%ifname%: %essid% (%local_ip%)";
        label-connected-foreground = "${colorScheme.cyan}";
      };
      "module/wired" = {
        type = "internal/network";
        interface-type = "wired";
        label-connected = "%ifname%: (%local_ip%)";
        label-connected-foreground = "${colorScheme.cyan}";
      };
    };
  };
}

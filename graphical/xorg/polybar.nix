{ inputs, lib, config, pkgs, ... }: let
  colorScheme = (import ../../common/colorscheme.nix).defaultColorScheme;
in {
  imports = [];
  xdg.configFile."polybar/kernel-info.sh" = {
    source = ./scripts/polybar-kernel-info.sh;
    executable = true;
  };

  services.polybar = {
    enable = true;
    # FIXME: What does this option even do?
    script = "polybar &";
    config = let
      barConfig = placement: left: center: right: {
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
        bottom = placement == "bottom";
        modules-left = left;
        modules-center= center;
        modules-right = right;
      };

      batteryConfig = battery: {
        type = "internal/battery";
        full-at = 99;
        low-at = 10;
        battery = battery;
        adapter = "";
        poll-interval = 5;
        time-format = "%H:%M";
        format-background = "${colorScheme.background}";
        format-foreground = "${colorScheme.foreground}";
        format-charging = "<label-charging>";
        format-discharging = "<label-discharging>";
        format-full = "<label-full>";
        format-low = "<label-low> <animation-low>";
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

      dateConfig = {
        type = "internal/date";
        internal = 1;
        date = "%B %d, %Y (%A)";
        time = "%l:%M:%S %p";
        date-alt = "%Y-%m-%d";
        time-alt = "%r";
        label = "%date% %time%";
        label-foreground = "${colorScheme.blue}";
      };

      workspaceConfig = {
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
        label-active = " %index%:%name% ";
        label-active-background = "${colorScheme.background}";
        label-active-foreground = "${colorScheme.blue}";
        label-occupied = " %index%:%name% ";
        label-occupied-background = "${colorScheme.background}";
        label-occupied-foreground = "${colorScheme.white}";
        label-urgent = " %index%:%name% ";
        label-urgent-background = "${colorScheme.background}";
        label-urgent-foreground = "${colorScheme.yellow}";
        label-empty = " %index%:%name% ";
        label-empty-background = "${colorScheme.background}";
        label-empty-foreground = "#484848";
      };

      systemInfoConfig = {
        type = "custom/script";
        # FIXME: can I get this to use xdg.configFile in the future?
        exec = "~/.config/polybar/kernel-info.sh";
        interval = 90;
        format = "<label>";
        label-foreground = "${colorScheme.foreground}";
      };

      cpuConfig = {
        type = "internal/cpu";
        interval = 1;
        warn-percentage = 50;
        format = "";
        format-warn = "<label-warn>";
        label-warn = "cpu:%percentage%%";
        label-warn-foreground = "${colorScheme.red}";
      };

      memoryConfig = {
        type = "internal/memory";
        interval = 1;
        warn-percentage = 75;
        format = "";
        format-warn = "<label-warn>";
        label-warn = "mem:%percentage_used%%(%percentage_swap_used%%)";
        label-warn-foreground = "${colorScheme.red}";
      };
      
      wiredConfig{
        type = "internal/network";
        interface-type = "wired";
        label-connected = "%ifname%: (%local_ip%)";
        label-connected-foreground = "${colorScheme.cyan}";
      };

      wirelessConfig = {
        type = "internal/network";
        interface-type = "wireless";
        label-connected = "%ifname%: %essid% (%local_ip%)";
        label-connected-foreground = "${colorScheme.cyan}";
      };
      
      windowConfig = {
        type = "internal/xwindow";
        format = "<label>";
        label = "%title%";
        label-foreground = "${colorScheme.foreground}";
      };

      backlightConfig = {
        type = "internal/backlight";
        format = "<label>";
        label = "%percentage%%";
        label-foreground = "${colorScheme.foreground}";
        enable-scroll = true;
      };

    in {
      "bar/top" = barConfig "top" "kernel" "date" "backlight battery1 battery0 wired wireless";
      "bar/bot" = barConfig "bottom" "workspace" "" "cpu memory window";

      "module/battery0" = batteryConfig "BAT0";
      "module/battery1" = batteryConfig "BAT1";
      "module/cpu" = cpuConfig;
      "module/memory" = memoryConfig;
      "module/backlight" = backlightConfig;
      "module/wired" = wiredConfig;
      "module/wireless" = wirelessConfig;

      "module/date" = dateConfig
      "module/kernel" = systemInfoConfig;
      "module/workspaces" = workspaceConfig;
      "module/window" = windowConfig;
    };
  };
}

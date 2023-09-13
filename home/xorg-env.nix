{ inputs, lib, config, pkgs, ... }: {

  imports = [];
  
  programs.kitty = {
    enable = true;
    settings = {
      "background_opacity" = "1.0";
      "foreground"         = "#ebdbb2";
      "background"         = "#000000";
      "color0"             = "#181818";
      "color1"             = "#cc241d";
      "color2"             = "#98971a";
      "color3"             = "#d79921";
      "color4"             = "#458588";
      "color5"             = "#b16286";
      "color6"             = "#689d6a";
      "color7"             = "#a89984";
      "color8"             = "#928374";
      "color9"             = "#fb4934";
      "color10"            = "#b8bb26";
      "color11"            = "#fabd2f";
      "color12"            = "#83a598";
      "color13"            = "#d3869b";
      "color14"            = "#8ec07c";
      "color15"            = "#ebdbb2";
    };
  };

  services.sxhkd = let
    sxhkdTerminal = "kitty";
  in {
    enable = true;
    keybindings = {
      "super + Return" = "${sxhkdTerminal}";
      # ROFI
      "super + e" = "rofi -show run";
      "super + w" = "rofi -show window";
    };
  };

  xsession.windowManager.herbstluftwm =
  {
    enable = true;
    extraConfig = let
      XBACKGROUND = "#000000";
      XCOLOR0 = "#181818";
      XCOLOR7 = "#a89984";
      WALLPAPER = "/home/zamlz/usr/walls/nature/mountains_valley.jpg";
    in ''
      herbstclient attr theme.active.outer_color "${XBACKGROUND}"
      herbstclient attr theme.active.inner_color "${XBACKGROUND}"
      herbstclient attr theme.normal.color "${XCOLOR0}"
      herbstclient attr theme.active.color "${XCOLOR7}"
      herbstclient attr theme.urgent.color orange
      herbstclient attr theme.border_width 9
      herbstclient attr theme.inner_width 3
      herbstclient attr theme.outer_width 4
      herbstclient attr theme.inner_color "${XBACKGROUND}"
      herbstclient attr theme.outer_color "${XBACKGROUND}"
      herbstclient attr theme.floating.border_width 9
      herbstclient attr theme.floating.inner_width 3
      herbstclient attr theme.floating.outer_width 4
      herbstclient attr theme.floating.outer_color "${XBACKGROUND}"
      herbstclient attr theme.background_color "#141414"

      herbstclient emit_hook reload
      herbstclient detect_monitors

      # services
      # FIXME: this shouldn't be here either
      (pkill sxhkd; sleep 0.1; ${pkgs.sxhkd}/bin/sxhkd) &
      ${pkgs.feh}/bin/feh --no-fehbg --bg-fill '${WALLPAPER}' 

      # xorg settings
      # FIXME: this shouldn't belong here, atleast not with NIXOS
      xset r rate 400 50
      xset s off
      setxkbmap -option caps:escape
    '';
    # mod4 is SUPER
    # You can use xev to identify X11 keys very easily!
    keybinds = let
      super = "Mod4";
      resizestep = "0.01";
    in {
      # FIXME: SXHKD WORKAROUND STUFF
      "${super}-Ctrl-Alt-Escape" = "quit";

      # Reload WM and Close Window
      "${super}-Ctrl-Alt-r" = "chain , spawn $HOME/etc/xorg/refresh.sh , reload";
      "${super}-Ctrl-r" = "reload";
      "${super}-q" = "close";
      
      # Window Focus
      "${super}-Left" = "focus left";
      "${super}-Down" = "focus down";
      "${super}-Up" = "focus up";
      "${super}-Right" = "focus right";
      "${super}-h" = "focus left";
      "${super}-j" = "focus down";
      "${super}-k" = "focus up";
      "${super}-l" = "focus right";

      # defining keybindings for cycling the focused client
      #"${super}-BackSpace" = "cycle_monitor";  # FIXME: this is used already
      "${super}-Tab" = "cycle +1";
      "${super}-Shift-Tab" = "cycle_frame +1";
      "${super}-c" = "cycle_all +1";
      "${super}-i" = "jumpto urgent";

      # Window Movement
      "${super}-Shift-Left" = "shift left";
      "${super}-Shift-Down" = "shift down";
      "${super}-Shift-Up" = "shift up";
      "${super}-Shift-Right" = "shift right";
      "${super}-Shift-h" = "shift left";
      "${super}-Shift-j" = "shift down";
      "${super}-Shift-k" = "shift up";
      "${super}-Shift-l" = "shift right";

      # Splitting Frames
      "${super}-u" = "split bottom 0.5";
      "${super}-o" = "split right 0.5";
      "${super}-Control-space" = "split explode";

      # Resizing Frames
      "${super}-Control-h" = "resize left +${resizestep}";
      "${super}-Control-j" = "resize down +${resizestep}";
      "${super}-Control-k" = "resize up +${resizestep}";
      "${super}-Control-l" = "resize right +${resizestep}";
      "${super}-Control-Left" = "resize left +${resizestep}";
      "${super}-Control-Down" = "resize down +${resizestep}";
      "${super}-Control-Up" = "resize up +${resizestep}";
      "${super}-Control-Right" = "resize right +${resizestep}";

      # Workspace Movement
      "${super}-grave" = "use_previous";
      "${super}-bracketright" = "use_index +1 --skip-visible";
      "${super}-bracketleft" = "use_index -1 --skip-visible";
      "${super}-slash" = "spawn $HOME/etc/herbstluftwm/scripts/tag_utils.sh goto";
      "${super}-Shift-slash" = "spawn $HOME/etc/herbstluftwm/scripts/tag_utils.sh move";
      "${super}-BackSpace" = "spawn $HOME/etc/herbstluftwm/scripts/tag_utils.sh remove";
      
      # Layout Control
      "${super}-r" = "remove";
      "${super}-s" = "floating toggle";
      "${super}-f" = "fullscreen toggle";
      "${super}-t" = "pseudotile toggle";

      # The following cycles through the available layouts within a frame, but skips
      # layouts, if the layout change wouldn't affect the actual window positions.
      # (I.e. if there are two windows within a frame, the grid layout is skipped.)
      "${super}-space" = ''
        or , and . compare tags.focus.curframe_wcount = 2 \
                 . cycle_layout +1 vertical horizontal max vertical grid \
           , cycle_layout +1
      '';

      # Tag Definitions (workspaces)
      "${super}-1" = "use_index 0";
      "${super}-2" = "use_index 1";
      "${super}-3" = "use_index 2";
      "${super}-4" = "use_index 3";
      "${super}-5" = "use_index 4";
      "${super}-6" = "use_index 5";
      "${super}-7" = "use_index 6";
      "${super}-8" = "use_index 7";
      "${super}-9" = "use_index 8";
      "${super}-0" = "use_index 9";
      "${super}-Shift-1" = "move_index 0";
      "${super}-Shift-2" = "move_index 1";
      "${super}-Shift-3" = "move_index 2";
      "${super}-Shift-4" = "move_index 3";
      "${super}-Shift-5" = "move_index 4";
      "${super}-Shift-6" = "move_index 5";
      "${super}-Shift-7" = "move_index 6";
      "${super}-Shift-8" = "move_index 7";
      "${super}-Shift-9" = "move_index 8";
      "${super}-Shift-0" = "move_index 9";
    };
    mousebinds = let
      super = "Mod4";
    in {
      "${super}-Button1" = "move";
      "${super}-Button2" = "zoom";
      "${super}-Button3" = "resize";
    };
    rules = [
      "focus=on"
      "class~'(.*[Rr]xvt.*|.*[Tt]erm|Konsole)' focus=on"
      "class~'(Discord|DiscordCanary)' focus=off"
      "windowtype~'_NET_WM_WINDOW_TYPE_(DIALOG|UTILITY|SPLASH)' pseudotile=on"
      "windowtype='_NET_WM_WINDOW_TYPE_DIALOG' focus=on"
      "windowtype~'_NET_WM_WINDOW_TYPE_(NOTIFICATION|DOCK|DESKTOP)' manage=off"
    ];
    settings = let
      XBACKGROUND = "#000000";
      XCOLOR0 = "#181818";
      XCOLOR8 = "#928374";
    in {
      "frame_transparent_width" = 0;
      "frame_border_width" = 2;
      "frame_border_active_color" = "${XCOLOR8}";
      "frame_border_normal_color" = "#00000000";
      "frame_bg_transparent" = 1;
      "frame_bg_normal_color" = "${XBACKGROUND}";
      "frame_bg_active_color" = "${XCOLOR0}";
      "always_show_frame" = 0;
      "frame_gap" = 0;
      "frame_padding" = 0;
      "window_gap" = 0;
      "smart_window_surroundings" = 0;
      "smart_frame_surroundings" = 1;
      "mouse_recenter_gap" = 0;
      "tree_style" = "╾│ ├└╼─┐";
    };
    tags = [ "λ" "2" "3" "4" ];
  };

  programs.rofi = {
    enable = true;
    theme = ../etc/rofi/default-theme.rasi;
  };
}

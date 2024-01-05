{ inputs, lib, config, pkgs, ... }: {

  imports = [];

  wayland.windowManager.hyprland = {
    enable = true;
    settings = {
      exec-once = [
        "waybar"
        "waypaper --restore"
      ];
      general = {
        layout = "dwindle";
        resize_on_border = true;
        allow_tearing = false;
      };
      input = {
        follow_mouse = false;
        kb_options = "caps:escape";
        repeat_delay = 300;
        repeat_rate = 50;
      };
      gestures = {
        workspace_swipe = true;
        workspace_swipe_fingers = 4;
        workspace_swipe_distance = 300;
        workspace_swipe_min_speed_to_force = 0;
        workspace_swipe_cancel_ratio = 0.0;
        workspace_swipe_create_new = true;
      };
      misc = {
        layers_hog_keyboard_focus = false;
        disable_splash_rendering = true;
        force_default_wallpaper = 0;
        animate_manual_resizes = true;
        animate_mouse_windowdragging = true;
      };
      monitor = [ ",highrr,auto,1" ];
      bind = [
        # System controls
        "SUPER CTRL ALT SHIFT, Escape, exit"
        # Application launchers
        "SUPER, Return, exec, kitty"
        "SUPER, w, exec, firefox"
	# window control
        "SUPER SHIFT, Q, killactive"
        "SUPER, F, fullscreen"
        "SUPER, T, togglefloating"
        "SUPER, O, fakefullscreen"
        "SUPER, P, togglesplit"
        # window focus
        "SUPER, Tab, focuscurrentorlast"
        "SUPER, H, movefocus, l"
        "SUPER, J, movefocus, d"
        "SUPER, K, movefocus, u"
        "SUPER, L, movefocus, r"
        # workspace focus
        "SUPER, bracketleft, workspace, -1"
        "SUPER, bracketright, workspace, +1"
      ]
      # workspace focus & controls
      ++ (
        # binds $mod + [shift +] {1..10} to [move to] workspace {1..10}
        builtins.concatLists (builtins.genList (
            x: let
              ws = let
                c = (x + 1) / 10;
              in
                builtins.toString (x + 1 - (c * 10));
            in [
              "SUPER, ${ws}, workspace, ${toString (x + 1)}"
              "SUPER SHIFT, ${ws}, movetoworkspace, ${toString (x + 1)}"
            ]
          )
          10)
      );
    };
  };
}

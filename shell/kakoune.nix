{ inputs, lib, config, pkgs, ... }: {
  programs.kakoune = {
    enable = true;
    defaultEditor = true;
    config = {
      numberLines = {
        enable = true;
        highlightCursor = true;
        relative = false;
      };
      ui = {
        enableMouse = true;
        assistant = "cat";
        setTitle = true;
        statusLine = "bottom";
      };
    };
  };
}


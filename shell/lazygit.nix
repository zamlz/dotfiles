{ inputs, lib, config, pkgs, ... }: {
  programs.lazygit = {
    enable = true;
    settings = {
      gui.theme = {
        activeBorderColor = [ "blue" "bold" ];
        selectedLineBgColor = [ "black" ];
        selectedRangeBgColor = [ "black" ];
      };
      git = {
        paging.useConfig = true;
        mainBranches = [ "main" "master" "develop" ];
        overrideGpg = false;
      };
      os.edit = "floaterm";
    };
  };
}

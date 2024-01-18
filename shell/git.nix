{ inputs, lib, config, pkgs, ... }: {
  programs.git = {
    enable = true;
    userName = "Amlesh Sivanantham (zamlz)";
    userEmail = "zamlz@pm.me";
    signing = {
      signByDefault = true;
      key = "0x882C395C3B28902C";
    };
    aliases = {
      root = "rev-parse --show-toplevel";
    };
    extraConfig = {
      init = {
        defaultBranch = "main";
      };
      pull = {
        ff = "only";
      };
    };
  };
}

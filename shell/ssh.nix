{ inputs, lib, config, pkgs, ... }: {
  programs.ssh = {
    enable = true;
    matchBlocks = {
      # Since we are using GnuPG's GPG Agent as the SSH agent, when in a terminal,
      # ssh agent doesn't know that it has to change terminals (a bug in openssh). So
      # when it connects to gpg-agent, it uses the terminal it was last configured to
      # use. The following command when run in a terminal updates gpg-agent to use
      # the current terminal for openssh. However, now if we run some ssh related
      # command in the prior terminal, it will use the new terminal instead creating
      # the exact inverse of the problem. Therefore we attempt to fix this by running
      # this command before every SSH command.
      gpgAgentFix.match = "host * exec \"gpg-connect-agent --no-autostart UPDATESTARTUPTTY /bye\"";
    };
  };

}

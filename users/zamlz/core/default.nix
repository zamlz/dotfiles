{ inputs, lib, config, pkgs, ... }: {

  imports = [
    ./git.nix
    ./gpg.nix
    ./neovim
    ./ssh.nix
    ./tmux.nix
    ./zsh.nix
  ];
}

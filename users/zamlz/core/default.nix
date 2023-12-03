{ inputs, lib, config, pkgs, ... }: {

  imports = [
    ./git.nix
    ./gpg.nix
    ./neovim
    ./password-store.nix
    ./ssh.nix
    ./tmux.nix
    ./zsh.nix
  ];
}

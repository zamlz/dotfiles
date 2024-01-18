{ inputs, lib, config, pkgs, ... }: {
  imports = [
    ./git.nix
    ./gpg.nix
    ./neofetch
    ./neovim.nix
    ./password-store.nix
    ./ssh.nix
    ./tmux.nix
    ./zsh.nix
  ];
}

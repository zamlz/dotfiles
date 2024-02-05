{ inputs, lib, config, pkgs, ... }: {
  imports = [
    ./git.nix
    ./gpg.nix
    ./lazygit.nix
    ./neofetch.nix
    ./neovim.nix
    ./password-store.nix
    ./ranger.nix
    ./ssh.nix
    ./tmux.nix
    ./zsh.nix
  ];
}

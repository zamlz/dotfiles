# NixOS Configuration

A (WIP) NixOS Configuration repo for all my systems.

This repo is based on `bbigras/nix-config`'s and `lovesegfault/nix-config`'s repos

## Features
- [x] Use `flake.nix` 
- [ ] Disk Wipe on Reboot
- [ ] Full Disk Encryption
- [ ] Use `deploy-rs`

## Structure
```
.
├── core        # Baseline configuration applicabable to all machines
├── develop     # Developer tooling configuration
├── graphical   # herbstluftwm(X11) and sway(wayland) configuration for desktop
├── hardware    # Hardware specific configuration
├── hosts       # Machine definitions
├── nix         # Nix build support files
└── users       # Per-user confugrations
```

## Usage
WIP

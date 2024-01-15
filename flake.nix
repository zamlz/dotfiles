{
  description = "zamlz's NixOS config";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixvim = {
      url = "github:nix-community/nixvim";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # FIXME: Maybe use this if I really need to use home-manager in arch
    # nixgl.url = "github:nix-community/nixGL";
  };

  outputs = { self, nixpkgs, home-manager, nixvim }@inputs: {

    # NixOS Configuration Entrypoint
    # ( available through `nixos-rebuild switch --flake .#${hostname}` )

    nixosConfigurations = {
      NAVI-CoplandOS = nixpkgs.lib.nixosSystem {
        specialArgs = { inherit inputs; };
        modules = [ ./hosts/navi.nix ];
      };
    };

    # HomeManager Configuration Entrypoint
    # ( available through `home-manager switch --flake .#${username}` )

    homeConfigurations = {
      zamlz = home-manager.lib.homeManagerConfiguration {
        pkgs = nixpkgs.legacyPackages.x86_64-linux;
        extraSpecialArgs = { inherit inputs; };
        modules = [
          ./users/zamlz
          nixvim.homeManagerModules.nixvim
        ];
      };
    };
  };
}

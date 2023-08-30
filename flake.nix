{
  description = "My NixOS System";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.05";
    home-manager = {
      url = "github:nix-community/home-manager/release-23.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, home-manager, ... }@inputs: {

    # NixOS Configuration Entrypoint
    # ( available through `nixos-rebuild switch --flake .#${hostname}` )
    
    nixosConfigurations = {
      NAVI-CoplandOS = nixpkgs.lib.nixosSystem {
        specialArgs = { inherit inputs; };
	modules = [ ./nixos/configuration.nix ];
      };
    };

    # HomeManager Configuration Entrypoint
    # ( available through `home-manager switch --flake .#${username}` )

    homeConfigurations = {
      zamlz = home-manager.lib.homeManagerConfiguration {
        pkgs = nixpkgs.legacyPackages.x86_64-linux;
	extraSpecialArgs = { inherit inputs; };
	modules = [ ./home/zamlz.nix ];
      };
    };
  };
}

{
	description = "hackathon_cli shell flake";

	inputs = {
		nixpkgs.url = "github:nixos/nixpkgs/24.05";
		flake-utils.url = "github:numtide/flake-utils/v1.0.0";
	};

	outputs = {flake-utils, nixpkgs, self, ...}:
		flake-utils.lib.eachDefaultSystem (system:
			let pkgs = nixpkgs.legacyPackages."${system}";
			in {
				devShells.default = pkgs.mkShell {
					name = "hackathon_cli";
					packages = with pkgs; [dune_3];
				};
				packages.default = pkgs.ocamlPackages.buildDunePackage {
					pname = "hackathon_cli";
					version = "0.1.0";
					duneVersion = "3";
					src = self;
					strictDeps = true;

					buildInputs = with pkgs.ocamlPackages; [
						h2
					];
				};
			}
		);
}

{
  description = "Application to set wallpapers from reddit as desktop-background";
  inputs = {
    os_flake.url = "github:septias/nixos-config";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = inputs:
    with inputs;
      flake-utils.lib.eachDefaultSystem (
        system: let
          pkgs = nixpkgs-unstable.legacyPackages.${system};
        in {
          formatter = pkgs.alejandra;
          devShells.default = pkgs.mkShell {
            buildInputs = with pkgs; [
              typst
              typstyle
              tinymist
            ];
          };
        }
      );
}

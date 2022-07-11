{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    material-design = {
      url = "github:templarian/MaterialDesign";
      flake = false;
    };
  };

  outputs = inputs: with inputs; flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      reflex-dom-mdi = pkgs.callPackage ./reflex-dom-mdi { inherit material-design; };
      reflex-arc = pkgs.callPackage ./default.nix { inherit reflex-dom-mdi; };
    in
    {
      packages = {
        inherit reflex-arc reflex-dom-mdi;
        default = reflex-arc;
      };

      devShells.default = pkgs.callPackage ./shell.nix { inherit reflex-dom-mdi; };
    });
}

{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    reflex-dom = {
      url = "github:reflex-frp/reflex-dom";
      flake = false;
    };
    material-design = {
      url = "github:templarian/MaterialDesign";
      flake = false;
    };
    jsaddle = {
      url = "github:ghcjs/jsaddle";
      flake = false;
    };
    clay = {
      url = "github:sebastiaanvisser/clay";
      flake = false;
    };
  };

  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import inputs.nixpkgs { inherit system; };
      haskellPackages = pkgs.haskellPackages;
      reflex-dom = pkgs.callPackage ./nix/reflex-dom.nix {
        inherit inputs haskellPackages;
      };
      clay = pkgs.callPackage ./nix/clay.nix {
        inherit inputs haskellPackages;
      };
      reflex-dom-mdi = pkgs.callPackage ./reflex-dom-mdi {
        inherit reflex-dom;
        inherit (inputs) material-design;
      };
      reflex-arc = pkgs.callPackage ./default.nix {
        inherit reflex-dom reflex-dom-mdi clay;
      };
      haskellDeps = drv:
        builtins.foldl'
          (acc: type: acc ++ drv.getCabalDeps."${type}HaskellDepends")
          [ ] [ "executable" "library" "test" ];
    in
    {
      inherit pkgs;
      packages = {
        # inherit reflex-dom;
        inherit reflex-dom-mdi;
        inherit reflex-arc;
        default = reflex-arc;
      };

      devShells.default = pkgs.mkShell {
        nativeBuildInputs = [
          (haskellPackages.ghcWithPackages (ps: haskellDeps reflex-arc))
          haskellPackages.cabal-install
          haskellPackages.haskell-language-server
          haskellPackages.hpack
          pkgs.clang
        ];
        buildInputs = pkgs.lib.optionals pkgs.stdenv.isDarwin (with pkgs.darwin.apple_sdk.frameworks; [ Foundation ]);
      };

      # formatter = pkgs.nixfmt;
    });
}

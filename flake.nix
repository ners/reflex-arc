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
    web-font-mdi = {
      url = "github:ners/web-font-mdi";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
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
    with builtins;
    let
      pkgs = import inputs.nixpkgs { inherit system; };
      haskellPackages = pkgs.haskellPackages;
      reflex-dom = pkgs.callPackage ./nix/reflex-dom.nix {
        inherit inputs haskellPackages;
      };
      clay = pkgs.callPackage ./nix/clay.nix {
        inherit inputs haskellPackages;
      };
      reflex-arc = pkgs.callPackage ./default.nix {
        inherit reflex-dom clay;
        inherit (inputs.web-font-mdi.packages.${system}) web-font-mdi;
      };
      haskellDeps = drv: concatLists (attrValues drv.getCabalDeps);
    in
    {
      packages = {
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
    }
  );
}

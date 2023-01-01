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
      lib = inputs.nixpkgs.lib;
      parsedSystem = lib.systems.parse.mkSystemFromString system;
      isDarwin = parsedSystem.kernel.name == "darwin";
      darwinFrameworks = lib.optionals isDarwin (with pkgs.darwin.apple_sdk.frameworks; [
        Cocoa
        CoreServices
        Foundation
        WebKit
      ]);
      darwinOverride = drv: drv.overrideAttrs (attrs: lib.optionalAttrs isDarwin {
        buildInputs = (attrs.buildInputs or [ ]) ++ darwinFrameworks;
        NIX_LDFLAGS = "-F${pkgs.darwin.apple_sdk.frameworks.Foundation}/Library/Frameworks -framework Foundation";
      });
      inherit (pkgs.haskell.lib) doJailbreak;
      haskellPackages = pkgs.haskell.packages.ghc92.override {
        overrides = self: super: {
          # TODO: GHC 9.4
          #reflex = super.reflex_0_9_0_0;
          #hlint = doJailbreak super.hlint_3_5;
          #time = super.time_1_12_2;

          web-font-mdi = inputs.web-font-mdi.lib.build self;

          clay = (self.callCabal2nix "clay" inputs.clay { }).overrideAttrs (attrs: {
            patchPhase = ''
              sed -i 's/, Cursor(..)/&\n, CursorValue(..)/;s/, Display$/&(..)/' src/Clay/Display.hs
              sed -i 's/, FontWeight$/&(..)/' src/Clay/Font.hs
              sed -i 's/, Content$/&(..)/' src/Clay/Text.hs
            '';
          });
          jsaddle-wkwebview = darwinOverride (self.callCabal2nix
            "jsaddle-wkwebview"
            "${inputs.jsaddle}/jsaddle-wkwebview"
            { });
          reflex-arc = darwinOverride (self.callCabal2nix "reflex-arc" ./. { });
        };
      };
      haskellDeps = drv: concatLists (attrValues drv.getCabalDeps);
    in
    {
      packages = rec {
        inherit (haskellPackages) reflex-arc;
        default = reflex-arc;
      };

      devShells.default = pkgs.mkShell {
        nativeBuildInputs = with haskellPackages; [
          (ghcWithPackages (ps: haskellDeps reflex-arc))
          cabal-install
          haskell-language-server
          hpack
          pkgs.clang
        ];
        buildInputs = darwinFrameworks;
      };

      formatter = pkgs.nixpkgs-fmt;
    }
  );
}

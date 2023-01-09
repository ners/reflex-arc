{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
      inputs.flake-compat.follows = "flake-compat";
    };
    web-font-mdi = {
      url = "github:ners/web-font-mdi";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
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
      inherit;
      src = inputs.nix-filter.lib {
        root = ./.;
        include = [ "app" "src" "package.yaml" ];
      };
      haskellPackages = pkgs.haskell.packages.ghc92.override {
        overrides = self: super: with pkgs.haskell.lib; {
          reflex-arc = darwinOverride (self.callCabal2nix "reflex-arc" src { });

          # TODO: GHC 9.4
          #reflex = super.reflex_0_9_0_0;
          #hlint = doJailbreak super.hlint_3_5;
          #time = super.time_1_12_2;

          web-font-mdi = inputs.web-font-mdi.packages.${system}.default;

          clay = (self.callCabal2nix "clay" inputs.clay { }).overrideAttrs (attrs: {
            patchPhase = ''
              sed -i 's/, Cursor(..)/&\n, CursorValue(..)/;s/, Display$/&(..)/' src/Clay/Display.hs
              sed -i 's/, FontWeight$/&(..)/' src/Clay/Font.hs
              sed -i 's/, Content$/&(..)/' src/Clay/Text.hs
            '';
          });
        };
      };
    in
    rec {
      checks = {
        pre-commit-check = inputs.pre-commit-hooks.lib.${system}.run rec {
          src = ./.;
          settings.ormolu.defaultExtensions = [
            "ApplicativeDo"
            "DataKinds"
            "DefaultSignatures"
            "DeriveAnyClass"
            "DeriveGeneric"
            "DerivingStrategies"
            "DerivingVia"
            "ExplicitNamespaces"
            "ImportQualifiedPost"
            "OverloadedStrings"
            "RecordWildCards"
            "TypeFamilies"
          ];
          tools = {
            fourmolu = haskellPackages.fourmolu;
            nix-linter = haskellPackages.nix-linter;
          };
          hooks = {
            hlint.enable = true;
            nixpkgs-fmt.enable = true;
            #nix-linter.enable = true;
            statix.enable = true;
            fourmolu.enable = true;
            cabal-fmt.enable = true;
            shellcheck.enable = true;
          };
        };
      };

      packages = {
        inherit haskellPackages;
        default = haskellPackages.reflex-arc;
      };

      devShells.default = haskellPackages.shellFor {
        packages = _: [ packages.default ];
        nativeBuildInputs = with pkgs; with haskellPackages; [
          cabal-install
          clang
          fourmolu
          haskell-language-server
          hpack
        ];
        buildInputs = darwinFrameworks ++ [
          inputs.pre-commit-hooks.defaultPackage.${system}
        ];
        inherit (inputs.self.checks.${system}.pre-commit-check) shellHook;
      };

      formatter = pkgs.nixpkgs-fmt;
    }
  );
}

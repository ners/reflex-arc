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
  };

  outputs = inputs:
    with inputs;
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        lib = pkgs.lib;
        haskellPackages = pkgs.haskellPackages;
        callCabal2nix = haskellPackages.callCabal2nix;
        jsaddle-wkwebview = (callCabal2nix "jsaddle-wkwebview"
          "${inputs.jsaddle}/jsaddle-wkwebview" { }).overrideAttrs (attrs: {
            buildInputs = (attrs.buildInputs or [ ])
              ++ lib.optionals pkgs.stdenv.isDarwin
              (with pkgs.darwin.apple_sdk.frameworks; [
                Cocoa
                CoreServices
                WebKit
              ]);
          });
        reflex-dom =
          (callCabal2nix "reflex-dom" "${inputs.reflex-dom}/reflex-dom" {
            inherit jsaddle-wkwebview;
          }).overrideAttrs (attrs: {
            patchPhase = (attrs.patchPhase or "") + ''
              sed -i 's/base .*,/base,/' *.cabal
            '';
          });
        reflex-dom-mdi = pkgs.callPackage ./reflex-dom-mdi {
          inherit reflex-dom material-design;
        };
        reflex-arc = pkgs.callPackage ./default.nix {
          inherit reflex-dom reflex-dom-mdi;
        };
        haskellDeps = drv:
          builtins.foldl'
          (acc: type: acc ++ drv.getCabalDeps."${type}HaskellDepends")
          [ ] [ "executable" "library" "test" ];
      in {
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
          ];
        };

        # formatter = pkgs.nixfmt;
      });
}

{ pkgs, stdenv, haskellPackages, reflex-dom, reflex-dom-mdi, lib, ... }:

let
  reflex-arc =
    haskellPackages.callCabal2nix "reflex-arc" ./. { inherit reflex-dom reflex-dom-mdi; };
in reflex-arc.overrideAttrs (attrs:
  lib.optionalAttrs stdenv.isDarwin {
    NIX_LDFLAGS =
      "-F${pkgs.darwin.apple_sdk.frameworks.Foundation}/Library/Frameworks -framework Foundation";
    buildInputs = (attrs.buildInputs or [ ])
      ++ (with pkgs.darwin.apple_sdk.frameworks; [ Foundation ]);
  })

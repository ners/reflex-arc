{ pkgs, inputs, haskellPackages, ... }:

if pkgs.stdenv.isDarwin
then
  (haskellPackages.callCabal2nix "reflex-dom" "${inputs.reflex-dom}/reflex-dom" {
    jsaddle-wkwebview = (haskellPackages.ccallCabal2nix "jsaddle-wkwebview"
      "${inputs.jsaddle}/jsaddle-wkwebview"
      { }).overrideAttrs (attrs: {
      buildInputs = (attrs.buildInputs or [ ]) ++
        (with pkgs.darwin.apple_sdk.frameworks; [
          Cocoa
          CoreServices
          WebKit
        ]);
    });
  }).overrideAttrs
    (attrs: {
      patchPhase = (attrs.patchPhase or "") + ''
        sed -i 's/base .*,/base,/' *.cabal
      '';
    })
else haskellPackages.reflex-dom

{ pkgs, inputs, haskellPackages, ... }@args:

let jsaddle-wkwebview = haskellPackages.callCabal2nix
  "jsaddle-wkwebview"
  "${inputs.jsaddle}/jsaddle-wkwebview"
  { };
in
jsaddle-wkwebview.overrideAttrs (attrs: {
  buildInputs = (attrs.buildInputs or [ ]) ++
    (with pkgs.darwin.apple_sdk.frameworks; [
      Cocoa
      CoreServices
      WebKit
    ]);
})

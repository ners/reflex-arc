{ pkgs, inputs, haskellPackages, ... }@args:

if pkgs.stdenv.isDarwin
then
  let
    jsaddle-wkwebview = pkgs.callPackage ./jsaddle-wkwebview.nix args;
    reflex-dom = haskellPackages.callCabal2nix "reflex-dom" "${inputs.reflex-dom}/reflex-dom" {
      inherit jsaddle-wkwebview;
    };
  in
  reflex-dom.overrideAttrs (attrs: {
    patchPhase = (attrs.patchPhase or "") + ''
      sed -i 's/base .*,/base,/' *.cabal
    '';
  })
else haskellPackages.reflex-dom

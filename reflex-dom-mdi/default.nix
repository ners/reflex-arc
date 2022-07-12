{ haskellPackages, reflex-dom, material-design, ... }:

let
  mdi =
    haskellPackages.callCabal2nix "reflex-dom-mdi" ./. { inherit reflex-dom; };
in
mdi.overrideAttrs (attrs: {
  patchPhase = ''
    cp -r ${material-design}/svg svg
    ls svg/*.svg | runhaskell mkSrc.hs >> src/Reflex/Dom/MDI.hs
  '';
})

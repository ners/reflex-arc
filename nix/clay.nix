{ inputs, haskellPackages, ... }:

let clay = haskellPackages.callCabal2nix "clay" inputs.clay { };
in
clay.overrideAttrs (attrs: {
  patchPhase = ''
    sed -i 's/, Display$/&(..)/' src/Clay/Display.hs
    sed -i 's/, Content$/&(..)/' src/Clay/Text.hs
  '';
})

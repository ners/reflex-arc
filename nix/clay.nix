{ inputs, haskellPackages, ... }:

let clay = haskellPackages.callCabal2nix "clay" inputs.clay { };
in
clay.overrideAttrs (attrs: {
  patchPhase = ''
    sed -i 's/, Cursor(..)/&\n, CursorValue(..)/;s/, Display$/&(..)/' src/Clay/Display.hs
    sed -i 's/, FontWeight$/&(..)/' src/Clay/Font.hs
    sed -i 's/, Content$/&(..)/' src/Clay/Text.hs
  '';
})

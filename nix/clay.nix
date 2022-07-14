{ inputs, haskellPackages, ... }:

let clay = haskellPackages.callCabal2nix "clay" inputs.clay { };
in
clay.overrideAttrs (attrs: {
  patchPhase = ''
    echo HELLO
    exit 1
  '';
})

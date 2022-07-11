{ haskellPackages
, callPackage
, reflex-dom-mdi
, clang_14
, lld_14
, ...
}:

haskellPackages.callCabal2nix "reflex-arc" ./. { inherit reflex-dom-mdi; }

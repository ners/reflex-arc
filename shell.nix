{ mkShell
, callPackage
, haskellPackages
, reflex-dom-mdi
, clang_14
, ...
}:

let
  arc = callPackage ./default.nix { inherit reflex-dom-mdi; };
  haskellDeps = drv: builtins.foldl'
    (acc: type: acc ++ drv.getCabalDeps."${type}HaskellDepends")
    [ ]
    [ "executable" "library" "test" ];
in
mkShell {
  nativeBuildInputs = [
    (haskellPackages.ghcWithPackages (ps: haskellDeps arc))
    haskellPackages.cabal-install
    haskellPackages.haskell-language-server
    haskellPackages.hpack
    clang_14
  ];
}

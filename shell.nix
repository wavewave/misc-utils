{ pkgs ? (import <nixpkgs>{}) }:

with pkgs;

let hsenv = haskellPackages.ghcWithPackages (p: with p; [
              #xml-conduit split unordered-containers
              #free gtk2hs-buildtools
              #zlib
              split
              data-default
              Chart
              Chart-cairo
            ]);
in stdenv.mkDerivation {
     name = "ghc-shell";
     buildInputs = [ hsenv ]; # gd expat pkgconfig cairo glib gtk3 librsvg poppler libxml2 ];
     shellHook = ''
     '';
   }
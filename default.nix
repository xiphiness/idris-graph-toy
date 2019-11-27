with import <nixpkgs> {};
{
  idris-graph-toy = idrisPackages.callPackage ./graph-toy.nix {};
}

{ pkgs }:

self: super:

with { inherit (pkgs.stdenv) lib; };

with pkgs.haskell.lib;

{
  knuckles = (
    with rec {
      knucklesSource = pkgs.lib.cleanSource ../.;
      knucklesBasic  = self.callCabal2nix "knuckles" knucklesSource { };
    };
    overrideCabal knucklesBasic (old: {
    })
  );
}

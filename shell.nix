{ package ? "dhcp-lease-parser", compiler ? "ghc822" }:

(import ./default.nix {
  inherit package compiler;
}).dhcp-lease-parser

{ package ? "dhcp-lease-parser", compiler ? "ghc841" }:

(import ./default.nix {
  inherit package compiler;
}).dhcp-lease-parser

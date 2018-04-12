{ package ? "dhcp-lease-parser", compiler ? "ghc841" }:
let fetchNixpkgs = import ./nix/fetchNixpkgs.nix;
    nixpkgs = fetchNixpkgs {
      rev = "c484079ac7b4cf003f6b09e64cde59cb9a98b923";
      sha256 = "0sh4f8w30sya7vydwm86dni1ylz59hiq627df1dv1zg7riq036cw";
      sha256unpacked = "0fc6y2yjlfbss7cq7lgah0xvlnyas5v3is9r5bxyyp7rkwlyvny4";
    };
    pkgs = import nixpkgs { config = {}; overlays = []; };
    inherit (pkgs) haskell;
 
  filterPredicate = p: type:
    let path = baseNameOf p; in !(
       (type == "directory" && path == "dist")
    || (type == "symlink"   && path == "result")
    || (type == "directory" && path == ".git")
    || (type == "symlink"   && pkgs.lib.hasPrefix "result" path)
    || pkgs.lib.hasSuffix "~" path
    || pkgs.lib.hasSuffix ".o" path
    || pkgs.lib.hasSuffix ".so" path
    || pkgs.lib.hasSuffix ".nix" path);
    
  overrides = haskell.packages.${compiler}.override {
    overrides = self: super:
    with haskell.lib;
    with { cp = file: (self.callPackage (./nix/haskell + "/${file}.nix") {}); 
           build = name: path: self.callCabal2nix name (builtins.filterSource filterPredicate path) {}; 
         };
    {
      ip = dontCheck (cp "ip"); 
      quickcheck-classes = dontCheck (cp "quickcheck-classes"); 
      prim-array = dontCheck (cp "prim-array"); 
      chronos = dontCheck (cp "chronos"); 
      dhcp-lease-parser = build "dhcp-lease-parser" ./.;
    };
  };
in rec {
  drv = overrides.${package};
  dhcp-lease-parser = if pkgs.lib.inNixShell then drv.env else drv;
}

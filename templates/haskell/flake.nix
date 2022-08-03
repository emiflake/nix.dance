{
  description = "template";

  inputs.nixpkgs-latest.url = "github:NixOS/nixpkgs?rev=a0a69be4b5ee63f1b5e75887a406e9194012b492";
  inputs.nixpkgs-2111 = { url = "github:NixOS/nixpkgs/nixpkgs-21.11-darwin"; };

  inputs.haskell-nix-extra-hackage.url = "github:mlabs-haskell/haskell-nix-extra-hackage";
  inputs.haskell-nix-extra-hackage.inputs.haskell-nix.follows = "haskell-nix";
  inputs.haskell-nix-extra-hackage.inputs.nixpkgs.follows = "nixpkgs";
  inputs.haskell-nix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
  inputs.iohk-nix.url = "github:input-output-hk/iohk-nix";
  inputs.iohk-nix.flake = false;
  inputs.haskell-language-server.url = "github:haskell/haskell-language-server";

  inputs.liqwid-nix.url = "github:Liqwid-Labs/liqwid-nix";

  outputs = inputs@{ liqwid-nix, ... }:
    (liqwid-nix.buildProject
      {
        inherit inputs;
        src = ./.;
      }
      [
        liqwid-nix.haskellProject
      ]
    ).toFlake;
}


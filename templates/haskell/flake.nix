{
  description = "template";

  inputs.haskell-nix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
  inputs.nixpkgs-latest.url = "github:NixOS/nixpkgs?rev=a0a69be4b5ee63f1b5e75887a406e9194012b492";
  inputs.nixpkgs-2111 = { url = "github:NixOS/nixpkgs/nixpkgs-21.11-darwin"; };

  inputs.haskell-language-server.url = "github:haskell/haskell-language-server";
  inputs.haskell-language-server.flake = false;

  outputs = inputs@{ self, nixpkgs, nixpkgs-latest, haskell-nix, ... }:
    let
      supportedSystems = nixpkgs-latest.lib.systems.flakeExposed;

      perSystem = nixpkgs.lib.genAttrs supportedSystems;

      pkgsFor = system: import nixpkgs {
        inherit system;
        overlays = [ haskell-nix.overlay ];
      };
      pkgsFor' = system: import nixpkgs-latest { inherit system; };

      fourmoluFor = system: (pkgsFor' system).haskell.packages.ghc922.fourmolu_0_6_0_0;

      defaultGhcVersion = "ghc923";

      nonReinstallablePkgs = [
        "array"
        "base"
        "binary"
        "bytestring"
        "Cabal"
        "containers"
        "deepseq"
        "directory"
        "exceptions"
        "filepath"
        "ghc"
        "ghc-bignum"
        "ghc-boot"
        "ghc-boot-th"
        "ghc-compact"
        "ghc-heap"
        # "ghci"
        # "haskeline"
        "ghcjs-prim"
        "ghcjs-th"
        "ghc-prim"
        "hpc"
        "integer-gmp"
        "integer-simple"
        "mtl"
        "parsec"
        "pretty"
        "process"
        "rts"
        "stm"
        "template-haskell"
        "terminfo"
        "text"
        "time"
        "transformers"
        "unix"
        "Win32"
        "xhtml"
      ];

      haskellModules = [
        ({ config, pkgs, hsPkgs, ... }: {
          inherit nonReinstallablePkgs; # Needed for a lot of different things
          packages = {
          };
        })
      ];

      hlsFor' = compiler-nix-name: system:
        let pkgs = pkgsFor system; in
        pkgs.haskell-nix.cabalProject' {
          modules = [{
            inherit nonReinstallablePkgs;
            reinstallableLibGhc = true;
          }];
          inherit compiler-nix-name;
          src = "${inputs.haskell-language-server}";
          sha256map."https://github.com/pepeiborra/ekg-json"."7a0af7a8fd38045fd15fb13445bdcc7085325460" = "fVwKxGgM0S4Kv/4egVAAiAjV7QB5PBqMVMCfsv7otIQ=";
        };
      hlsFor = compiler-nix-name: system: (hlsFor' compiler-nix-name system).hsPkgs.haskell-language-server.components.exes.haskell-language-server;

      projectForGhc = compiler-nix-name: system:
        let pkgs = pkgsFor system; in
        let pkgs' = pkgsFor' system; in
        let pkgSet = pkgs.haskell-nix.cabalProject' {
              src = ./.;
              inherit compiler-nix-name;
              modules = [ ];
              shell = {
                withHoogle = true;

                exactDeps = true;

                # We use the ones from Nixpkgs, since they are cached reliably.
                # Eventually we will probably want to build these with haskell.nix.
                nativeBuildInputs = [
                  pkgs'.cabal-install
                  pkgs'.hlint
                  pkgs'.haskellPackages.cabal-fmt
                  (fourmoluFor system)
                  pkgs'.nixpkgs-fmt
                  (hlsFor compiler-nix-name system)
                ];
              };
            }; in
        pkgSet;

      projectFor = projectForGhc defaultGhcVersion;
    in
    {
      project = perSystem projectFor;
      flake = perSystem (system: (projectFor system).flake { });

      packages = perSystem (system:
        self.flake.${system}.packages // { });

      # Define what we want to test
      checks = perSystem (system:
        self.flake.${system}.checks // { });
      check = perSystem (system:
        (pkgsFor system).runCommand "combined-test"
          {
            checksss = builtins.attrValues self.checks.${system};
          } ''
          echo $checksss
          touch $out
        '');
      devShell = perSystem (system: self.flake.${system}.devShell);
    };
}


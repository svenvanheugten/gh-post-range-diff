{
  description = "Post `git range-diff` on a PR after a force-push";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-26.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };

        # MultilineStrings requires GHC >= 9.12.
        hsPkgs = pkgs.haskell.packages.ghc912;

        ghc = hsPkgs.ghcWithPackages (ps: [
          ps.process
          ps.extra
          ps.hspec
        ]);

        runtimeDeps = [
          pkgs.git
          pkgs.gh
        ];
      in
      {
        devShells.default = pkgs.mkShell {
          packages = [
            ghc
            hsPkgs.haskell-language-server
            hsPkgs.hlint
            hsPkgs.fourmolu
          ]
          ++ runtimeDeps;

          shellHook = ''
            echo "Run: runghc Main.hs <pr-number>"
          '';
        };

        apps.default = {
          type = "app";
          program = "${self.packages.${system}.default}/bin/gh-post-range-diff";
        };

        packages.default = pkgs.stdenv.mkDerivation {
          pname = "gh-post-range-diff";
          version = "0.1.0";
          src = self;
          meta.mainProgram = "gh-post-range-diff";

          nativeBuildInputs = [
            ghc
            pkgs.makeWrapper
          ];

          buildPhase = ''
            ghc -O2 -Wall -o gh-post-range-diff Main.hs
          '';

          installPhase = ''
            mkdir -p $out/bin
            install -m755 gh-post-range-diff $out/bin/gh-post-range-diff
            wrapProgram $out/bin/gh-post-range-diff \
              --prefix PATH : ${pkgs.lib.makeBinPath runtimeDeps}
          '';
        };

        checks = {
          lint = pkgs.runCommand "hlint" { nativeBuildInputs = [ hsPkgs.hlint ]; } ''
            hlint ${self}/*.hs
            touch $out
          '';

          format = pkgs.runCommand "fourmolu" { nativeBuildInputs = [ hsPkgs.fourmolu ]; } ''
            fourmolu --mode check ${self}/*.hs
            touch $out
          '';

          test = pkgs.runCommand "test" { nativeBuildInputs = [ ghc pkgs.git ]; } ''
            export HOME=$TMPDIR
            runghc -i${self} ${self}/Test.hs
            touch $out
          '';
        };
      }
    );
}

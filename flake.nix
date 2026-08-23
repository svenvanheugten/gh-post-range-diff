{
  description = "Post `git range-diff` on a PR after a force-push";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-26.05";
    flake-utils.url = "github:numtide/flake-utils";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      treefmt-nix,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
        hsLib = pkgs.haskell.lib;

        hsPkgs = pkgs.haskellPackages;

        treefmtEval = treefmt-nix.lib.evalModule pkgs {
          projectRootFile = "flake.nix";
          programs.ormolu.enable = true;
          programs.nixfmt.enable = true;
        };

        runtimeDeps = [
          pkgs.git
          pkgs.gh
        ];

        # The test suite builds real repos: it rewrites them with jujutsu and
        # reads them back with git.
        testDeps = [
          pkgs.git
          pkgs.jujutsu
        ];

        gh-post-range-diff = hsLib.overrideCabal (hsPkgs.callCabal2nix "gh-post-range-diff" ./. { }) (old: {
          doCheck = true;
          testToolDepends = (old.testToolDepends or [ ]) ++ testDeps;
        });
      in
      {
        devShells.default = hsPkgs.shellFor {
          packages = _: [ gh-post-range-diff ];

          nativeBuildInputs = [
            hsPkgs.cabal-install
            hsPkgs.haskell-language-server
            hsPkgs.hlint
            treefmtEval.config.build.wrapper
          ]
          ++ runtimeDeps
          ++ testDeps;

          shellHook = ''
            echo "Run:    cabal run gh-post-range-diff -- <pr-number>"
            echo "Format: nix fmt"
          '';
        };

        formatter = treefmtEval.config.build.wrapper;

        apps.default = {
          type = "app";
          program = "${self.packages.${system}.default}/bin/gh-post-range-diff";
        };

        packages.default =
          pkgs.runCommand "gh-post-range-diff-${gh-post-range-diff.version}"
            {
              nativeBuildInputs = [ pkgs.makeWrapper ];
              meta.mainProgram = "gh-post-range-diff";
            }
            ''
              mkdir -p $out/bin
              makeWrapper ${hsLib.justStaticExecutables gh-post-range-diff}/bin/gh-post-range-diff \
                $out/bin/gh-post-range-diff \
                --prefix PATH : ${pkgs.lib.makeBinPath runtimeDeps}
            '';

        checks = {
          # Same derivation `nix build` produces, so the test suite is not
          # built and run a second time.
          build = self.packages.${system}.default;

          lint = pkgs.runCommand "hlint" { nativeBuildInputs = [ hsPkgs.hlint ]; } ''
            cd ${self}
            hlint src app test
            touch $out
          '';

          format = treefmtEval.config.build.check self;
        };
      }
    );
}

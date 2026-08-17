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
        hsLib = pkgs.haskell.lib;

        hsPkgs = pkgs.haskellPackages;

        runtimeDeps = [
          pkgs.git
          pkgs.gh
        ];

        gh-post-range-diff = hsLib.overrideCabal (hsPkgs.callCabal2nix "gh-post-range-diff" ./. { }) (old: {
          doCheck = true;
          testToolDepends = (old.testToolDepends or [ ]) ++ [ pkgs.git ];
        });
      in
      {
        devShells.default = hsPkgs.shellFor {
          packages = _: [ gh-post-range-diff ];

          nativeBuildInputs = [
            hsPkgs.cabal-install
            hsPkgs.haskell-language-server
            hsPkgs.hlint
            hsPkgs.fourmolu
          ]
          ++ runtimeDeps;

          shellHook = ''
            echo "Run: cabal run gh-post-range-diff -- <pr-number>"
          '';
        };

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
          build = gh-post-range-diff;

          lint = pkgs.runCommand "hlint" { nativeBuildInputs = [ hsPkgs.hlint ]; } ''
            cd ${self}
            hlint src app test
            touch $out
          '';

          format = pkgs.runCommand "fourmolu" { nativeBuildInputs = [ hsPkgs.fourmolu ]; } ''
            cd ${self}
            fourmolu --mode check src app test
            touch $out
          '';
        };
      }
    );
}

{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    dart.url = "github:roman-vanesyan/dart-overlay";
  };

  outputs =
    {
      self,
      flake-utils,
      nixpkgs,
      dart,
      ...
    }@inputs:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            dart.overlays.default
          ];
        };
        buildInputs = [
          pkgs.jdk23
          pkgs.gnumake
          pkgs.dartpkgs."2.19.6"
        ];
      in
      {
        devShells.default = pkgs.mkShell {
          name = "devshell";
          inherit buildInputs;
          shellHook = ''
            echo "Welcome to the development shell"
          '';
        };

        checks = {
          default =
            pkgs.runCommand "test-suite"
              {
                inherit buildInputs;
                PATH = pkgs.lib.makeBinPath buildInputs;
              }
              ''
                echo "Running tests..."
                cabal build
                cd craftinginterpreters
                dart tool/bin/test.dart chap10_functions --interpreter ../dist-newstyle/build/aarch64-osx/ghc-9.6.5/xolsh-0.1.0.0/x/xolsh-exe/build/xolsh-exe/xolsh-exe
                echo "Tests completed successfully."
                touch $out
              '';
        };
      }
    );
}

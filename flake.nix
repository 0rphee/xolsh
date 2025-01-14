{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    flake-utils.url = "github:numtide/flake-utils";

    clean-devshell.url = "github:ZentriaMC/clean-devshell";

    dart.url = "github:roman-vanesyan/dart-overlay";
    dart.inputs.nixpkgs.follows = "nixpkgs";
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
        jlox-test = pkgs.writeShellScriptBin "jlox-test" ''
          #!/usr/bin/env bash
          cabal build xolsh-exe
          EXE_PATH=$(cabal list-bin xolsh-exe)
          cd craftinginterpreters
          dart tool/bin/test.dart chap13_inheritance --interpreter $EXE_PATH
          unset EXE_PATH && cd .. && echo "Tests completed successfully."
        '';
        mkShell = pkgs.callPackage inputs.clean-devshell.lib.mkDevShell { };
        packages = [
          pkgs.gnumake
          pkgs.dartpkgs."2.19.6"
          jlox-test
        ];
      in
      {
        # pkgs.mkShell
        devShells.default = mkShell {
          name = "xolsh";
          inherit packages;
          shellHook = ''
            echo "Welcome to the development shell."
          '';
        };
      }
    );
}

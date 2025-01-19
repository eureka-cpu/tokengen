{
  description = ''
    A simple token generator rust library
  '';

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    crane.url = "github:ipetkov/crane";

    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.rust-analyzer-src.follows = "";
    };

    flake-utils.url = "github:numtide/flake-utils";

    advisory-db = {
      url = "github:rustsec/advisory-db";
      flake = false;
    };

    nix-core = {
      url = "github:Cloud-Scythe-Labs/nix-core";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.fenix.follows = "fenix";
    };
  };

  outputs =
    { self
    , nixpkgs
    , crane
    , fenix
    , flake-utils
    , advisory-db
    , nix-core
    , ...
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      inherit (pkgs) lib;
      pkgs = nixpkgs.legacyPackages.${system};

      rustToolchain = nix-core.toolchains.${system}.mkRustToolchainFromTOML
        ./.rust-toolchain.toml
        "sha256-s1RPtyvDGJaX/BisLT+ifVfuhDT1nZkZ1NcK8sbwELM=";
      craneLib = (crane.mkLib pkgs).overrideToolchain rustToolchain.fenix-pkgs;
      workspace = rec {
        root = ./.;
        src = craneLib.cleanCargoSource root;
      };

      commonArgs = {
        inherit (workspace) src;
        strictDeps = true;

        buildInputs = lib.optionals pkgs.stdenv.isDarwin [
          pkgs.libiconv
        ];
      };

      craneLibLLvmTools = craneLib.overrideToolchain
        (fenix.packages.${system}.complete.withComponents [
          "cargo"
          "llvm-tools"
          "rustc"
        ]);

      cargoArtifacts = craneLib.buildDepsOnly commonArgs;

      individualCrateArgs = commonArgs // {
        inherit cargoArtifacts;
        inherit (craneLib.crateNameFromCargoToml { inherit (workspace) src; }) version;
        doCheck = false;
      };

      fileSetForCrate = crate: deps: lib.fileset.toSource {
        inherit (workspace) root;
        fileset = lib.fileset.unions ([
          ./Cargo.toml
          ./Cargo.lock
          (craneLib.fileset.commonCargoSources crate)
        ] ++ deps);
      };

      tokengen = craneLib.buildPackage (individualCrateArgs // {
        pname = "tokengen";
        cargoExtraArgs = "-p tokengen";
        src = fileSetForCrate ./crates/tokengen [ ./crates/tokengen-derive ];
      });
    in
    {
      checks = {
        inherit tokengen;

        cargo-clippy = craneLib.cargoClippy (commonArgs // {
          inherit cargoArtifacts;
          cargoClippyExtraArgs = "--all-targets -- --deny warnings";
        });

        cargo-doc = craneLib.cargoDoc (commonArgs // {
          inherit cargoArtifacts;
        });

        cargo-fmt = craneLib.cargoFmt {
          inherit (workspace) src;
        };

        toml-fmt = craneLib.taploFmt {
          src = lib.sources.sourceFilesBySuffices workspace.src [ ".toml" ];
        };

        cargo-audit = craneLib.cargoAudit {
          inherit (workspace) src;
          inherit advisory-db;
        };

        cargo-deny = craneLib.cargoDeny {
          inherit (workspace) src;
        };

        cargo-nextest = craneLib.cargoNextest (commonArgs // {
          inherit cargoArtifacts;
          partitions = 1;
          partitionType = "count";
        });
      };

      packages = {
        inherit tokengen;
      } // lib.optionalAttrs (!pkgs.stdenv.isDarwin) {
        my-workspace-llvm-coverage = craneLibLLvmTools.cargoLlvmCov (commonArgs // {
          inherit cargoArtifacts;
        });
      };

      devShells.default = craneLib.devShell {
        checks = self.checks.${system};

        packages = with pkgs; [
          nil
        ];
      };

      formatter = pkgs.nixpkgs-fmt;
    });
}

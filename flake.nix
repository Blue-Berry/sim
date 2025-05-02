{
  description = "Description for ocaml project";

  inputs = {
    nixpkgs.url = "github:nix-ocaml/nix-overlays";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = inputs @ {flake-parts, ...}:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = ["x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin"];

      perSystem = {
        config,
        self',
        inputs',
        pkgs,
        system,
        ...
      }: let
        inherit (pkgs) dockerTools mkShell;
        inherit (dockerTools) buildImage;
        # Use specific version of ocamlPackages
        # inherit (pkgs) ocamlPackages;
        ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_3;
        inherit (ocamlPackages) buildDunePackage;
        name = "sim";
        version = "0.0.1";
      in {
        devShells = {
          default = mkShell {
            inputsFrom = [self'.packages.default];
            buildInputs = with ocamlPackages; [
                camlzip
              utop
              ocamlformat
              # patch ocaml-lsp so that inlay hints dont hide ghost values
              (ocamlPackages.ocaml-lsp.overrideAttrs (oldAttrs: {
                patches = [
                  ./inlay-hints.patch
                ];
              }))
              pkgs.cargo-flamegraph
            ];
          };
        };

        packages = {
          default = buildDunePackage {
            inherit version;
            pname = name;
            src = ./.;
            buildInputs = with ocamlPackages; [
              core
              owl
              ppx_jane
              graphics
              core_bench
            ];
          };

          docker = buildImage {
            inherit name;
            tag = version;
            config = {
              Cmd = ["${self'.packages.default}/bin/${name}"];
              Env = [
                "SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
              ];
            };
          };
        };
      };
    };
}

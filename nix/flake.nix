{
  description = "A startup Guile project with devshell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
    devshell = {
      url = "github:numtide/devshell";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [ inputs.devshell.flakeModule ];

      perSystem =
        {
          pkgs,
          self',
          lib,
          ...
        }:
        {
          packages.blue = pkgs.callPackage ./pkgs/blue.nix { };
          devshells.default =
            let
              inherit (pkgs) guile;
            in
            {
              packages = with pkgs; [
                guile
                guile-hoot
                self'.packages.blue
              ];
              env =
                let
                  makeAclocalPath = lib.makeSearchPathOutput "dev" "share/aclocal";
                  makePkgconfigPath = pkgs.lib.makeSearchPathOutput "dev" "lib/pkgconfig";
                in
                [
                  {
                    name = "GUILE_LOAD_PATH";
                    prefix = "$DEVSHELL_DIR/${guile.siteDir}";
                  }
                  {
                    name = "ACLOCAL_PATH";
                    prefix = makeAclocalPath [
                      guile
                      pkgs.pkg-config
                    ];
                  }
                  {
                    name = "PKG_CONFIG_PATH";
                    prefix = makePkgconfigPath [ guile ];
                  }
                ];
            };
        };

      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
        "x86_64-darwin"
      ];
    };
}

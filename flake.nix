{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  };

  outputs = {
    self,
    nixpkgs,
  }: let
    forAllSystems = nixpkgs.lib.genAttrs [
      "aarch64-darwin"
      "x86_64-darwin"
      "x86_64-linux"
      "aarch64-linux"
    ];
  in {
    devShells = forAllSystems (system: let
      pkgs = nixpkgs.legacyPackages.${system};

      sbclWithDeps = pkgs.sbcl.withPackages (ps: [
        ps.swank
        ps.alexandria
        ps.yason
        ps.vom
        ps.usocket
      ]);

      vlime-src = pkgs.fetchFromGitHub {
        owner = "vlime";
        repo = "vlime";
        rev = "e276e9a6f37d2699a3caa63be19314f5a19a1481";
        hash = "sha256-tCqN80lgj11ggzGmuGF077oqL5ByjUp6jVmRUTrIWJA=";
      };

      vlime-server = pkgs.writeShellScriptBin "vlime-server" ''
        exec ${sbclWithDeps}/bin/sbcl --noinform \
          --load ${vlime-src}/lisp/start-vlime.lisp
      '';
    in {
      default = pkgs.mkShell {
        packages = [
          sbclWithDeps
          vlime-server
          pkgs.texliveMedium
        ];
      };
    });
  };
}

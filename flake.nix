{
  description = "Lurk lisp";
  inputs = {
    nixpkgs.url = github:nixos/nixpkgs;
    flake-utils = {
      url = github:numtide/flake-utils;
      inputs.nixpkgs.follows = "nixpkgs";
    };
    naersk = {
      url = github:yatima-inc/naersk;
      inputs.nixpkgs.follows = "nixpkgs";
    };
    utils = {
      url = github:yatima-inc/nix-utils;
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.naersk.follows = "naersk";
    };
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    , utils
    , naersk
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      lib = utils.lib.${system};
      pkgs = nixpkgs.legacyPackages.${system};
      name = "lurk";
      project = pkgs.stdenv.mkDerivation {
        src = ./.;
      };
    in
    {
      # packages.${name} = project;

      # defaultPackage = self.packages.${system}.${name};

      # To run with `nix run`
      # apps.${name} = flake-utils.lib.mkApp {
      #   drv = project;
      # };

      # `nix develop`
      devShell = pkgs.mkShell {
        buildInputs = with pkgs; [
          lispPackages.quicklisp
          sbcl
        ];
        QUICKLISP = "${pkgs.lispPackages.quicklisp}";
      };
    });
}

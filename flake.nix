{
  description = "Nix flake for R package development";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default = pkgs.mkShell {
          packages = [ pkgs.bashInteractive ];
          buildInputs = with pkgs; [
            # Software
            curl
            gettext
            gh
            git
            pandoc
            quarto
            R
            radianWrapper
            # R packages: development
            rPackages.devtools
            rPackages.httr
            rPackages.httptest
            rPackages.languageserver
            rPackages.pak
            rPackages.rcompendium
            rPackages.testthat
            rPackages.usethis
            rPackages.goodpractice
            # R packages: package
            rPackages.dplyr
            rPackages.fs
            rPackages.ggplot2
            rPackages.glue
            rPackages.gutenbergr
            rPackages.kableExtra
            rPackages.knitr
            rPackages.Matrix
            rPackages.openai
            rPackages.purrr
            rPackages.readr
            rPackages.rlang
            rPackages.rmarkdown
            rPackages.sessioninfo
            rPackages.stringr
            rPackages.tibble
            rPackages.tidyr
            rPackages.tidytext
            rPackages.webshot2
          ];

          shellHook = ''
            export R_LIBS_USER=$PWD/Library
          '';
        };
      });
}

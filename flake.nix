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
            imagemagick
            pandoc
            quarto
            R
            radianWrapper
            # R packages: development
            rPackages.devtools
            rPackages.formatR
            rPackages.goodpractice
            rPackages.httptest
            rPackages.httr
            rPackages.languageserver
            rPackages.pak
            rPackages.profvis
            rPackages.testthat
            rPackages.usethis
            # R packages: package
            rPackages.Matrix
            rPackages.dplyr
            rPackages.fs
            rPackages.ggplot2
            rPackages.glue
            rPackages.gutenbergr
            rPackages.kableExtra
            rPackages.knitr
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
            rPackages.xml2
          ];

          shellHook = ''
            export R_LIBS_USER=$PWD/Library
            mkdir -p "$R_LIBS_USER"
          '';
        };
      });
}

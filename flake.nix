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
            gettext
            gh
            git
            pandoc
            quarto
            R
            radianWrapper
            # R packages: development
            rPackages.devtools
            rPackages.pkgdown
            rPackages.languageserver
            rPackages.testthat
            rPackages.usethis
            rPackages.goodpractice
            # R packages: package
            rPackages.Matrix
            rPackages.dplyr
            rPackages.ggplot2
            rPackages.kableExtra
            rPackages.knitr
            rPackages.rlang
            rPackages.rmarkdown
            rPackages.sessioninfo
            rPackages.tibble
            rPackages.tidytext
            rPackages.webshot2
          ];
        };
      });
}

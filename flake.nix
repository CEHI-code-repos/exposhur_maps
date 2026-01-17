{
  description = "Dev env";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/8f73eb3044e416e8f6f92ffd91e9b1108c354751";
  };

  outputs =
    { self, nixpkgs }:
    let
      # You can add more systems here if needed (e.g., "aarch64-linux")
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };

      rpkgs = with pkgs.rPackages; [
        patchwork
        sf
        tidyverse
        tigris
        units
      ];

      system_packages = with pkgs; [
        glibcLocales
        nix
        R
      ];
    in
    {
      devShells.${system}.default = pkgs.mkShell {
        buildInputs = [
          rpkgs
          system_packages
        ];

        # Environment variables
        LOCALE_ARCHIVE =
          if pkgs.stdenv.hostPlatform.system == "x86_64-linux" then
            "${pkgs.glibcLocales}/lib/locale/locale-archive"
          else
            "";
        LANG = "en_US.UTF-8";
        LC_ALL = "en_US.UTF-8";
        LC_TIME = "en_US.UTF-8";
        LC_MONETARY = "en_US.UTF-8";
        LC_PAPER = "en_US.UTF-8";
        LC_MEASUREMENT = "en_US.UTF-8";
      };
    };
}

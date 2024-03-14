{
  description = "DAT151-Lab1-Environment";
  inputs = { nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.05"; };
  outputs = inputs@{ self, nixpkgs }:
    let
      # GENERAL
      supportedSystems =
        [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      perSystem = nixpkgs.lib.genAttrs supportedSystems;
      nixpkgsFor = system: nixpkgs.legacyPackages.${system};

      mkDevEnv = system:
        let pkgs = nixpkgsFor system;
        in pkgs.stdenv.mkDerivation {
          name = "Standard-Dev-Environment-with-Utils";
          buildInputs = (with pkgs; [ git gnumake nixfmt python310 ]);
        };

      haskell = rec {
        projectFor = system:
          let
            pkgs = nixpkgsFor system;
            stdDevEnv = mkDevEnv system;
            haskell-pkgs = pkgs.haskellPackages;

            project = pkgs.stdenv.mkDerivation {
              name = "Haskell-Dev-Environment-with-Utils";
              buildInputs = stdDevEnv.buildInputs
                ++ (with haskell-pkgs; [ BNFC alex happy ]);
            };
          in project;
      };

      java = rec {
        projectFor = system:
          let
            pkgs = nixpkgsFor system;
            stdDevEnv = mkDevEnv system;
            haskell-pkgs = pkgs.haskellPackages;

            project = pkgs.stdenv.mkDerivation {
              name = "Java-Dev-Environment-with-Utils";

              buildInputs = stdDevEnv.buildInputs
                ++ (with pkgs; [ antlr javaCup jflex jdk ])
                ++ (with haskell-pkgs; [ BNFC ]);

              shellHook = ''
                export CLASSPATH=$CLASSPATH:${pkgs.javaCup.outPath}/share/java-cup/java-cup-11b.jar:${pkgs.jflex.outPath}/lib/jflex-full-1.8.2.jar:$(pwd);
              '';
            };
          in project;
      };
    in {
      haskell = perSystem (system: (haskell.projectFor system));
      java = perSystem (system: (java.projectFor system));
      devShells = perSystem (system: {
        haskell = self.haskell.${system};
        java = self.java.${system};
      });
    };
}


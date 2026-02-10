{
  description = "Clonadic - A spreadsheet where Claude evaluates every formula";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    clonad = {
      url = "github:brittonr/clonad";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, clonad }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskellPackages.override {
          overrides = final: prev: {
            clonad = clonad.packages.${system}.clonad;
            clonadic = final.callCabal2nix "clonadic" ./. { };
          };
        };

        clonadic = haskellPackages.clonadic;
      in
      {
        packages = {
          default = clonadic;
          clonadic = clonadic;
        };

        apps.default = {
          type = "app";
          program = "${clonadic}/bin/clonadic";
        };

        devShells.default = haskellPackages.shellFor {
          packages = p: [ p.clonadic ];
          buildInputs = with haskellPackages; [
            cabal-install
            haskell-language-server
            hlint
            ormolu
            ghcid
          ] ++ [ pkgs.ollama ];
          withHoogle = true;

          OLLAMA_HOST = "http://localhost:11434";
          CLONAD_MODEL = "qwen3:4b";

          shellHook = ''
            # Start Ollama in background if not running
            if ! pgrep -x "ollama" > /dev/null; then
              echo "Starting Ollama server..."
              ollama serve &>/dev/null &
              sleep 2
            fi

            # Pull the model if not present
            if ! ollama list 2>/dev/null | grep -q "qwen3:4b"; then
              echo "Pulling qwen3:4b model (~2.5GB)..."
              ollama pull qwen3:4b
            fi

            echo "Ollama ready with qwen3:4b model"
            echo "OLLAMA_HOST=$OLLAMA_HOST"
            echo "CLONAD_MODEL=$CLONAD_MODEL"
          '';
        };

        formatter = pkgs.writeShellScriptBin "fmt" ''
          ${haskellPackages.ormolu}/bin/ormolu --mode inplace "$@"
        '';
      });
}

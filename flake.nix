{
  description = "Vectored Notes - Semantic search for Org-roam notes using vector embeddings";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        # Use Python 3.12 for better compatibility with current packages
        pythonEnv = pkgs.python312.withPackages (ps: with ps; [
          adjusttext
          langchain
          orgparse
          matplotlib-inline
          numpy
          pandas
          sentence-transformers
          scikit-learn
          ollama
          chromadb
          langchain-chroma
          langchain-ollama
          # Additional commonly needed packages
          pip
          ipython
        ]);

        # Create the main package
        org-vector = pkgs.stdenv.mkDerivation {
          pname = "org-vector";
          version = "0.3.0";

          src = ./.;

          buildInputs = [ pythonEnv ];

          installPhase = ''
            mkdir -p $out/bin
            mkdir -p $out/lib/org-vector
            mkdir -p $out/share/emacs/site-lisp

            # Install Python modules
            cp -r org_vector $out/lib/org-vector/
            cp main.py $out/lib/org-vector/

            # Create wrapper script
            cat > $out/bin/org-vector <<EOF
            #!${pkgs.bash}/bin/bash
            exec ${pythonEnv}/bin/python3 $out/lib/org-vector/main.py "\$@"
            EOF
            chmod +x $out/bin/org-vector

            # Install Emacs Lisp package
            cp org-vector.el $out/share/emacs/site-lisp/
          '';

          meta = with pkgs.lib; {
            description = "Semantic vector search for Org-roam note collections";
            homepage = "https://github.com/yourusername/org-vector";
            license = licenses.mit;
            maintainers = [ ];
            platforms = platforms.unix;
          };
        };

      in
      {
        # Package outputs
        packages = {
          default = org-vector;
          org-vector = org-vector;
        };

        # App for easy running
        apps = {
          default = {
            type = "app";
            program = "${org-vector}/bin/org-vector";
          };
        };

        # Development shell
        devShells.default = pkgs.mkShell {
          buildInputs = [
            pythonEnv
            pkgs.pyright
          ];

          shellHook = ''
            echo "Vectored Notes development environment"
            echo "Python version: $(python --version)"
            echo ""
            echo "Available commands:"
            echo "  python main.py embed -d <dir>    # Index org files"
            echo "  python main.py search -q <query> # Search indexed files"
            echo "  python test_context_fix.py       # Run tests"
            echo ""
            echo "Installed packages:"
            pip list | grep -E "adjustText|langchain|openai|orgparse|matplotlib-inline|numpy|pandas|sentence-transformers|scikit-learn|chromadb"
          '';
        };
      }
    );
}

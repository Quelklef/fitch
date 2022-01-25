let

inherit (import ./pins.nix) pkgs purs-nix elmish-latest ps-inline-asm npmlock2nix gitignoreSource;

nixed = purs-nix.purs
  { srcs = [ ];
    dependencies =
      with purs-nix.ps-pkgs;
      [ console
        effect
        lists
        maybe
        node-fs
        strings
        stringutils
        unicode
        elmish-latest
        string-parsers
        quickcheck
      ];
  };

in {

  deriv = pkgs.stdenv.mkDerivation {
    name = "fitch";
    src = ../app;

    buildInputs = [
      pkgs.nodePackages.uglify-js
      (nixed.command { srcs = [ "." ]; })
      ps-inline-asm
    ];

    installPhase = ''
      shopt -s extglob globstar
      mkdir -p $out
      ps-inline-asm ./**/*.purs
      purs-nix bundle
      uglifyjs ./index.js -o $out/index.js
      cp $src/{index.html,css.css,favicon.ico} $out
    '';
  };

  shell = pkgs.mkShell {
    buildInputs =
      [ (nixed.command { srcs = [ ''$(realpath "$PWD/app")'' ]; })
        ps-inline-asm
        pkgs.nodejs-17_x  # some newer APIs needed for tests
        pkgs.python3
        pkgs.entr
      ];

    shellHook = ''

      function workflow.build {(
        echo watching
        find app | entr -cs '
          set -eo pipefail
          shopt -s extglob globstar

          echo building

          mkdir -p .working
          cd .working

          # Repopulate all but compiler output
          rm -rf !(output)
          cp -r ../{app,app/{index.html,css.css,favicon.ico}} ./.

          # Compile
          ps-inline-asm ./**/*.purs
          purs-nix bundle
        '
      )}

      function workflow.serve {(
        mkdir -p .working &&
          cd .working &&
          python3 -m http.server
      )}

    '';
  };

}

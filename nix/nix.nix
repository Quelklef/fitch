let

inherit (import ./pins.nix) pkgs purs-nix elmish-latest npmlock2nix gitignoreSource;

nixed = purs-nix.purs
  { srcs = [ ../app ];
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
    src = ./../app;

    buildInputs = [ pkgs.nodejs pkgs.nodePackages.uglify-js ];

    installPhase = ''
      mkdir -p $out
      cp ${nixed.modules.Main.bundle {}} ./index.js
      uglifyjs ./index.js -o $out/index.js
      cp $src/{index.html,css.css,favicon.ico} $out
    '';
  };

  shell = pkgs.mkShell {
    buildInputs =
      [ (nixed.command { srcs = [ ''$(realpath "$PWD/app")'' ]; })
        pkgs.nodejs-17_x  # some newer APIs needed for tests
        pkgs.python3
        pkgs.entr
      ];

    shellHook = ''

      function workflow.build {(
        echo watching
        find app | entr -cs '
          set -eo pipefail
          echo building

          mkdir -p .working
          cd .working

          cp -r ../{app,app/{index.html,css.css,favicon.ico}} .

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

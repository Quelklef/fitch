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

node_modules = npmlock2nix.node_modules { src = gitignoreSource ./..; };

in {

  deriv = pkgs.stdenv.mkDerivation {
    name = "porygon";
    dontUnpack = true;

    buildInputs = [ pkgs.nodejs ];

    installPhase = ''
      mkdir -p $out

      cp ${nixed.modules.Main.bundle {}} $out/index.js

      if [ -d ${node_modules}/node_modules ]; then
        mkdir -p $out/node_modules/
        cp -r ${node_modules}/node_modules $out/
      fi

      echo "${pkgs.nodejs}/bin/node $out/index.js" > $out/run.sh
      chmod +x $out/run.sh
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

          cp -r ../{app,app/{index.html,css.css}} .

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

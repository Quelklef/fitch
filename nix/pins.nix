rec {

pkgs =
  let fetched = builtins.fetchGit {
        url = "https://github.com/NixOS/nixpkgs";
        rev = "7e89775a9e618fd494558b2e78f510e9e4ec6b27";
      };
  in import fetched {};

purs-nix =
  let fetched = builtins.fetchGit
       { url = "https://github.com/ursi/purs-nix.git";
         rev = "21bc6fc6dc3b595acf6f02cf09b873db877caa5d";
       };
  in import fetched { inherit pkgs; };

elmish-latest =
  let src = pkgs.fetchFromGitHub
              { owner = "ursi";
                repo   = "purescript-elmish";
                rev    = "87ee4b198f86bf57d215fd220899206f13b5f7a4";
                sha256 = "0yx3jvjsasm8hrczda18j8mngr7q7z77z9cfr0crnsj4jbkx5pap";
              };
  in purs-nix.ps-pkgs-ns.ursi.elmish.local src;

ps-inline-asm =
  let src = builtins.fetchGit
              { url = "https://github.com/quelklef/ps-inline-asm";
                rev = "6b5985a9d65439430abb67d0c83dc8555424a6a3";
                ref = "main";
              };
  in import src { inherit pkgs; };

npmlock2nix =
  let fetched = builtins.fetchGit {
        url = "https://github.com/tweag/npmlock2nix.git";
        rev = "228988a72aa375b7632ef658e93c9edcced169eb";
      };
  in import fetched { inherit pkgs; };

gitignoreSource =
  let fetched = builtins.fetchGit {
        url = "https://github.com/hercules-ci/gitignore.nix";
        rev = "80463148cd97eebacf80ba68cf0043598f0d7438";
      };
  in (import fetched { inherit (pkgs) lib; }).gitignoreSource;

}

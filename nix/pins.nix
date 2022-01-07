rec {

pkgs = import <nixpkgs> {};

purs-nix =
  let fetched = builtins.fetchGit
       { url = "https://github.com/ursi/purs-nix.git";
         rev = "21bc6fc6dc3b595acf6f02cf09b873db877caa5d";
       };
  in import fetched {};

elmish-latest =
  let src = pkgs.fetchFromGitHub
              { owner = "ursi";
                repo   = "purescript-elmish";
                rev    = "87ee4b198f86bf57d215fd220899206f13b5f7a4";
                sha256 = "0yx3jvjsasm8hrczda18j8mngr7q7z77z9cfr0crnsj4jbkx5pap";
              };
  in purs-nix.ps-pkgs-ns.ursi.elmish.local src;

npmlock2nix =
  let fetched = builtins.fetchGit {
        url = "https://github.com/tweag/npmlock2nix.git";
        rev = "8ada8945e05b215f3fffbd10111f266ea70bb502";
      };
  in import fetched { inherit pkgs; };

gitignoreSource =
  let fetched = builtins.fetchGit {
        url = "https://github.com/hercules-ci/gitignore.nix";
        rev = "80463148cd97eebacf80ba68cf0043598f0d7438";
      };
  in (import fetched { inherit (pkgs) lib; }).gitignoreSource;

}

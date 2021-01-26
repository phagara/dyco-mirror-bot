{ pkgs ? import <nixpkgs> {}, dyco-mirror-bot ? import ./default.nix {} }:

let
  dyco-mirror-bot-exe = dyco-mirror-bot.dyco-mirror-bot.components.exes.dyco-mirror-bot-exe;

  entrypoint = pkgs.writeScript "entrypoint.sh" ''
  #!${pkgs.stdenv.shell}
  $@
  '';
in
pkgs.dockerTools.buildImage {
  name = "dyco-mirror-bot";
  contents = [ pkgs.bashInteractive pkgs.coreutils dyco-mirror-bot-exe pkgs.iana-etc pkgs.cacert ];
  config = {
    Cmd = [ "${dyco-mirror-bot-exe}/bin/dyco-mirror-bot-exe" ];
    Entrypoint = [ entrypoint ];
  };
}

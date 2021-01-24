{ pkgs ? import <nixpkgs> {}, dyco-mirror-bot ? import ./default.nix {} }:

let
  dyco-mirror-bot-exe = dyco-mirror-bot.projectCross.raspberryPi.hsPkgs.dyco-mirror-bot.components.exes.dyco-mirror-bot-exe;

  entrypoint = pkgs.writeScript "entrypoint.sh" ''
  #!${pkgs.stdenv.shell}
  $@
  '';
in
pkgs.dockerTools.buildImage {
  name = "dyco-mirror-bot";
  contents = [ pkgs.bashInteractive pkgs.coreutils dyco-mirror-bot-exe pkgs.iana-etc pkgs.cacert ];
  # runAsRoot = ''
  #   #!${pkgs.stdenv.shell}
  #   ${pkgs.dockerTools.shadowSetup}
  #   rm -rf /nix/**/*gcc-{,debug-}9*
  # '';
  config = {
    Cmd = [ "${dyco-mirror-bot-exe}/bin/dyco-mirror-bot-exe" ];
    Entrypoint = [ entrypoint ];
  };
}

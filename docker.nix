{ pkgs ? import <nixpkgs> {}, dyco-mirror-bot ? import ./default.nix {} }:

let dyco-mirror-bot-exe = dyco-mirror-bot.projectCross.raspberryPi.hsPkgs.dyco-mirror-bot.components.exes.dyco-mirror-bot-exe;
in
pkgs.dockerTools.buildImage {
  name = "dyco-mirror-bot";
  contents = [ dyco-mirror-bot-exe pkgs.iana-etc pkgs.cacert ];
  config = {
    Cmd = [ "${dyco-mirror-bot-exe}/bin/dyco-mirror-bot-exe" ];
  };
}

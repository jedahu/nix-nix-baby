{ config, pkgs, lib, ... }:

{

  imports = [
    (import ./env.nix)
    (import ./programs)
    (import ./modules/project-repos.nix)
  ];

  home.stateVersion = "22.05";

  # https://github.com/malob/nixpkgs/blob/master/home/default.nix

  home.file.".gnupg/gpg-agent.conf".text =
    let ttl = toString (60 * 60 * 8);
    in ''
      enable-ssh-support
      default-cache-ttl ${ttl}
      default-cache-ttl-ssh ${ttl}
      max-cache-ttl ${ttl}
      max-cache-ttl-ssh ${ttl}
      allow-emacs-pinentry
      allow-loopback-pinentry
      pinentry-program ${pkgs.pinentry_mac}/Applications/pinentry-mac.app/Contents/MacOS/pinentry-mac
    '';

  home.file.".gnupg/sshcontrol".text = ''
    54FD55B120409C928D151E22525E6CEBB870E4E3
    614757A27BDE1D737DC19A21A135DB34A27F4E3F
    52603C703FDBBC5C38D02065873579949DD92E29
  '';

  xdg.configFile."ranger/rc.conf".text = ''
    set preview_script ${pkgs.pistol}/bin/pistol
    set use_preview_script true
    set show_hidden true
  '';

  home.packages = import ./packages.nix { inherit pkgs lib; };

  home.activation = {
    rectangleRemoteControl = lib.hm.dag.entryAfter ["writeBoundary"] ''
      $DRY_RUN_CMD defaults write com.knollsoft.Rectangle remoteControlPath ~/.run/rectangleControl
    '';
  };

  project-repos = {
    repos = {
      "gc/core" = {
        uri = "git@github.com:goodcover/core.git";
        untracked.".envrc".text = ''
          use_nix --argstr jdk jdk8
          export MYSQL_UNIX_PORT=/tmp/mysql.sock
          export DANGEROUS_DEV_PARTITIONS=true
        '';
      };
      "gc/secure" = {
        uri = "keybase://team/goodcover.devs/secure";
      };
      "gc/marketing" = {
        uri = "git@github.com:goodcover/marketing.git";
      };
    };
  };

}

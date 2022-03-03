{ config, lib, pkgs, ... }:

lib.mkMerge [{
  home.sessionVariables = {
    ALTERNATE_EDITOR = "";
    EDITOR = "vim";
    VISUAL = "vim";
    PAGER = "${pkgs.bat}/bin/bat -p";
    BAT_PAGER = "${pkgs.less}/bin/less -SR";
    BASH_ENV = "";
    XDG_DATA_DIRS = "$HOME/.nix-profile/share";
    SSL_CERT_FILE = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
    GIT_SSL_CAINFO = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";

    # LESS_TERMCAP_mb="$'\e[1;32m'";
    # LESS_TERMCAP_md="$'\e[1;32m'";
    # LESS_TERMCAP_me="$'\e[0m'";
    # LESS_TERMCAP_se="$'\e[0m'";
    # LESS_TERMCAP_so="$'\e[01;33m'";
    # LESS_TERMCAP_ue="$'\e[0m'";
    # LESS_TERMCAP_us="$'\e[1;4;31m'";

    GPG_TTY = "$(tty)";
    SSH_AUTH_SOCK = "$(gpgconf --list-dirs agent-ssh-socket)";
  };
}]

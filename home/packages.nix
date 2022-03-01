{ pkgs, lib }:

with pkgs; [
  # (agda.withPackages (p: [ p.standard-library ]))
  # Dev stuff
  # bandwhich
  # elinks
  # emacsMacport
  # eternal-terminal
  # goku
  # google-cloud-sdk
  # haskellPackages.cabal-install
  # haskellPackages.hoogle
  # haskellPackages.hpack
  # haskellPackages.implicit-hie
  # haskellPackages.stack
  # hx
  # hyperkit
  # idris2
  # iosevka
  # kitty
  # licensor
  # local.borgmatic
  # master.alot
  # master.astroid
  # mu
  # ncspot # spotify client
  # neovim
  # nix-bash-completions
  # nodePackages.node2nix
  # nodePackages.typescript
  # nodejs
  # pandoc
  # procs
  # purescript
  # purescript
  # rbw # bitwarden cli (unofficial)
  # xhyve
  # yabai
  # ytop
  (pass.withExtensions (ext: with ext; [pass-import]))
  abduco
  ag
  aria2
  asciidoctor
  asciinema
  autojump
  bash
  bashCompletion
  bashInteractive
  bingrep
  bitwarden-cli
  borgbackup
  broot
  cacert
  cachix # adding/managing alternative binary caches hosted by Cachix
  caddy
  cmake
  cocoapods
  comma # run software from without installing it
  coreutils
  coreutils
  coursier
  cowsay
  curl
  curl
  direnv
  dos2unix
  dust
  dvtm
  elvish
  exa
  exa
  fd
  figlet
  file
  flock
  fontconfig
  fzf
  gettext # for envsubst
  gitAndTools.hub
  gitAndTools.hub
  gitAndTools.tig
  gitAndTools.tig
  gitFull
  glances
  glances
  gnugrep
  gnupg
  gnused
  gnutar
  go2nix
  graphviz
  htop
  hyperfine
  inkscape
  joker
  jq
  jq
  k9s
  keybase
  kbfs
  kubectl
  less
  lf
  libtool
  libvterm-neovim
  links2
  # local.metals-emacs
  # local.miniserve
  lsd
  lynx
  metals
  moreutils
  mosh
  mplus-outline-fonts
  mpv
  nethack
  niv
  niv # easy dependency management for nix projects
  nix-prefetch-git
  pinentry_mac
  pistol
  ps
  pstree
  qrencode
  ranger
  ripgrep
  rlwrap
  rsync
  shellcheck
  skhd
  spotify-tui
  terminal-notifier
  tmux
  travis
  tree
  unnethack
  unzip
  vim
  watch
  wget
  wget
  which
  xorg.lndir
  yaml2json
  youtube-dl
  zip
]
++ lib.optionals stdenv.isDarwin [
  cocoapods
  m-cli # useful macOS CLI commands
]

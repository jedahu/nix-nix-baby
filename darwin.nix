{ pkgs, lib, ... }:
{
  # Nix configuration ------------------------------------------------------------------------------

  nix.binaryCaches = [
    "https://cache.nixos.org/"
  ];
  nix.binaryCachePublicKeys = [
    "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
  ];
  nix.trustedUsers = [
    "@admin"
  ];
  users.nix.configureBuildUsers = true;

  # Enable experimental nix command and flakes
  nix.package = pkgs.nixUnstable;
  nix.extraOptions = ''
    auto-optimise-store = true
    experimental-features = nix-command flakes
  '' + lib.optionalString (pkgs.system == "aarch64-darwin") ''
    extra-platforms = x86_64-darwin aarch64-darwin
  '';

  # Create /etc/bashrc that loads the nix-darwin environment.
  programs.zsh.enable = true;
  programs.bash.enable = true;

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;

  environment.darwinConfig = "$HOME/.config/nix/darwin.nix";

  environment.shells = [ pkgs.bashInteractive pkgs.zsh ];

  # Apps
  # `home-manager` currently has issues adding them to `~/Applications`
  # Issue: https://github.com/nix-community/home-manager/issues/1341
  environment.systemPackages = with pkgs; [
    # kitty
    emacs
    skhd
    terminal-notifier
    vim
  ];

  environment.systemPath = [
    "/opt/homebrew/bin"
  ];

  # https://github.com/nix-community/home-manager/issues/423
  environment.variables = {
    # TERMINFO_DIRS = "${pkgs.kitty.terminfo.outPath}/share/terminfo";
  };
  programs.nix-index.enable = true;

  services.skhd = {
    enable = false;
    skhdConfig =
      let
        skhd = "${pkgs.skhd}/bin/skhd";
        esc = "${skhd} -k escape";
        rectangle = action: ''echo -e "${action}\n" >~/.run/rectangleControl'';
      in ''
        :: switchTo
        :: moveTo

        hyper - o ; switchTo
        hyper - w ; moveTo

        switchTo, moveTo < escape ; default

        switchTo < e : open -a Emacs;  ${esc}
        switchTo < l : open -a Linear; ${esc}
        switchTo < n : open -a Notion; ${esc}
        switchTo < w : open -a Safari; ${esc}

        moveTo < d : ${rectangle "leftHalf"}
        moveTo < f : ${rectangle "rightHalf"}

        moveTo < e : ${rectangle "topLeft"}
        moveTo < r : ${rectangle "topRight"}
        moveTo < c : ${rectangle "bottomLeft"}
        moveTo < v : ${rectangle "bottomRight"}

        moveTo < shift - j : ${rectangle "firstTwoThirds"}
        moveTo < j : ${rectangle "firstThird"}
        moveTo < k : ${rectangle "centerThird"}
        moveTo < l : ${rectangle "lastThird"}
        moveTo < shift - l : ${rectangle "lastTwoThirds"}

        moveTo < u : ${rectangle "topLeftSixth"}
        moveTo < i : ${rectangle "topCenterSixth"}
        moveTo < o : ${rectangle "topRightSixth"}
        moveTo < m : ${rectangle "bottomLeftSixth"}
        moveTo < 0x2B : ${rectangle "bottomCenterSixth"}
        moveTo < 0x2F : ${rectangle "bottomRightSixth"}

        moveTo < shift - 0x19 : ${rectangle "previousDisplay"}
        moveTo < shift - 0x1D : ${rectangle "nextDisplay"}
      '';
  };

  # Fonts
  fonts.enableFontDir = true;
  fonts.fonts = with pkgs; [
     recursive
     (nerdfonts.override { fonts = [ "JetBrainsMono" ]; })
   ];

  homebrew = {
    enable = true;
    brews = [
      # "mysql@5.7"
      "cassandra"
      "choose-gui"
    ];
    casks = [
      "1password"
      "alfred"
      "figma"
      "Hummingbird"
      "iterm2"
      "karabiner-elements"
      "keybase"
      "launchcontrol"
      "linear-linear"
      "notion"
      "paletro"
    ];
    taps = [
      "finestructure/Hummingbird"
    ];
    masApps = {
      # 1Password7 = 1333542190;
      # Alfred = 405843582;
      AdGuardForSafari = 1440147259;
      KeyCodes = 414568915;
      HushNagBlocker = 1544743900;
      Infuse7 = 1136220934;
      LgScreenManager = 1142051783;
      Messenger = 1480068668;
      RefinedGithub = 1519867270;
      SaveToPocket = 1477385213;
      SlackForDesktop = 803453959;
      XCode = 497799835;
    };
    cleanup = "zap";
    extraConfig = ''
      brew 'mysql@5.7', restart_service: true, link: true, conflicts_with: ['mysql']
      brew 'cassandra', restart_service: true
    '';
  };

  # Keyboard
  system.keyboard.enableKeyMapping = true;
  system.keyboard.remapCapsLockToEscape = true;

  system.activationScripts.postUserActivation.text =
    let
      opt-cassandra = "/opt/homebrew/opt/cassandra";
      plist = "${opt-cassandra}/homebrew.mxcl.cassandra.plist";
      jna = pkgs.fetchurl {
        url = "https://search.maven.org/remotecontent?filepath=net/java/dev/jna/jna/5.9.0/jna-5.9.0.jar";
        sha256 = "0qbis8acv04fi902qzak1mbagqaxcsv2zyp7b8y4shs5nj0cgz7a";
      };
    in ''
      sed -i -e s'/-Xss256k/-Xss512k/g' /opt/homebrew/etc/cassandra/cassandra-env.sh

      ln -sf ${jna} ${opt-cassandra}/libexec/jna-5.6.0.jar

      defaults write ${plist} EnvironmentVariables \
        -dict-add \
          MAX_HEAP_SIZE 2G \
          HEAP_NEWSIZE 400M
      plutil -convert xml1 ${plist}
    '';

  # Add ability to used TouchID for sudo authentication
  security.pam.enableSudoTouchIdAuth = true;

  users.users.jal = {
    uid = 501;
    gid = 20;
    home = "/Users/jal";
    shell = pkgs.bashInteractive;
  };

  # launchd.user.agents.input-sink = {
  #   serviceConfig = {
  #     Program = "${pkgs.input-sink}/bin/InputSink";
  #     StandardErrorPath = "/tmp/InputSink.err.log";
  #     StandardOutPath = "/tmp/InputSink.out.log";
  #     KeepAlive = true;
  #   };
  # };

  system.defaults = {
    NSGlobalDomain = {
      AppleInterfaceStyleSwitchesAutomatically = true;
      AppleShowAllExtensions = true;
      AppleShowScrollBars = "WhenScrolling"; # "Automatic" "Always"
      NSAutomaticCapitalizationEnabled = false;
      NSAutomaticSpellingCorrectionEnabled = true;
      NSDocumentSaveNewDocumentsToCloud = true;
      NSScrollAnimationEnabled = true;
      _HIHideMenuBar = false;
      "com.apple.keyboard.fnState" = false; # Use F1 etc as function keys.
      "com.apple.mouse.tapBehavior" = null; # 1 -> tap to click
    };
    SoftwareUpdate.AutomaticallyInstallMacOSUpdates = true;
    dock = {
      autohide = true;
      mru-spaces = false;
      show-process-indicators = true;
      show-recents = true;
      showhidden = false;
      static-only = false;
    };
    finder = {
      AppleShowAllExtensions = true;
      CreateDesktop = true;
      FXEnableExtensionChangeWarning = false;
      _FXShowPosixPathInTitle = false;
    };
    screencapture.location = "~/Downloads";
    spaces.spans-displays = false;
    trackpad = {
      Clicking = false; # tap to click
      Dragging = false; # tap to drag
      TrackpadRightClick = false;
      TrackpadThreeFingerDrag = false;
    };
  };
}

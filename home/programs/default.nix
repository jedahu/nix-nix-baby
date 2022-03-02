{ options, config, lib, spacemacs, pkgs, ... }:

let
  emacs = {
    home.file = {
      ".emacs.d" = {
        recursive = true;
        source = spacemacs;
      };

      ".spacemacs".source = ./emacs/dotspacemacs.el;

    };

    xdg.configFile.spacemacs = {
      recursive = true;
      source = ./emacs/private;
    };

    home.activation.emacsNixEnv = lib.hm.dag.entryAfter ["writeBoundary"] ''
      cat <<EOF >~/.emacs.d/nix-env-vars
      ${config.lib.shell.exportAll config.home.sessionVariables}
      EOF
    '';
  };

  karabiner = {
    home.activation.karabinerConfig = lib.hm.dag.entryAfter ["writeBoundary"] ''
      $DRY_RUN_CMD mkdir -p $HOME/.config/karabiner
      $DRY_RUN_CMD cp ${./karabiner/karabiner.json} $HOME/.config/karabiner/karabiner.json
    '';
  };

  simple = {
    programs = {
      autojump.enable = true;

      bash = {
        enable = true;
        historyControl = ["ignorespace" "ignoredups"];
        historyIgnore = ["exit" "ls" "cd"];
        shellOptions = [
          "histappend"
          "checkwinsize"
          "extglob"
          "globstar"
          "checkjobs"
        ];
        profileExtra = "";
        initExtra = ''
          . ${pkgs.bash-completion}/share/bash-completion/bash_completion
        '';
        shellAliases = with pkgs; {
          cat = "${bat}/bin/bat --paging never";
          du = "${du-dust}/bin/dust";
          grep = "${ripgrep}/bin/rg";
          less = "${bat}/bin/bat -p";
          ls = "${exa}/bin/exa";
          objdump = "${bingrep}/bin/bingrep";
          time = "${hyperfine}/bin/hyperfine";
          vi = "/usr/bin/vi --clean";
          drs = "darwin-rebuild switch --flake $HOME/.config/nix";
          # hexdump = "${hx}/bin/hx";
          # find = "${fd}/bin/fd";
          # iftop = "${badwhich}/bin/bandwhich";
          # license = "${licensor}/bin/licensor";
          # ps = "${procs}/bin/procs";
          # top = "${ytop}/bin/ytop";
          # sbt = "sbt -java-home $JAVA_HOME";
        };
      };

      bat = {
        enable = true;
        config = {
          style = "plain";
          theme = "ansi";
        };
      };

      # Direnv, load and unload environment variables depending on the current directory.
      # https://direnv.net
      # https://rycee.gitlab.io/home-manager/options.html#opt-programs.direnv.enable
      direnv = {
        enable = true;
        enableBashIntegration = true;
        nix-direnv.enable = true;
      };

      fzf = {
        enable = true;
        enableBashIntegration = true;
        # defaultOptions = ["--bind" "'enter:execute(${pkgs.bat}/bin/bat --paging always {})'"];
      };

      # emacs.enable = true;

      git = {
        enable = true;
        package = pkgs.gitAndTools.gitFull;
        aliases = {};
        attributes = [];
        delta.enable = true;
        ignores = [
          ".projectile"
          ".dir-locals.el"
          ".envrc"
          ".direnv"
        ];
        signing = {
          key = null;
          gpgPath = "${pkgs.gnupg}/bin/gpg2";
          signByDefault = true;
        };
        userName = "Jeremy Hughes";
        userEmail = "jedahu@gmail.com";
        extraConfig = {
          init = {
            defaultBranch = "master";
          };
        };
      };

      gpg = {
        enable = true;
        settings = {
          keyid-format = "LONG";
        };
      };

      # Htop
      # https://rycee.gitlab.io/home-manager/options.html#opt-programs.htop.enable
      htop = {
        enable = true;
        settings.show_program_path = true;
      };

      readline = {
        enable = true;
        extraConfig = ''
          set editing-mode vi
        '';
      };

      ssh = {
        enable = true;
        extraOptionOverrides = {
          Match = "host * exec \"gpg-connect-agent UPDATESTARTUPTTY /bye\"";
        };
      };

      starship = {
        enable = true;
        enableBashIntegration = true;
        package = pkgs.starship;
        settings = {
          battery = {
            disabled = true;
          };
        };
      };


    };
  };

in
  lib.mkMerge [
    emacs
    karabiner
    simple
  ]

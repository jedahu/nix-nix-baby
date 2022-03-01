{ options, config, lib, pkgs, modulesPath, ... }:

with lib;

let

  cfg = config.project-repos;

  fileType = (import (modulesPath + "/lib/file-type.nix") {
    inherit (config.home) homeDirectory;
    inherit lib pkgs;
  }).fileType;

  repoType = types.submodule ({ name, config, ... }: {
    options = {
      path = mkOption {
        internal = true;
        default = name;
        type = types.str;
        description = "The local repository path";
      };

      uri = mkOption {
        type = types.str;
        description = "Git project remote URI";
      };

      remote-branch = mkOption {
        type = types.str;
        default = "origin/master";
        description = "Branch to checkout";
      };

      track-remote = mkOption {
        type = types.bool;
        default = true;
        description = "Track checked out branch. i.e. `git checkout -t <remote-branch>`";
      };

      untracked = mkOption {
        default = {};
        type = fileType "<varname>project-repos.repos.*</varname>" config.path;
        description = ".envrc content";
      };
    };
  });

in {
  options.project-repos = {
    git-package = mkOption {
      type = types.package;
      default = config.programs.git.package;
      defaultText = options.programs.git.package.defaultText;
      description = "Git package to use.";
    };

    repos = mkOption {
      type = types.attrsOf repoType;
      default = {};
      description = "Attribute set of repositories";
    };
  };

  config = mkMerge [
    {
      home.file = mkMerge [
        (foldl' (x: y: x // y) {}
        (map (repo:
          (mapAttrs' (name: file:
            nameValuePair "${repo.path}/${name}" file)
            repo.untracked))
          (attrValues cfg.repos)))
      ];

      home.activation.cloneProjectRepos =
        let
          git = "${cfg.git-package}/bin/git";
          doClone = repo:
            let
              p = "${config.home.homeDirectory}/${repo.path}";
              t = if repo.track-remote then "-t" else "";
            in ''
              $DRY_RUN_CMD mkdir -p ${p}
              $DRY_RUN_CMD cd ${p}
              if [[ ! -d ${p}/.git ]]; then
                $DRY_RUN_CMD ${git} init
                $DRY_RUN_CMD ${git} remote add origin ${repo.uri}
                $DRY_RUN_CMD ${git} fetch
                $DRY_RUN_CMD ${git} checkout ${t} ${repo.remote-branch}
              fi
            '';
         in
           hm.dag.entryAfter ["writeBoundary"] (
             (concatMapStrings doClone (attrValues cfg.repos))
           );
    }
  ];
}

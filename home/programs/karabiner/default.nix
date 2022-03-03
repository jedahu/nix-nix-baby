{ lib, pkgs, ... }:

let
  inherit (builtins) elemAt head tail toJSON;

  hyper-keys = ["left_shift" "left_command" "left_control" "left_option"];
  hyper = key: [key] ++ hyper-keys;

  fn = key: [key "fn"];

  from = key: mods: {
    key_code = key;
    modifiers = mods;
  };

  basic = descr: mappings: {
    description = descr;
    manipulators = map (mapping:
      let
        from = elemAt mapping 0;
        to = elemAt mapping 1;
      in {
        from = {
          key_code = head from;
          modifiers = { mandatory = tail from; };
        };
        to = [
          ({ key_code = head to; }
           // (if (tail to == [])
               then {}
               else { modifiers = tail to; }))
        ];
        type = "basic";
      }) mappings;
  };

  rules = [
    (basic "Basics" [
      [["fn"] hyper-keys]
      [(hyper "h") ["left_arrow"]]
      [(hyper "j") ["down_arrow"]]
      [(hyper "k") ["up_arrow"]]
      [(hyper "l") ["right_arrow"]]
      [(fn "h") ["left_arrow"]]
      [(fn "j") ["down_arrow"]]
      [(fn "k") ["up_arrow"]]
      [(fn "l") ["right_arrow"]]
    ])
  ];

  conf-in = lib.importJSON ./karabiner.json;

  profile-in = head conf-in.profiles;

  profile-out = profile-in // {
    complex_modifications = profile-in.complex_modifications // {
      inherit rules;
    };
  };

  conf-path = pkgs.writeText "karabiner.json" (toJSON (conf-in // {profiles = [profile-out];}));

in lib.mkMerge[{
  home.activation.karabinerConfig = lib.hm.dag.entryAfter ["writeBoundary"] ''
    $DRY_RUN_CMD mkdir -p $HOME/.config/karabiner
    $DRY_RUN_CMD cp ${conf-path} $HOME/.config/karabiner/karabiner.json
  '';
}]

with import <nixpkgs> {};

# with import "${src.out}/rust-overlay.nix" pkgs pkgs;

let
idris-sdl2-src = fetchFromGitHub {
      owner = "xiphiness";
      repo = "idris-sdl2";
      rev = "da98776a73028d90820667b2e24cd1184c0f7d71";
      sha256 = "0l50ba3xk7yc6z365g2pxx8xdbvd1jkwnfg081f23wrl1r7qim7s";
   };
gl-idris-src = fetchFromGitHub {
         owner = "xiphiness";
         repo = "gl-idris";
         rev = "ce2626f04e9d5978575c6aea47b30599e84cb9c3";
         sha256 = "13nhza9lmqrbc1pgsaqgn96jwm5ifclvwlk0r8pfrsnshy481n1b";
      };
in
{ build-idris-package,
 effects, sdl2, glfw
}:
build-idris-package  {
  name = "graph-toy";
  version = "2018-01-25";
  # This is the .ipkg file that should be built, defaults to the package name
  # In this case it should build `Yaml.ipkg` instead of `yaml.ipkg`
  # This is only necessary because the yaml packages ipkg file is
  # different from its package name here.
  # Idris dependencies to provide for the build

  ipkgName = "graph-toy";
  idrisDeps = [ effects (import "${idris-sdl2-src.out}/default.nix").idris-sdl2 (import "${gl-idris-src.out}/default.nix").idris-gl glfw ];
  extraBuildInputs = [ pkg-config SDL2 SDL2_gfx glew libpng glfw ];
  src = ./.;

  meta = {
    description = "Idris GL lib";
  };
}

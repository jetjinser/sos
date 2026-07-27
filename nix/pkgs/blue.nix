{
  lib,
  stdenv,
  fetchFromCodeberg,
  guile,
  texinfo,
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "blue";
  version = "0-unstable-2026-03-05";

  src = fetchFromCodeberg {
    owner = "lapislazuli";
    repo = "blue";
    rev = "d14b504f569a7783e89478d1478bdceece5fa579";
    hash = "sha256-7H6fBSrPPtQp4SyOEBwo4lQn2dtu3f+QclARvetYn74=";
  };

  strictDeps = true;

  buildInputs = [ guile ];
  nativeBuildInputs = [
    guile
    texinfo
  ];

  buildPhase = ''
    runHook preBuild

    mkdir build

    pushd build
    ../bootstrap
    ./pre-inst-env blue configure --prefix=$out
    ./pre-inst-env blue build
    popd

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    pushd build
    ./pre-inst-env blue install
    popd

    runHook postInstall
  '';

  meta = {
    description = "A generic build-system crafted entirely in Guile";
    homepage = "https://codeberg.org/lapislazuli/blue";
    license = lib.licenses.lgpl3Plus;
    maintainers = with lib.maintainers; [ jinser ];
    mainProgram = "blue";
    platforms = lib.platforms.all;
  };
})

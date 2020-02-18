let
  pkgs = import <nixpkgs> {};
  source = pkgs.lib.sourceByRegex ./. [
    "^hasky-stack\.el$"
  ];
in
pkgs.stdenv.mkDerivation {
  name = "hasky-stack";
  src = source;
  buildInputs = [
    (pkgs.emacs26WithPackages (epkgs: [epkgs.f epkgs.magit-popup]))
  ];
  buildPhase = ''
    emacs -L . --batch -f batch-byte-compile *.el
  '';
  installPhase = ''
    LISPDIR=$out/share/emacs/site-lisp
    install -d $LISPDIR
    install *.el *.elc $LISPDIR
  '';
  # checkPhase = ''
  #   emacs -L . --batch --eval "(progn (require 'ert-runner) (ert-run-tests t (lambda (x) nil)))"
  # '';
  # doCheck = true;
}

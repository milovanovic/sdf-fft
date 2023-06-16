set -e
[ "$VERILATOR_CHECKOUT_TARGET" ] || VERILATOR_CHECKOUT_TARGET=v3.926
[ "$VERILATOR_REPO_URL" ] || VERILATOR_REPO_URL='https://github.com/verilator/verilator.git'
# if [ ! "$VERILATOR_CHECKOUT_TARGET" ]; then
#   echo "error: VERILATOR_CHECKOUT_TARGET not defined" >&2
#   exit 1
# fi
# Install Verilator (http://www.veripool.org/projects/verilator/wiki/Installing)
if [ ! -f $INSTALL_DIR/bin/verilator ]; then
  mkdir -p $INSTALL_DIR
  git clone "$VERILATOR_REPO_URL"
  unset VERILATOR_ROOT
  cd verilator
  git pull
  git checkout "$VERILATOR_CHECKOUT_TARGET"
  autoconf
  ./configure --prefix=$INSTALL_DIR
  make
  make install
  export VERILATOR_ROOT=$INSTALL_DIR
  # Fix verilator for local install (http://www.lowrisc.org/docs/untether-v0.2/verilator/)
  ln -s $VERILATOR_ROOT/share/verilator/include $VERILATOR_ROOT/include
  ln -s $VERILATOR_ROOT/share/verilator/bin/verilator_includer $VERILATOR_ROOT/bin/verilator_includer
fi

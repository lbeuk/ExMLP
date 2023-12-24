# Install Haskell toolchain
cd `mkdtemp -d`
export BOOTSTRAP_HASKELL_NONINTERACTIVE=\"t\"
export BOOTSTRAP_HASKELL_ADJUST_BASHRC=\"t\"
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
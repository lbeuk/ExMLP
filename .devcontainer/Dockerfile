FROM mcr.microsoft.com/devcontainers/base:jammy
RUN apt update && apt install -y build-essential curl libffi-dev libffi8ubuntu1 libgmp-dev libgmp10 libncurses-dev
RUN sudo -u vscode sh -c "export BOOTSTRAP_HASKELL_NONINTERACTIVE=\"t\"; export BOOTSTRAP_HASKELL_ADJUST_BASHRC=\"t\"; curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh"
# RUN sudo -u vscode sh -c "ghcup install hls"
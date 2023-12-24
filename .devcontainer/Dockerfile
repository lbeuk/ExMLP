FROM mcr.microsoft.com/devcontainers/base:jammy
RUN apt update && apt install -y build-essential curl libffi-dev libffi8ubuntu1 libgmp-dev libgmp10 libncurses-dev
COPY ./setup_root.sh /opt/setup_root.sh
COPY ./setup_user.sh /opt/setup_user.sh
RUN sh /opt/setup_root.sh
USER vscode
RUN sh /opt/setup_user.sh
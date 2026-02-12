#!/usr/bin/env bash
set -euo pipefail

# install.sh - Install bechmlup from its latest GitHub release
# Usage: curl -sSf https://raw.githubusercontent.com/bechml/bechmlup/main/install.sh | bash

REPO="bechml/bechmlup"
INSTALL_DIR="$HOME/.bechmlup/bin"
API_URL="https://api.github.com/repos/${REPO}/releases/latest"

info()  { printf "\033[1;34m==> %s\033[0m\n" "$*"; }
warn()  { printf "\033[1;33mwarning: %s\033[0m\n" "$*"; }
error() { printf "\033[1;31merror: %s\033[0m\n" "$*" >&2; exit 1; }

detect_platform() {
  local uname_s uname_m
  uname_s="$(uname -s)"
  uname_m="$(uname -m)"

  case "$uname_s" in
    Linux*)  OS="linux" ;;
    Darwin*) OS="macos" ;;
    MINGW*|MSYS*|CYGWIN*) OS="windows" ;;
    *) error "Unsupported OS: $uname_s" ;;
  esac

  case "$uname_m" in
    x86_64|amd64) ARCH="x86_64" ;;
    aarch64|arm64) ARCH="aarch64" ;;
    *) error "Unsupported architecture: $uname_m" ;;
  esac

  PLATFORM="${OS}-${ARCH}"
  info "Detected platform: $PLATFORM"
}

fetch_latest_release() {
  info "Fetching latest bechmlup release..."

  if command -v curl &>/dev/null; then
    RELEASE_JSON="$(curl -sSf -H "User-Agent: bechmlup-installer" "$API_URL" 2>/dev/null)" || true
  elif command -v wget &>/dev/null; then
    RELEASE_JSON="$(wget -qO- --header="User-Agent: bechmlup-installer" "$API_URL" 2>/dev/null)" || true
  else
    error "Neither curl nor wget found. Please install one of them."
  fi

  if [ -z "${RELEASE_JSON:-}" ] || [ "$RELEASE_JSON" = "null" ]; then
    warn "No releases found yet for bechmlup."
    warn "Releases have not been published yet."
    warn "Building from source instead..."
    install_from_source
    return 1
  fi

  TAG="$(echo "$RELEASE_JSON" | grep '"tag_name"' | head -1 | sed 's/.*: *"\(.*\)".*/\1/')"
  info "Latest release: $TAG"
  return 0
}

find_asset_url() {
  # Try to find a matching asset for our platform
  ASSET_URL="$(echo "$RELEASE_JSON" \
    | grep '"browser_download_url"' \
    | grep -i "$OS" \
    | head -1 \
    | sed 's/.*: *"\(.*\)".*/\1/')"

  if [ -z "$ASSET_URL" ]; then
    warn "No prebuilt binary found for $PLATFORM."
    warn "Building from source..."
    install_from_source
    return 1
  fi

  info "Downloading: $ASSET_URL"
  return 0
}

download_binary() {
  mkdir -p "$INSTALL_DIR"
  local dest="$INSTALL_DIR/bechmlup"
  if [ "$OS" = "windows" ]; then
    dest="$INSTALL_DIR/bechmlup.exe"
  fi

  if command -v curl &>/dev/null; then
    curl -sSfL -o "$dest" "$ASSET_URL"
  else
    wget -qO "$dest" "$ASSET_URL"
  fi

  chmod +x "$dest"
  info "Installed bechmlup to $dest"
}

install_from_source() {
  info "Installing bechmlup from source (requires GHC and cabal)..."

  if ! command -v cabal &>/dev/null; then
    error "cabal not found. Please install GHC and cabal-install first (e.g. via ghcup)."
  fi

  if ! command -v ghc &>/dev/null; then
    error "ghc not found. Please install GHC first (e.g. via ghcup)."
  fi

  local tmpdir
  tmpdir="$(mktemp -d)"
  trap 'rm -rf "$tmpdir"' EXIT

  info "Cloning bechmlup..."
  git clone --depth 1 "https://github.com/${REPO}.git" "$tmpdir/bechmlup"

  info "Building bechmlup..."
  cd "$tmpdir/bechmlup"
  cabal build exe:bechmlup

  info "Installing bechmlup..."
  mkdir -p "$INSTALL_DIR"
  cabal install exe:bechmlup --install-method=copy --overwrite-policy=always --installdir="$INSTALL_DIR"

  info "Installed bechmlup to $INSTALL_DIR/bechmlup"
}

setup_path() {
  if echo "$PATH" | tr ':' '\n' | grep -qx "$INSTALL_DIR"; then
    return
  fi

  info "Adding $INSTALL_DIR to PATH..."

  local shell_profile=""
  case "${SHELL:-/bin/bash}" in
    */zsh)  shell_profile="$HOME/.zshrc" ;;
    */bash)
      if [ -f "$HOME/.bash_profile" ]; then
        shell_profile="$HOME/.bash_profile"
      else
        shell_profile="$HOME/.bashrc"
      fi
      ;;
    */fish) shell_profile="$HOME/.config/fish/config.fish" ;;
    *)      shell_profile="$HOME/.profile" ;;
  esac

  local path_line="export PATH=\"$INSTALL_DIR:\$PATH\""
  if [ -n "$shell_profile" ] && [ -f "$shell_profile" ]; then
    if ! grep -qF "$INSTALL_DIR" "$shell_profile"; then
      echo "" >> "$shell_profile"
      echo "# Added by bechmlup installer" >> "$shell_profile"
      echo "$path_line" >> "$shell_profile"
      info "Added PATH entry to $shell_profile"
    fi
  else
    warn "Could not detect shell profile. Add this to your shell config:"
    warn "  $path_line"
  fi
}

main() {
  info "bechmlup installer"
  echo ""

  detect_platform

  if fetch_latest_release && find_asset_url; then
    download_binary
  fi

  setup_path

  echo ""
  info "Installation complete!"
  info "Restart your shell or run:"
  info "  export PATH=\"$INSTALL_DIR:\$PATH\""
  echo ""
  info "Then run: bechmlup install"
}

main "$@"

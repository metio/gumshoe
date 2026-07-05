# SPDX-FileCopyrightText: The gumshoe Authors
# SPDX-License-Identifier: 0BSD

{
  description = "Everything needed to run the gumshoe's books";

  # Pinned to an immutable nixpkgs revision, not a moving branch like
  # nixos-unstable, so every build - dev shell and container image alike -
  # resolves the exact same tool versions. flake.lock records the verified
  # narHash of this tree; regenerate both together with `nix flake update`.
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/65179426c83bb3f6bc14898b42ea1c6f01d374b0";

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});

      # The single list of tools every book needs. The dev shell and the
      # container image (dev/Containerfile builds packages.default) both draw
      # from this, so the shell you develop in and the image you run in can
      # never drift apart.
      toolsFor = pkgs: with pkgs; [
        babashka # every book runs on babashka
        git # changelog announcement metadata

        kubectl # all cluster access
        krew # installs the netshoot plugin: kubectl krew install netshoot
        fluxcd # helmrelease reconciliation
        cmctl # cert-manager certificate renewal
        prometheus-alertmanager # amtool for alert silences
        mimirtool # prometheus metrics usage analyses

        postgresql # psql and pg_dump for db-operator databases
        restic # the restic backup detective queries repositories directly
        gopass # secrets for alertmanager, grafana, prometheus

        dig # DNS detectives probe the nameservers directly
        openssl # mail detectives probe TLS on the SMTP/POP3/IMAP ports
        fzf # interactive fuzzy selection everywhere
        gum # nicer yes/no confirmations (optional at runtime)
        upterm # optional: share a live terminal with colleagues to investigate together
        iputils # ping for connectivity prerequisites
        openssh # ceph books talk to cephadm hosts over ssh
      ];

      # Extra packages the container image needs for a usable interactive shell,
      # which a host provides for the dev shell but a minimal base image does not.
      imageExtras = pkgs: with pkgs; [ bashInteractive coreutils cacert ];

      # The CI gates. verify.yml and release.yml run every check through this
      # flake's devShell (nix develop --command ...), so CI and local shells
      # resolve the exact same tool versions from flake.lock - Renovate keeps
      # the lock fresh, and the metio policy check enforces that no job installs
      # a tool any other way.
      ciTools = pkgs: with pkgs; [
        reuse # REUSE/SPDX licensing lint
        typos # spell check
        yamllint # workflow + config YAML
        actionlint # GitHub Actions
        shellcheck # actionlint shells out to it for run: blocks
        markdownlint-cli2 # docs
      ];
    in
    {
      # A single derivation carrying every tool on its bin/, for the container
      # image to put on PATH. dev/Containerfile builds this.
      packages = forAllSystems (pkgs: {
        default = pkgs.buildEnv {
          name = "gumshoe-tools";
          paths = (toolsFor pkgs) ++ (imageExtras pkgs);
        };
      });

      devShells = forAllSystems (pkgs: {
        default = pkgs.mkShell {
          packages = (toolsFor pkgs) ++ (ciTools pkgs);

          shellHook = ''
            echo "📚 gumshoe - run books with: bb <path-to-book> --help"
            echo "   run the test suite with: bb test"
            echo "   netshoot books also need: kubectl krew install netshoot"
          '';
        };
      });

      formatter = forAllSystems (pkgs: pkgs.nixfmt-rfc-style);
    };
}

# https://just.systems

@help:
    just --list

# Bump version, commit, tag, and publish (update CHANGELOG.md first!)
publish:
    #!/usr/bin/env bash
    set -euo pipefail
    elm bump
    version=$(jq -r .version elm.json)
    echo "Publishing version $version..."
    git add -A
    git commit -m "Release $version"
    git tag "$version"
    git push
    git push origin "$version"
    elm publish


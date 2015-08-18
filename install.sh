set -e

git submodule init
git submodule update
rsync -av .* ~ --exclude=.git --exclude=.gitmodules --exclude=README.md
chmod 700 ~/.ssh
chmod 600 ~/.ssh/*

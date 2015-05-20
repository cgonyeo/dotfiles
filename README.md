# Dotfiles

--------

A dumping group for all my dotfiles I use.

To use:

```bash
git clone https://github.com/dgonyeo/dotfiles.git
cd dotfiles
git submodule init
git submodule update
rsync -av .* ~ --exclude=.git --exclude=.gitmodules
```

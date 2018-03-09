if g:dein#_cache_version != 100 | throw 'Cache loading error' | endif
let [plugins, ftplugin] = dein#load_cache_raw(['/home/derek/.config/nvim/init.vim'])
if empty(plugins) | throw 'Cache loading error' | endif
let g:dein#_plugins = plugins
let g:dein#_ftplugin = ftplugin
let g:dein#_base_path = '/home/derek/.vim/dein-bundles'
let g:dein#_runtime_path = '/home/derek/.vim/dein-bundles/.cache/init.vim/.dein'
let g:dein#_cache_path = '/home/derek/.vim/dein-bundles/.cache/init.vim'
let &runtimepath = '/home/derek/.config/nvim,/etc/xdg/nvim,/home/derek/.local/share/nvim/site,/usr/local/share/nvim/site,/home/derek/.vim/dein-bundles/repos/github.com/Shougo/dein.vim,/home/derek/.vim/dein-bundles/.cache/init.vim/.dein,/usr/share/nvim/site,/usr/share/nvim/runtime,/home/derek/.vim/dein-bundles/.cache/init.vim/.dein/after,/usr/share/nvim/site/after,/usr/local/share/nvim/site/after,/home/derek/.local/share/nvim/site/after,/etc/xdg/nvim/after,/home/derek/.config/nvim/after,/home/derek/.vim,/home/derek/.vim/after'
filetype off

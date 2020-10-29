syntax on
set nocompatible              " be iMproved, required

set tabstop=4 softtabstop=4
set shiftwidth=4
set expandtab

set ignorecase
set smartcase

set smartindent
set nowrap
set noswapfile
set nobackup
set undodir=~/.vim/undodir
set undofile
set incsearch

set number                     " Show current line number
set relativenumber             " Show relative line numbers


" Disable the default Vim startup message.
set shortmess+=F  " to get rid of the file name displayed in the command line bar
"set noshowmode  " to get rid of thing like --INSERT--
"set noshowcmd  " to get rid of display of last command

" Disable audible bell because it's annoying.
set noerrorbells visualbell t_vb=

set mouse+=a " Enable mouse support

set colorcolumn=100
highlight ColorColumn ctermbg=0 guibg=lightgrey

call plug#begin('~/.vim/plugged')

Plug 'VundleVim/Vundle.vim'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
"Plugin 'leafgarland/typescript-vim'
Plug 'mbbill/undotree'
Plug 'prettier/vim-prettier', { 'do': 'yarn install' }
Plug 'preservim/nerdcommenter'
"Plugin 'mxw/vim-jsx'
"Plugin 'pangloss/vim-javascript'
"Plugin 'jiangmiao/auto-pairs'
Plug 'tpope/vim-fugitive'
Plug 'gruvbox-community/gruvbox'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

call plug#end()            " required
filetype plugin indent on    " required

let g:gruvbox_contrast_dark = 'hard'
set background=dark
colorscheme gruvbox

let mapleader = " "

nnoremap <leader>h :wincmd h<CR>
nnoremap <leader>j :wincmd j<CR>
nnoremap <leader>k :wincmd k<CR>
nnoremap <leader>l :wincmd l<CR>
nnoremap <leader>H :windo wincmd H<CR>
nnoremap <leader>J :windo wincmd J<CR>
nnoremap <leader>K :windo wincmd K<CR>
nnoremap <leader>L :windo wincmd L<CR>
nnoremap <leader>o :only<CR>
nnoremap <leader>u :UndotreeShow<CR>
nnoremap <leader>dr :wincmd v<bar> :Ex <bar> :vertical resize 30<CR>
nnoremap <Leader>ps :Rg<SPACE>
nnoremap <Leader>= :vertical resize +5<CR>
nnoremap <Leader>- :vertical resize -5<CR>
nnoremap <Leader>+ :resize +5<CR>
nnoremap <Leader>_ :resize -5<CR>
nnoremap <Leader>vsp :vsplit<CR>
nnoremap <Leader>hsp :split<CR>

nmap <Leader>gj :diffget //3<CR>
nmap <Leader>gf :diffget //2<CR>
nmap <Leader>gs :G<CR>

" Latex compilation commands
autocmd FileType tex nmap <buffer> <C-T> :w <bar> !latexmk -pdf %<CR>
autocmd FileType tex let b:did_indent = 1

set hidden
set cmdheight=2
" Use tab for trigger completion with characters ahead and navigate.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

"" Set Prettier on by default
let g:prettier#autoformat = 1
let g:prettier#autoformat_require_pragma = 0
""
"" Trigger configuration. You need to change this to something other than <tab> if you use one of the following:
"" - https://github.com/Valloric/YouCompleteMe
"" - https://github.com/nvim-lua/completion-nvim
"let g:UltiSnipsExpandTrigger="<tab>"
"let g:UltiSnipsJumpForwardTrigger="<c-b>"
"let g:UltiSnipsJumpBackwardTrigger="<c-z>"

"packloadall

 "let g:prettier#autoformat = 1
 "let g:prettier#autoformat_require_pragma = 0

"filetype plugin on

""COC
"" TextEdit might fail if hidden is not set.
"set hidden

"" Some servers have issues with backup files, see #649.
"set nobackup
"set nowritebackup

"" Give more space for displaying messages.
"set cmdheight=2

"" Having longer updatetime (default is 4000 ms = 4 s) leads to noticeable
"" delays and poor user experience.
"set updatetime=300

"" Don't pass messages to |ins-completion-menu|.
"set shortmess+=c

"" Always show the signcolumn, otherwise it would shift the text each time
"" diagnostics appear/become resolved.
"if has("patch-8.1.1564")
  "" Recently vim can merge signcolumn and number column into one
  "set signcolumn=number
"else
  "set signcolumn=yes
"endif

"" Use tab for trigger completion with characters ahead and navigate.
"" NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
"" other plugin before putting this into your config.
"inoremap <silent><expr> <TAB>
      "\ pumvisible() ? "\<C-n>" :
      "\ <SID>check_back_space() ? "\<TAB>" :
      "\ coc#refresh()
"inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

"function! s:check_back_space() abort
  "let col = col('.') - 1
  "return !col || getline('.')[col - 1]  =~# '\s'
"endfunction

"" Use <c-space> to trigger completion.
"if has('nvim')
  "inoremap <silent><expr> <c-space> coc#refresh()
"else
  "inoremap <silent><expr> <c-@> coc#refresh()
"endif

"" Use <cr> to confirm completion, `<C-g>u` means break undo chain at current
"" position. Coc only does snippet and additional edit on confirm.
"" <cr> could be remapped by other vim plugin, try `:verbose imap <CR>`.
"if exists('*complete_info')
  "inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"
"else
  "inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
"endif

"" Use `[g` and `]g` to navigate diagnostics
"" Use `:CocDiagnostics` to get all diagnostics of current buffer in location list.
"nmap <silent> [g <Plug>(coc-diagnostic-prev)
"nmap <silent> ]g <Plug>(coc-diagnostic-next)

"" GoTo code navigation.
"nmap <silent> gd <Plug>(coc-definition)
"nmap <silent> gy <Plug>(coc-type-definition)
"nmap <silent> gi <Plug>(coc-implementation)
"nmap <silent> gr <Plug>(coc-references)

"" Use K to show documentation in preview window.
"nnoremap <silent> K :call <SID>show_documentation()<CR>

"function! s:show_documentation()
  "if (index(['vim','help'], &filetype) >= 0)
    "execute 'h '.expand('<cword>')
  "else
    "call CocAction('doHover')
  "endif
"endfunction

"" Highlight the symbol and its references when holding the cursor.
"autocmd CursorHold * silent call CocActionAsync('highlight')

"" Symbol renaming.
"nmap <leader>rn <Plug>(coc-rename)

"" Formatting selected code.
"xmap <leader>f  <Plug>(coc-format-selected)
"nmap <leader>f  <Plug>(coc-format-selected)

"augroup mygroup
  "autocmd!
  "" Setup formatexpr specified filetype(s).
  "autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
  "" Update signature help on jump placeholder.
  "autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
"augroup end

"" Applying codeAction to the selected region.
"" Example: `<leader>aap` for current paragraph
"xmap <leader>a  <Plug>(coc-codeaction-selected)
"nmap <leader>a  <Plug>(coc-codeaction-selected)

"" Remap keys for applying codeAction to the current buffer.
"nmap <leader>ac  <Plug>(coc-codeaction)
"" Apply AutoFix to problem on the current line.
"nmap <leader>qf  <Plug>(coc-fix-current)

"" Map function and class text objects
"" NOTE: Requires 'textDocument.documentSymbol' support from the language server.
"xmap if <Plug>(coc-funcobj-i)
"omap if <Plug>(coc-funcobj-i)
"xmap af <Plug>(coc-funcobj-a)
"omap af <Plug>(coc-funcobj-a)
"xmap ic <Plug>(coc-classobj-i)
"omap ic <Plug>(coc-classobj-i)
"xmap ac <Plug>(coc-classobj-a)
"omap ac <Plug>(coc-classobj-a)

"" Use CTRL-S for selections ranges.
"" Requires 'textDocument/selectionRange' support of LS, ex: coc-tsserver
"nmap <silent> <C-s> <Plug>(coc-range-select)
"xmap <silent> <C-s> <Plug>(coc-range-select)

"" Add `:Format` command to format current buffer.
"command! -nargs=0 Format :call CocAction('format')

"" Add `:Fold` command to fold current buffer.
"command! -nargs=? Fold :call     CocAction('fold', <f-args>)

"" Add `:OR` command for organize imports of the current buffer.
"command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')

"" Add (Neo)Vim's native statusline support.
"" NOTE: Please see `:h coc-status` for integrations with external plugins that
"" provide custom statusline: lightline.vim, vim-airline.
"set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}

"" Mappings for CoCList
"" Show all diagnostics.
""nnoremap <silent><nowait> <space>a  :<C-u>CocList diagnostics<cr>
"" Manage extensions.
""nnoremap <silent><nowait> <space>e  :<C-u>CocList extensions<cr>
"" Show commands.
""nnoremap <silent><nowait> <space>c  :<C-u>CocList commands<cr>
"" Find symbol of current document.
""nnoremap <silent><nowait> <space>o  :<C-u>CocList outline<cr>
"" Search workspace symbols.
""nnoremap <silent><nowait> <space>s  :<C-u>CocList -I symbols<cr>
"" Do default action for next item.
""nnoremap <silent><nowait> <space>j  :<C-u>CocNext<CR>
"" Do default action for previous item.
""nnoremap <silent><nowait> <space>k  :<C-u>CocPrev<CR>
"" Resume latest coc list.
""nnoremap <silent><nowait> <space>p  :<C-u>CocListResume<CR>



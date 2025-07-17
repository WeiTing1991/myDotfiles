set mouse=a

if exists('g:fvim_loaded')
    " good old 'set guifont' compatibility with HiDPI hints...
    if g:fvim_os == 'windows' || g:fvim_render_scale > 1.0
      set guifont=JetBrainsMono\ Nerd\ Font:h14
    else
      set guifont=JetBrainsMono\ Nerd\ Font:h28
    endif

    nnoremap <silent> <C--> :set guifont=-<CR>
    nnoremap <silent> <C-=> :set guifont=+<CR>
    nnoremap <A-CR> :FVimToggleFullScreen<CR>
    nnoremap <Tab> za
    vnoremap <Tab> za
endif

if exists('g:fvim_loaded')
    FVimCustomTitleBar v:true
    FVimFontNormalWeight 400
    FVimFontBoldWeight 700

    FVimCursorSmoothMove v:false
    FVimCursorSmoothBlink v:true
    FVimUIPopupMenu v:true
    FVimUIWildMenu v:false

    FVimFontAntialias v:true
    FVimFontAutohint v:true
    FVimFontHintLevel 'full'
    FVimFontLigature v:true
    FVimFontLineHeight '+1.0'
    FVimFontSubpixel v:true
    " FVimFontNoBuiltinSymbols v:true
endif


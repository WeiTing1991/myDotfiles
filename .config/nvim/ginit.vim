if exists('g:fvim_loaded')
    " Adjust guifont based on OS and render scale
    if g:fvim_os == 'windows' || g:fvim_render_scale > 1.0
        set guifont=Hack\ Nerd\ Font:h16
    else
        set guifont=Hack\ Nerd\ Font:h16
    endif
    " Cursor tweaks
    FVimCursorSmoothMove v:true
    FVimCursorSmoothBlink v:true

    " Debug UI overlay
    FVimDrawFPS v:false

    " Font tweaks
    FVimFontAntialias v:true
    FVimFontAutohint v:true
    FVimFontHintLevel 'full'
    FVimFontLigature v:true
    FVimFontSubpixel v:true
    FVimFontAutoSnap v:true

    FVimFontNoBuiltinSymbols v:true

    " Font weight tuning, possible valuaes are 100..900
    FVimFontNormalWeight 400
    FVimFontBoldWeight 701

    " Font debugging -- draw bounds around each glyph
    FVimFontDrawBounds v:false

    " UI options (all default to v:false)
    FVimUIPopupMenu v:true

    " Default options (workspace-agnostic)
    FVimDefaultWindowWidth 1000
    FVimDefaultWindowHeight 600
endif


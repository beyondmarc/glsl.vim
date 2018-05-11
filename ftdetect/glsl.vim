if !exists('g:glsl_default_version')
    let g:glsl_default_version = 'glsl330'
endif

if !exists('g:glsl_file_extensions')
    let g:glsl_file_extensions = '*.glsl,*.vsh,*.fsh,*.vert,*.tesc,*.tese,*.geom,*.frag,*.comp'
endif

func! s:set_glsl_filetype()
  for item in getline(1,30)
    if item =~ "#version 460"
      execute ':set filetype=glsl460'
      break
    endif
    if item =~ "#version 450"
      execute ':set filetype=glsl450'
      break
    endif
    if item =~ "#version 440"
      execute ':set filetype=glsl440'
      break
    endif
    if item =~ "#version 430"
      execute ':set filetype=glsl430'
      break
    endif
    if item =~ "#version 420"
      execute ':set filetype=glsl420'
      break
    endif
    if item =~ "#version 410"
      execute ':set filetype=glsl410'
      break
    endif
    if item =~ "#version 400"
      execute ':set filetype=glsl400'
      break
    endif
    if item =~ "#version 330"
      execute ':set filetype=glsl330'
      break
    endif
    " Default glsl syntax highlighting
    execute ':set filetype='.g:glsl_default_version
  endfor
endfunction

execute 'autocmd BufRead,BufNewFile '.g:glsl_file_extensions.' call s:set_glsl_filetype()'

if !exists('g:glsl_default_version')
    let g:glsl_default_version = 'glsl330'
endif

if !exists('g:glsl_file_extensions')
    let g:glsl_file_extensions = '*.glsl,*.vsh,*.fsh'
endif

execute 'autocmd BufRead,BufNewFile '.g:glsl_file_extensions.' setfiletype '.g:glsl_default_version

if !exists('g:glsl_default_version')
    let g:glsl_default_version = 'glsl330'
endif

execute 'autocmd BufRead,BufNewFile *.vp,*.fp,*.gp,*.vs,*.fs,*.gs,*.tcs,*.tes,*.cs,*.vert,*.frag,*.geom,*.tess,*.shd,*.gls,*.glsl,*.vsh,*.fsh setfiletype '.g:glsl_default_version

glsl.vim
========

OpenGL Shading Language (GLSL) Vim syntax highlighting

Instructions
-----------------------------------------------------------

Using the Pathogen plugin:
(http://vimcasts.org/episodes/synchronizing-plugins-with-git-submodules-and-pathogen/)

From your ~/.vim directory:
    git submodule add git://github.com/beyondmarc/glsl.vim.git bundle/syntax_glsl

If not using the Pathogen plugin:
Add the glsl\*\*\*.vim file into your vim syntax folder (in most Linux distros:
/usr/share/vim/vim\<version\>/syntax).

Then add the following line at the bottom of your .vimrc file:
    
  autocmd BufNewFile,BufRead \*.vp,\*.fp,\*.gp,\*.vs,\*.fs,\*.gs,\*.tcs,\*.tes,\*.cs,\*.vert,\*.frag,\*.geom,\*.tess,\*.shd,\*.gls,\*.glsl set ft=glsl\*\*\*


# glsl.vim

OpenGL Shading Language (GLSL) Vim syntax highlighting.

## Instructions

Using the [Pathogen plugin](http://vimcasts.org/episodes/synchronizing-plugins-with-git-submodules-and-pathogen/):

* From your `~/.vim` directory:

   `git submodule add https://github.com/beyondmarc/glsl.vim.git bundle/glsl.vim`

If not using the Pathogen plugin:

* Add this directory to your `runtimepath`, in your `.vimrc`:

   `set rtp^=/path/to/glsl.vim`

### Choosing the GLSL version to work with

The plugin will try to automatically detect which version to use by looking for
the text `#version XXX` in the 30 first lines of the file.

If the version of the file can't be determined, the default version will be used.

To set the default GLSL version for which the syntax is loaded, set the global
variable `glsl_default_version` in your `.vimrc` before loading the plugin:

    let g:glsl_default_version = 'glsl330'

If this variable is not set, it will default to `glsl330`.

### Choosing which file extensions to interpret as GLSL files

To set the list of file extensions for which the syntax is loaded, set the
global variable `glsl_file_extensions` in your `.vimrc` before loading the plugin:

    let g:glsl_file_extensions = '*.glsl,*.vsh,*.fsh,*.vert,*.tesc,*.tese,*.geom,*.frag,*.comp'

If this variable is not set, it will default to `*.glsl,*.vsh,*.fsh,*.vert,*.tesc,*.tese,*.geom,*.frag,*.comp`.

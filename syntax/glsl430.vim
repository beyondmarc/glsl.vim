" Vim syntax file the OpenGL Shading Language
" Language:     GLSL 430
" Author:       Marc Costa <beyond.marc@gmail.com>
" Date:         November 17, 2012
" File Types:   .glsl
" Version:      1
" Notes:        Adapted from glsl400.vim - Kamil Patelczyk <patelczyk@gmail.com>
"               Adapted from c.vim - Bram Moolenaar <bram.vim.org>
"               Adapted from cg.vim - Kevin Bjorke <kbjorke@nvidia.com>
"               Adapted from glsl.vim - Nathan Cournia <nathan@cournia.com>

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" glslCommentGroup allows adding matches for special things in comments
syn cluster         glslCommentGroup    contains=glslTodo

"catch errors caused by wrong parenthesis and brackets
syn cluster         glslParenGroup      contains=glslParenError,glslIncluded,glslSpecial,glslCommentSkip,glslCommentString,glslComment2String,@glslCommentGroup,glslCommentStartError,glslUserCont,glslUserLabel,glslBitField,glslCommentSkip,glslOctalZero,glslCppOut,glslCppOut2,glslCppSkip,glslFormat,glslNumber,glslFloat,glslOctal,glslOctalError,glslNumbersCom,glslLayoutQual,glslMemoryQual
if exists("c_no_bracket_error")
  syn region        glslParen           transparent start='(' end=')' contains=ALLBUT,@glslParenGroup,glslCppParen,glslCppString
  " glslCppParen: same as glslParen but ends at end-of-line; used in glslDefine
  syn region        glslCppParen        transparent start='(' skip='\\$' excludenl end=')' end='$' contained contains=ALLBUT,@glslParenGroup,glslParen,glslString
  syn match         glslParenError      display ")"
  syn match         glslErrInParen      display contained "[{}]"
else
  syn region        glslParen           transparent start='(' end=')' contains=ALLBUT,@glslParenGroup,glslCppParen,glslErrInBracket,glslCppBracket,glslCppString
  " glslCppParen: same as glslParen but ends at end-of-line; used in glslDefine
  syn region        glslCppParen        transparent start='(' skip='\\$' excludenl end=')' end='$' contained contains=ALLBUT,@glslParenGroup,glslErrInBracket,glslParen,glslBracket,glslString
  syn match         glslParenError      display "[\])]"
  syn match         glslErrInParen      display contained "[\]{}]"
  syn region        glslBracket         transparent start='\[' end=']' contains=ALLBUT,@glslParenGroup,glslErrInParen,glslCppParen,glslCppBracket,glslCppString
  " glslCppBracket: same as glslParen but ends at end-of-line; used in glslDefine
  syn region        glslCppBracket      transparent start='\[' skip='\\$' excludenl end=']' end='$' contained contains=ALLBUT,@glslParenGroup,glslErrInParen,glslParen,glslBracket,glslString
  syn match         glslErrInBracket    display contained "[);{}]"
endif

"integer number, or floating point number without a dot and with "f".
syn case ignore
syn match           glslNumbers         display transparent "\<\d\|\.\d" contains=glslNumber,glslFloat,glslOctalError,glslOctal
" Same, but without octal error (for comments)
syn match           glslNumbersCom      display contained transparent "\<\d\|\.\d" contains=glslNumber,glslFloat,glslOctal
syn match           glslNumber          display contained "\d\+\(u\=l\{0,2}\|ll\=u\)\>"
"hex number
syn match           glslNumber          display contained "0x\x\+\(u\=l\{0,2}\|ll\=u\)\>"
" Flag the first zero of an octal number as something special
syn match           glslOctal           display contained "0\o\+\(u\=l\{0,2}\|ll\=u\)\>" contains=glslOctalZero
syn match           glslOctalZero       display contained "\<0"
syn match           glslFloat           display contained "\d\+f"
"floating point number, with dot, optional exponent
syn match           glslFloat           display contained "\d\+\.\d*\(e[-+]\=\d\+\)\=[fl]\="
"floating point number, starting with a dot, optional exponent
syn match           glslFloat           display contained "\.\d\+\(e[-+]\=\d\+\)\=[fl]\=\>"
"floating point number, without dot, with exponent
syn match           glslFloat           display contained "\d\+e[-+]\=\d\+[fl]\=\>"
" flag an octal number with wrong digits
syn match           glslOctalError      display contained "0\o*[89]\d*"
syn case match

if exists("c_comment_strings")
  " A comment can contain glslString, glslCharacter and glslNumber.
  " But a "*/" inside a glslString in a glslComment DOES end the comment!  So we
  " need to use a special type of glslString: glslCommentString, which also ends on
  " "*/", and sees a "*" at the start of the line as glslomment again.
  " Unfortunately this doesn't very well work for // type of comments :-(
  syntax match      glslCommentSkip         contained "^\s*\*\($\|\s\+\)"
  syntax region     glslCommentString       contained start=+L\=\\\@<!"+ skip=+\\\\\|\\"+ end=+"+ end=+\*/+me=s-1 contains=glslSpecial,glslCommentSkip
  syntax region     glslComment2String      contained start=+L\=\\\@<!"+ skip=+\\\\\|\\"+ end=+"+ end="$" contains=glslSpecial
  syntax region     glslCommentL            start="//" skip="\\$" end="$" keepend contains=@glslCommentGroup,glslComment2String,glslCharacter,glslNumbersCom,glslSpaceError
  syntax region     glslComment             matchgroup=glslCommentStart start="/\*" matchgroup=NONE end="\*/" contains=@glslCommentGroup,glslCommentStartError,glslCommentString,glslCharacter,glslNumbersCom,glslSpaceError
else                                        
  syn region        glslCommentL            start="//" skip="\\$" end="$" keepend contains=@glslCommentGroup,glslSpaceError
  syn region        glslComment             matchgroup=glslCommentStart start="/\*" matchgroup=NONE end="\*/" contains=@glslCommentGroup,glslCommentStartError,glslSpaceError
endif
" keep a // comment separately, it terminates a preproc. conditional
syntax match        glslCommentError        display "\*/"
syntax match        glslCommentStartError   display "/\*"me=e-1 contained

syn region          glslPreCondit           start="^\s*#\s*\(if\|ifdef\|ifndef\|elif\)\>" skip="\\$" end="$" end="//"me=s-1 contains=glslComment,glslCppString,glslCharacter,glslCppParen,glslParenError,glslNumbers,glslCommentError,glslSpaceError
syn match           glslPreCondit           display "^\s*#\s*\(else\|endif\)\>"
syn region          glslCppOut              start="^\s*#\s*if\s\+0\+\>" end=".\|$" contains=glslCppOut2
syn region          glslCppOut2             contained start="0" end="^\s*#\s*\(endif\>\|else\>\|elif\>\)" contains=glslSpaceError,glslCppSkip
syn region          glslCppSkip             contained start="^\s*#\s*\(if\>\|ifdef\>\|ifndef\>\)" skip="\\$" end="^\s*#\s*endif\>" contains=glslSpaceError,glslCppSkip
"syn match glslLineSkip        "\\$"
syn cluster         glslPreProglslGroup     contains=glslPreCondit,glslIncluded,glslInclude,glslDefine,glslErrInParen,glslErrInBracket,glslUserLabel,glslSpecial,glslOctalZero,glslCppOut,glslCppOut2,glslCppSkip,glslFormat,glslNumber,glslFloat,glslOctal,glslOctalError,glslNumbersCom,glslString,glslCommentSkip,glslCommentString,glslComment2String,@glslCommentGroup,glslCommentStartError,glslParen,glslBracket,glslMulti
syn region          glslDefine              start="^\s*#\s*\(define\|undef\)\>" skip="\\$" end="$" end="//"me=s-1 contains=ALLBUT,@glslPreProglslGroup
syn region          glslPreProc             start="^\s*#\s*\(pragma\>\|line\>\|error\>\|version\>\|extension\>\)" skip="\\$" end="$" keepend contains=ALLBUT,@glslPreProglslGroup

" Highlight User Labels
syn cluster         glslMultiGroup          contains=glslIncluded,glslSpecial,glslCommentSkip,glslCommentString,glslComment2String,@glslCommentGroup,glslCommentStartError,glslUserCont,glslUserLabel,glslBitField,glslOctalZero,glslCppOut,glslCppOut2,glslCppSkip,glslFormat,glslNumber,glslFloat,glslOctal,glslOctalError,glslNumbersCom,glslCppParen,glslCppBracket,glslCppString
syn region          glslMulti               transparent start='?' skip='::' end=':' contains=ALLBUT,@glslMultiGroup
" Avoid matching foo::bar() in C++ by requiring that the next char is not ':'
syn cluster         glslLabelGroup          contains=glslUserLabel
syn match           glslUserCont            display "^\s*\I\i*\s*:$" contains=@glslLabelGroup
syn match           glslUserCont            display ";\s*\I\i*\s*:$" contains=@glslLabelGroup
syn match           glslUserCont            display "^\s*\I\i*\s*:[^:]"me=e-1 contains=@glslLabelGroup
syn match           glslUserCont            display ";\s*\I\i*\s*:[^:]"me=e-1 contains=@glslLabelGroup

syn match           glslUserLabel           display "\I\i*" contained

" Avoid recognizing most bitfields as labels
syn match           glslBitField            display "^\s*\I\i*\s*:\s*[1-9]"me=e-1
syn match           glslBitField            display ";\s*\I\i*\s*:\s*[1-9]"me=e-1


" Keywords and simple stuff 
syn keyword         glslTodo                contained TODO FIXME XXX

syn keyword         glslStorageClass        const uniform
syn keyword         glslStorageClass        in out inout
syn keyword         glslStorageClass        patch sample
syn keyword         glslStorageClass        centroid flat smooth noperspective
syn keyword         glslStorageClass        lowp mediump highp precision
syn keyword         glslStorageClass        invariant
syn keyword         glslStorageClass        subroutine
syn keyword         glslStorageClass        buffer

syn match           glslLayout              /\<layout\s*(\_.\{-})/hs=s,he=s+6 contains=glslLayoutQual,glslNumber
                                            
syn keyword         glslStorageClassDeprec  attribute varying

syn keyword         glslLayoutQual          contained depth_any depth_greater depth_less depth_unchanged
syn keyword         glslLayoutQual          contained shared packed std140 row_major column_major ccw cw equal_spacing fractional_even_spacing
syn keyword         glslLayoutQual          contained fractional_odd_spacing index invocations isolines line_strip lines lines_adjacency location
syn keyword         glslLayoutQual          contained max_vertices origin_upper_left pixel_center_integer point_mode points quads
syn keyword         glslLayoutQual          contained stream triangle_strip triangles triangles_adjacency vertices
syn keyword         glslLayoutQual          contained local_size_x local_size_y local_size_z
syn keyword         glslLayoutQual          contained std430

syn keyword         glslMemoryQual          coherent volatile restrict readonly writeonly

syn keyword         glslStatement           break continue discard return
syn keyword         glslRepeat              while for do
syn keyword         glslConditional         if else switch
syn keyword         glslLabel               case default
                                            
syn keyword         glslConstant            true false
syn keyword         glslConstant            __LINE__ __FILE__ __VERSION__
                                            
syn keyword         glslType                uint uvec2 uvec3 uvec4
syn keyword         glslType                float double int void bool
syn keyword         glslType                mat2 mat3 mat4 dmat2 dmat3 dmat4
syn keyword         glslType                mat2x2 mat2x3 mat2x4 dmat2x2 dmat2x3 dmat2x4
syn keyword         glslType                mat3x2 mat3x3 mat3x4 dmat3x2 dmat3x3 dmat3x4
syn keyword         glslType                mat4x2 mat4x3 mat4x4 dmat4x2 dmat4x3 dmat4x4
syn keyword         glslType                vec2 vec3 vec4 ivec2 ivec3 ivec4 bvec2 bvec3 bvec4 dvec2 dvec3 dvec4
syn keyword         glslType                sampler1D sampler2D sampler3D samplerCube 
syn keyword         glslType                sampler1DShadow sampler2DShadow samplerCubeShadow
syn keyword         glslType                sampler1DArray sampler2DArray
syn keyword         glslType                sampler1DArrayShadow sampler2DArrayShadow
syn keyword         glslType                isampler1D isampler2D isampler3D isamplerCube
syn keyword         glslType                isampler1DArray isampler2DArray
syn keyword         glslType                usampler1D usampler2D usampler3D usamplerCube
syn keyword         glslType                usampler1DArray usampler2DArray
syn keyword         glslType                sampler2DRect sampler2DRectShadow isampler2DRect usampler2DRect
syn keyword         glslType                samplerBuffer isamplerBuffer usamplerBuffer
syn keyword         glslType                sampler2DMS isampler2DMS usampler2DMS
syn keyword         glslType                sampler2DMSArray isampler2DMSArray usampler2DMSArray
syn keyword         glslType                samplerCubeArray samplerCubeArrayShadow isamplerCubeArray usamplerCubeArray
                                            
syn keyword         glslStructure           struct
                                            
" Swizzling                                  
syn match           glslSwizzle             /\.[xyzw]\{1,4\}\>/
syn match           glslSwizzle             /\.[rgba]\{1,4\}\>/
syn match           glslSwizzle             /\.[stpq]\{1,4\}\>/
                                            
" Builtin variables
syn keyword         glslState               gl_FragCoord gl_FrontFacing gl_PointCoord gl_SampleID gl_SamplePosition gl_FragDepth
syn keyword         glslState               gl_VertexID gl_InstanceID gl_Position gl_PointSize
syn keyword         glslState               gl_PrimitiveIDIn gl_InvocationID gl_PrimitiveID gl_Layer gl_ViewportIndex
syn keyword         glslState               gl_ClipDistance gl_in gl_SampleMask
syn keyword         glslState               gl_NumWorkGroups gl_WorkGroupSize gl_WorkGroupID gl_LocalInvocationID gl_GlobalInvocationID gl_LocalInvocationIndex

syn keyword         glslStateDeprec         gl_FragColor gl_FragData gl_TexCoord
syn keyword         glslStateDeprec         gl_ClipVertex gl_FrontColor gl_BackColor gl_FrontSecondaryColor gl_BackSecondaryColor gl_FogFragCoord
syn keyword         glslStateDeprec         gl_Color gl_SecondaryColor gl_Normal gl_Vertex gl_MultiTexCoord0 gl_MultiTexCoord1 gl_MultiTexCoord2
syn keyword         glslStateDeprec         gl_MultiTexCoord3 gl_MultiTexCoord4 gl_MultiTexCoord5 gl_MultiTexCoord6 gl_MultiTexCoord7 gl_FogCoord

" Uniforms variables
syn keyword         glslUniform             gl_DepthRange

syn keyword         glslUniformDeprec       gl_ModelViewMatrix gl_ProjectionMatrix gl_ModelViewProjectionMatrix gl_NormalMatrix gl_NormalScale
syn keyword         glslUniformDeprec       gl_ModelViewMatrixInverse gl_ProjectionMatrixInverse gl_ModelViewProjectionMatrixInverse
syn keyword         glslUniformDeprec       gl_ModelViewMatrixTranspose gl_ProjectionMatrixTranspose gl_ModelViewProjectionMatrixTranspose
syn keyword         glslUniformDeprec       gl_ModelViewMatrixInverseTranspose gl_ProjectionMatrixInverseTranspose gl_ModelViewProjectionMatrixInverseTranspose 
syn keyword         glslUniformDeprec       gl_Point gl_FrontMaterial gl_BackMaterial gl_LightModel gl_FrontLightModelProduct gl_BackLightModelProduct gl_Fog
syn keyword         glslUniformDeprec       gl_BackLightProduct gl_ClipPlane gl_EyePlaneQ gl_EyePlaneR gl_EyePlaneS gl_EyePlaneT
syn keyword         glslUniformDeprec       gl_FrontLightProduct gl_LightSource gl_ObjectPlaneQ gl_ObjectPlaneR gl_ObjectPlaneS
syn keyword         glslUniformDeprec       gl_ObjectPlaneT gl_TextureEnvColor gl_TextureMatrixInverseTranspose gl_TextureMatrixInverse
syn keyword         glslUniformDeprec       gl_TextureMatrixTranspose gl_TextureMatrix

" Builtin constants
syn keyword         glslConstant            gl_MaxVertexAttribs gl_MaxVertexUniformComponents gl_MaxVertexOutputComponents gl_MaxGeometryInputComponents
syn keyword         glslConstant            gl_MaxGeometryOutputComponents gl_MaxFragmentInputComponents gl_MaxVertexTextureImageUnits
syn keyword         glslConstant            gl_MaxCombinedTextureImageUnits gl_MaxTextureImageUnits gl_MaxFragmentUniformComponents gl_MaxDrawBuffers
syn keyword         glslConstant            gl_MaxClipDistances gl_MaxGeometryTextureImageUnits gl_MaxGeometryOutputVertices gl_MaxGeometryTotalOutputComponents
syn keyword         glslConstant            gl_MaxGeometryUniformComponents gl_MaxGeometryVaryingComponents gl_MaxTessControlInputComponents gl_MaxTessControlOutputComponents
syn keyword         glslConstant            gl_MaxTessControlTextureImageUnits gl_MaxTessControlUniformComponents gl_MaxTessControlTotalOutputComponents
syn keyword         glslConstant            gl_MaxTessEvaluationInputComponents gl_MaxTessEvaluationOutputComponents gl_MaxTessEvaluationTextureImageUnits
syn keyword         glslConstant            gl_MaxTessEvaluationUniformComponents gl_MaxTessPatchComponents gl_MaxPatchVertices gl_MaxTessGenLevel
syn keyword         glslConstant            gl_MaxComputeWorkGroupCount gl_MaxComputeWorkGroupSize gl_MaxComputeUniformComponents gl_MaxComputeTextureImageUnits
syn keyword         glslConstant            gl_MaxComputeImageUniforms gl_MaxComputeAtomicCounters gl_MaxComputeAtomicCounterBuffers

syn keyword         glslConstantDeprec      gl_MaxVaryingFloats gl_MaxVaryingComponents
syn keyword         glslConstantDeprec      gl_MaxTextureUnits gl_MaxTextureCoords gl_MaxClipPlanes

" Built in functions                         
syn keyword         glslFunc                abs acos acosh asin asinh atan atanh ceil cos cosh degrees exp exp2 floor fract inversesqrt log
syn keyword         glslFunc                log2 max min mod modf pow radians round roundEven sign sin sinh sqrt tan tanh trunc
syn keyword         glslFunc                clamp cross distance dot floatBitsToInt floatBitsToUint fma frexp intBitsToFloat isinf isnan
syn keyword         glslFunc                ldexp length mix normalize packDouble2x32 packSnorm4x8 packUnorm2x16 packSnorm2x16 packUnorm4x8
syn keyword         glslFunc                uintBitsToFloat unpackDouble2x32 unpackSnorm4x8 unpackUnorm2x16 unpackSnorm2x16 unpackUnorm4x8
syn keyword         glslFunc                packHalf2x16 unpackHalf2x16 smoothstep step
syn keyword         glslFunc                all any bitCount bitfieldExtract bitfieldInsert bitfieldReverse determinant equal faceforward
syn keyword         glslFunc                findLSB findMSB greaterThan greaterThanEqual imulExtended inverse lessThan lessThanEqual matrixCompMult 
syn keyword         glslFunc                not notEqual outerProduct reflect refract transpose uaddCarry umulExtended usubBorrow 
syn keyword         glslFunc                textureSize textureQueryLod texture textureProj textureLod textureOffset texelFetch texelFetchOffset
syn keyword         glslFunc                textureProjOffset textureLodOffset textureProjLod textureProjLodOffset textureGrad textureGradOffset
syn keyword         glslFunc                textureProjGrad textureProjGradOffset textureGather textureGatherOffset textureGatherOffsets textureQueryLevels
syn keyword         glslFunc                dFdx dFdy fwidth interpolateAtCentroid interpolateAtOffset interpolateAtSample 
syn keyword         glslFunc                noise1 noise2 noise3 noise4
syn keyword         glslFunc                EmitStreamVertex EndStreamPrimitive EmitVertex EndPrimitive barrier
syn keyword         glslFunc                memoryBarrier memoryBarrierAtomicCounter memoryBarrierBuffer memoryBarrierImage memoryBarrierShared groupMemoryBarrier
syn keyword         glslFunc                imageSize
syn keyword         glslFunc                atomicAdd atomicMin atomicMax atomicAnd atomicOr atomicXor atomicExchange atomicCompSwap

syn keyword         glslFuncDeprec          ftransform
syn keyword         glslFuncDeprec          texture1D texture1DProj texture1DProj texture1DLod texture1DProjLod texture1DProjLod texture2D 
syn keyword         glslFuncDeprec          texture2DProj texture2DProj texture2DLod texture2DProjLod texture2DProjLod texture3D texture3DProj 
syn keyword         glslFuncDeprec          texture3DLod texture3DProjLod textureCube textureCubeLod shadow1D shadow2D shadow1DProj shadow2DProj 
syn keyword         glslFuncDeprec          shadow1DLod shadow2DLod shadow1DProjLod shadow2DProjLod


" Highlight unsupported keywords             
syn keyword         glslUnsupported         asm
syn keyword         glslUnsupported         class union enum typedef template this packed 
syn keyword         glslUnsupported         common partition active
syn keyword         glslUnsupported         filter 
syn keyword         glslUnsupported         goto 
syn keyword         glslUnsupported         hvec2 hvec3 hvec4 fvec2 fvec3 fvec4
syn keyword         glslUnsupported         iimage1D iimage2D iimage3D iimageCube 
syn keyword         glslUnsupported         iimage1DArray iimage2DArray uimage1DArray uimage2DArray
syn keyword         glslUnsupported         image1D image2D image3D imageCube 
syn keyword         glslUnsupported         image1DArray image2DArray 
syn keyword         glslUnsupported         image1DArrayShadow image2DArrayShadow
syn keyword         glslUnsupported         image1DShadow image2DShadow
syn keyword         glslUnsupported         imageBuffer iimageBuffer uimageBuffer
syn keyword         glslUnsupported         inline noinline volatile public static extern external interface
syn keyword         glslUnsupported         input output
syn keyword         glslUnsupported         long short half fixed unsigned superp
syn keyword         glslUnsupported         namespace using
syn keyword         glslUnsupported         sampler3DRect 
syn keyword         glslUnsupported         sizeof cast
syn keyword         glslUnsupported         uimage1D uimage2D uimage3D uimageCube 

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_glsl_syn_inits")
  if version < 508
    let did_glsl_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink glslCharacter                Character
  HiLink glslComment                  Comment
  HiLink glslComment2String           glslString
  HiLink glslCommentError             glslError
  HiLink glslCommentL                 glslComment
  HiLink glslCommentSkip              glslComment
  HiLink glslCommentStart             glslComment
  HiLink glslCommentStartError        glslError
  HiLink glslCommentString            glslString
  HiLink glslCppOut                   Comment
  HiLink glslCppOut2                  glslCppOut
  HiLink glslCppSkip                  glslCppOut
  HiLink glslCppString                glslString
  HiLink glslDefine                   Macro
  HiLink glslErrInBracket             glslError
  HiLink glslErrInParen               glslError
  HiLink glslError                    Error
  HiLink glslFloat                    Float
  HiLink glslFormat                   glslSpecial
  HiLink glslInclude                  Include
  HiLink glslIncluded                 glslString
  HiLink glslNumber                   Number
  HiLink glslOctal                    Number
  HiLink glslOctalError               glslError
  HiLink glslOctalZero                PreProc
  HiLink glslOperator                 Operator
  HiLink glslParenError               glslError
  HiLink glslPreCondit                PreCondit
  HiLink glslPreProc                  PreProc
  HiLink glslSpaceError               glslError
  HiLink glslSpecial                  SpecialChar
  HiLink glslSpecialCharacter         glslSpecial
  HiLink glslSpecialError             glslError
  HiLink glslString                   String

  HiLink glslConditional              Conditional
  HiLink glslConstant                 Constant
  HiLink glslFunc                     glslStatement
  HiLink glslFuncDeprec               WarningMsg
  HiLink glslLabel                    Label
  HiLink glslLayoutQual               glslFunc
  HiLink glslMemoryQual               glslFunc
  HiLink glslRepeat                   Repeat
  HiLink glslState                    glslType
  HiLink glslStateDeprec              WarningMsg
  HiLink glslStatement                Statement
  HiLink glslStorageClass             StorageClass
  HiLink glslLayout                   StorageClass
  HiLink glslStorageClassDeprec       WarningMsg
  HiLink glslStructure                Structure
  HiLink glslSwizzle                  SpecialChar
  HiLink glslTodo                     Todo
  HiLink glslType                     Type
  HiLink glslUniform                  glslType
  HiLink glslUniformDeprec            WarningMsg
  HiLink glslUnsupported              glslError
  HiLink glslUserLabel                Label

  delcommand HiLink
endif

let b:current_syntax = "glsl430"

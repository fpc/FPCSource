{
  $Id$

  Adaption of the delphi3d.net OpenGL units to FreePascal
  Sebastian Guenther (sg@freepascal.org) in 2002
  These units are free to use
}

{$MACRO ON}
{$MODE Delphi}
{$IFDEF Win32}
  {$DEFINE extdecl:=stdcall }
{$ELSE}
  {$DEFINE extdecl:=cdecl }
{$ENDIF}

unit GLext;

{******************************************************************************}
{ OpenGL Extension Header for Delphi.					       }
{ Written by Tom Nuydens (tom@delphi3d.net)				       }
{ For updates, visit http://www.delphi3d.net				       }
{									       }
{ This unit declares constants and function prototypes for all OpenGL	       }
{ extensions known to man.						       }
{									       }
{ To use an extension, call glext_ExtensionSupported() to find out if that     }
{ extension is supported by the user's ICD. If so, use wglGetProcAddress() to  }
{ load the extension's new functions (if any). The functions have already been }
{ declared in this unit, so there is no need to copy/paste prototypes from the }
{ extension specs or declare the function pointers yourself. The same goes for }
{ constants that are introduced by the extension.			       }
{									       }
{ If you find typos or other mistakes in this unit, please let me know and I   }
{ will correct them ASAP. Also, if you should come across an extension that    }
{ isn't yet included in this unit, tell me and I will add it.		       }
{******************************************************************************}

{ REVISIONS:
  - June 8, 2001         : WGL_EXT_swap_control
		           wglAllocateMemoryNV, wglFreeMemoryNV (for VAR)
  - June 13, 2001        : Forgot to add GL_ prefix to the NV_fence tokens (?).
  - June 22, 2001        : Fixed typo in glGenProgramsNV() declaration.
  - August 14, 2001      : Added WGL_ARB_multisample.
  - August 20, 2001      : ARB_texture_env_combine
                           ARB_texture_env_dot3.
  - September 10, 2001   : SGIX_texture_coordinate_clamp
                           OML_interlace
                           OML_subsample
                           OML_resample
                           WGL_OML_sync_control
                           NV_copy_depth_to_color
                           ATI_envmap_bumpmap
                           ATI_fragment_shader
                           ATI_pn_triangles
                           ATI_vertex_array_object
                           EXT_vertex_shader
                           ATI_vertex_streams
  - September 14, 2001   : OpenGL 1.3
  - February 25, 2002    : NV_depth_clamp
                           NV_multisample_filter_hint
                           NV_occlusion_query
                           NV_point_sprite
                           NV_texture_shader3
                           WGL_ARB_render_texture
                           WGL_NV_render_depth_texture
                           WGL_NV_render_texture_rectangle
                           Added optional "searchIn" argument to
                             glext_ExtensionSupported() function. This allows
                             you to manually specify a string in which to search
                             for an extension name. The intended use for this is
                             to check for WGL extensions, which have a separate
                             extension string.
  - March 13, 2002       : Fixed some omissions in the OpenGL 1.2 section
  - April 19, 2002       : Fixed typos in WGL_ARB_pixel_format
  - May 17, 2002         : ARB_vertex_blend
                           ARB_matrix_palette
                           ARB_texture_mirrored_repeat
                           ARB_depth_texture
                           ARB_shadow
                           ARB_shadow_ambient
                           ARB_window_pos
  - August 29, 2002      : ARB_vertex_program
                           EXT_stencil_two_side
                           NV_element_array
                           NV_float_buffer
                           NV_fragment_program
}

interface

uses
  SysUtils,
  {$IFDEF Win32}
  Windows,
  {$ELSE}
  DLLFuncs,
  {$ENDIF}
  GL;

{******************************************************************************}

{$IFDEF Win32}
// Not declared in Windows.pas.
function wglGetProcAddress(proc: PChar): Pointer; extdecl; external 'OpenGL32.dll';
{$ELSE}
function wglGetProcAddress(proc: PChar): Pointer;
{$ENDIF}

// Check if the given extension is supported. Case sensitive!
function glext_ExtensionSupported(const extension: String): Boolean;
function glext_ExtensionSupported(const extension: String; const searchIn: String): Boolean;

{******************************************************************************}

//*** OpenGL 1.2
const
  GL_CONSTANT_COLOR		    = $8001;
  GL_ONE_MINUS_CONSTANT_COLOR	    = $8002;
  GL_CONSTANT_ALPHA		    = $8003;
  GL_ONE_MINUS_CONSTANT_ALPHA	    = $8004;
  GL_BLEND_COLOR		    = $8005;
  GL_FUNC_ADD			    = $8006;
  GL_MIN			    = $8007;
  GL_MAX			    = $8008;
  GL_BLEND_EQUATION		    = $8009;
  GL_FUNC_SUBTRACT		    = $800A;
  GL_FUNC_REVERSE_SUBTRACT	    = $800B;
  GL_CONVOLUTION_1D		    = $8010;
  GL_CONVOLUTION_2D		    = $8011;
  GL_SEPARABLE_2D		    = $8012;
  GL_CONVOLUTION_BORDER_MODE	    = $8013;
  GL_CONVOLUTION_FILTER_SCALE	    = $8014;
  GL_CONVOLUTION_FILTER_BIAS	    = $8015;
  GL_REDUCE			    = $8016;
  GL_CONVOLUTION_FORMAT 	    = $8017;
  GL_CONVOLUTION_WIDTH		    = $8018;
  GL_CONVOLUTION_HEIGHT 	    = $8019;
  GL_MAX_CONVOLUTION_WIDTH	    = $801A;
  GL_MAX_CONVOLUTION_HEIGHT	    = $801B;
  GL_POST_CONVOLUTION_RED_SCALE     = $801C;
  GL_POST_CONVOLUTION_GREEN_SCALE   = $801D;
  GL_POST_CONVOLUTION_BLUE_SCALE    = $801E;
  GL_POST_CONVOLUTION_ALPHA_SCALE   = $801F;
  GL_POST_CONVOLUTION_RED_BIAS	    = $8020;
  GL_POST_CONVOLUTION_GREEN_BIAS    = $8021;
  GL_POST_CONVOLUTION_BLUE_BIAS     = $8022;
  GL_POST_CONVOLUTION_ALPHA_BIAS    = $8023;
  GL_HISTOGRAM			    = $8024;
  GL_PROXY_HISTOGRAM		    = $8025;
  GL_HISTOGRAM_WIDTH		    = $8026;
  GL_HISTOGRAM_FORMAT		    = $8027;
  GL_HISTOGRAM_RED_SIZE 	    = $8028;
  GL_HISTOGRAM_GREEN_SIZE	    = $8029;
  GL_HISTOGRAM_BLUE_SIZE	    = $802A;
  GL_HISTOGRAM_ALPHA_SIZE	    = $802B;
  GL_HISTOGRAM_LUMINANCE_SIZE	    = $802C;
  GL_HISTOGRAM_SINK		    = $802D;
  GL_MINMAX			    = $802E;
  GL_MINMAX_FORMAT		    = $802F;
  GL_MINMAX_SINK		    = $8030;
  GL_TABLE_TOO_LARGE		    = $8031;
  GL_UNSIGNED_BYTE_3_3_2	    = $8032;
  GL_UNSIGNED_SHORT_4_4_4_4	    = $8033;
  GL_UNSIGNED_SHORT_5_5_5_1	    = $8034;
  GL_UNSIGNED_INT_8_8_8_8	    = $8035;
  GL_UNSIGNED_INT_10_10_10_2	    = $8036;
  GL_RESCALE_NORMAL		    = $803A;
  GL_UNSIGNED_BYTE_2_3_3_REV	    = $8362;
  GL_UNSIGNED_SHORT_5_6_5	    = $8363;
  GL_UNSIGNED_SHORT_5_6_5_REV	    = $8364;
  GL_UNSIGNED_SHORT_4_4_4_4_REV     = $8365;
  GL_UNSIGNED_SHORT_1_5_5_5_REV     = $8366;
  GL_UNSIGNED_INT_8_8_8_8_REV	    = $8367;
  GL_UNSIGNED_INT_2_10_10_10_REV    = $8368;
  GL_COLOR_MATRIX		    = $80B1;
  GL_COLOR_MATRIX_STACK_DEPTH	    = $80B2;
  GL_MAX_COLOR_MATRIX_STACK_DEPTH   = $80B3;
  GL_POST_COLOR_MATRIX_RED_SCALE    = $80B4;
  GL_POST_COLOR_MATRIX_GREEN_SCALE  = $80B5;
  GL_POST_COLOR_MATRIX_BLUE_SCALE   = $80B6;
  GL_POST_COLOR_MATRIX_ALPHA_SCALE  = $80B7;
  GL_POST_COLOR_MATRIX_RED_BIAS     = $80B8;
  GL_POST_COLOR_MATRIX_GREEN_BIAS   = $80B9;
  GL_POST_COLOR_MATRIX_BLUE_BIAS    = $80BA;
  GL_COLOR_TABLE		    = $80D0;
  GL_POST_CONVOLUTION_COLOR_TABLE   = $80D1;
  GL_POST_COLOR_MATRIX_COLOR_TABLE  = $80D2;
  GL_PROXY_COLOR_TABLE		    = $80D3;
  GL_PROXY_POST_CONVOLUTION_COLOR_TABLE = $80D4;
  GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE = $80D5;
  GL_COLOR_TABLE_SCALE		    = $80D6;
  GL_COLOR_TABLE_BIAS		    = $80D7;
  GL_COLOR_TABLE_FORMAT 	    = $80D8;
  GL_COLOR_TABLE_WIDTH		    = $80D9;
  GL_COLOR_TABLE_RED_SIZE	    = $80DA;
  GL_COLOR_TABLE_GREEN_SIZE	    = $80DB;
  GL_COLOR_TABLE_BLUE_SIZE	    = $80DC;
  GL_COLOR_TABLE_ALPHA_SIZE	    = $80DD;
  GL_COLOR_TABLE_LUMINANCE_SIZE     = $80DE;
  GL_COLOR_TABLE_INTENSITY_SIZE     = $80DF;
  GL_CLAMP_TO_EDGE		    = $812F;
  GL_TEXTURE_MIN_LOD		    = $813A;
  GL_TEXTURE_MAX_LOD		    = $813B;
  GL_TEXTURE_BASE_LEVEL 	    = $813C;
  GL_TEXTURE_MAX_LEVEL		    = $813D;
  GL_LIGHT_MODEL_COLOR_CONTROL      = $81F8;
  GL_SINGLE_COLOR                   = $81F9;
  GL_SEPARATE_SPECULAR_COLOR        = $81FA;
  GL_MAX_ELEMENTS_VERTICES          = $F0E8;
  GL_MAX_ELEMENTS_INDICES           = $F0E9;
  GL_POST_COLOR_MATRIX_ALPHA_BIAS   = $80BB;
  GL_BGR                            = $80E0;
  GL_BGRA                           = $80E1;

var
  glBlendColor: procedure(red, green, blue, alpha: GLclampf); extdecl;
  glBlendEquation: procedure(mode: GLenum); extdecl;
  glDrawRangeElements: procedure(mode: GLenum; start, aend: GLuint;
    count: GLsizei; atype: GLenum; const indices: Pointer); extdecl;
  glColorTable: procedure(target, internalformat: GLenum; width: GLsizei;
    format, atype: GLenum; const table: Pointer); extdecl;
  glColorTableParameterfv: procedure(target, pname: GLenum;
    const params: Pointer); extdecl;
  glColorTableParameteriv: procedure(target, pname: GLenum;
    const params: PGLint); extdecl;
  glCopyColorTable: procedure(target, internalformat: GLenum; x, y: GLint;
    width: GLsizei); extdecl;
  glGetColorTable: procedure(target, format, atype: GLenum; table: Pointer); extdecl;
  glGetColorTableParameterfv: procedure(target, pname: GLenum;
    params: PGLfloat); extdecl;
  glGetColorTableParameteriv: procedure(target, pname: GLenum; params: PGLint); extdecl;
  glColorSubTable: procedure(target: GLenum; start, count: GLsizei;
    format, atype: GLenum; const data: Pointer); extdecl;
  glCopyColorSubTable: procedure(target: GLenum; start: GLsizei; x, y: GLint;
    width: GLsizei); extdecl;
  glConvolutionFilter1D: procedure(target, internalformat: GLenum;
    width: GLsizei; format, atype: GLenum; const image: Pointer); extdecl;
  glConvolutionFilter2D: procedure(target, internalformat: GLenum;
    width, height: GLsizei; format, atype: GLenum; const image: Pointer); extdecl;
  glConvolutionParameterf: procedure(target, pname: GLenum; params: GLfloat); extdecl;
  glConvolutionParameterfv: procedure(target, pname: GLenum;
    const params: PGLfloat); extdecl;
  glConvolutionParameteri: procedure(target, pname: GLenum; params: GLint); extdecl;
  glConvolutionParameteriv: procedure(target, pname: GLenum;
    const params: PGLint); extdecl;
  glCopyConvolutionFilter1D: procedure(target, internalformat: GLenum;
    x, y: GLint; width: GLsizei); extdecl;
  glCopyConvolutionFilter2D: procedure(target, internalformat: GLenum;
    x, y: GLint; width, height: GLsizei); extdecl;
  glGetConvolutionFilter: procedure(target, format, atype: GLenum;
    image: Pointer); extdecl;
  glGetConvolutionParameterfv: procedure(target, pname: GLenum;
    params: PGLfloat); extdecl;
  glGetConvolutionParameteriv: procedure(target, pname: GLenum; params: PGLint); extdecl;
  glGetSeparableFilter: procedure(target, format, atype: GLenum;
    row, column, span: Pointer); extdecl;
  glSeparableFilter2D: procedure(target, internalformat: GLenum;
    width, height: GLsizei; format, atype: GLenum; const row, column: Pointer); extdecl;
  glGetHistogram: procedure(target: GLenum; reset: GLboolean;
    format, atype: GLenum; values: Pointer); extdecl;
  glGetHistogramParameterfv: procedure(target, pname: GLenum; params: Pointer); extdecl;
  glGetHistogramParameteriv: procedure(target, pname: GLenum; params: PGLint); extdecl;
  glGetMinMax: procedure(target: GLenum; reset: GLboolean;
    format, atype: GLenum; values: Pointer); extdecl;
  glGetMinMaxParameterfv: procedure(target, pname: GLenum; params: PGLfloat); extdecl;
  glGetMinMaxParameteriv: procedure(target, pname: GLenum; params: PGLint); extdecl;
  glHistogram: procedure(target: GLenum; width: GLsizei; internalformat: GLenum;
    sink: GLboolean); extdecl;
  glMinMax: procedure(target, internalformat: GLenum; sink: GLboolean); extdecl;
  glResetHistogram: procedure(target: GLenum); extdecl;
  glResetMinMax: procedure(target: GLenum); extdecl;
  glTexImage3D: procedure(target: GLenum; level, internalformat: GLint;
    width, height, depth: GLsizei; border: GLint; format, atype: GLenum;
    const pixels: Pointer); extdecl;
  glTexSubImage3D: procedure(target: GLenum;
    level, xoffset, yoffset, zoffset: GLint; width, height, depth: GLsizei;
    format, atype: GLenum; const pixels: Pointer); extdecl;
  glCopyTexSubImage3D: procedure(target: GLenum;
    level, xoffset, yoffset, zoffset, x, y: GLint; width, height: GLsizei); extdecl;

//*** OpenGL 1.3
const
  GL_COMPRESSED_ALPHA    	    = $84E9;
  GL_COMPRESSED_LUMINANCE    	    = $84EA;
  GL_COMPRESSED_LUMINANCE_ALPHA     = $84EB;
  GL_COMPRESSED_INTENSITY    	    = $84EC;
  GL_COMPRESSED_RGB     	    = $84ED;
  GL_COMPRESSED_RGBA    	    = $84EE;
  GL_TEXTURE_COMPRESSION_HINT       = $84EF;
  GL_TEXTURE_IMAGE_SIZE    	    = $86A0;
  GL_TEXTURE_COMPRESSED    	    = $86A1;
  GL_NUM_COMPRESSED_TEXTURE_FORMATS = $86A2;
  GL_COMPRESSED_TEXTURE_FORMATS     = $86A3;

var
  glCompressedTexImage3D: procedure (target: GLenum; level: GLint;
    internalformat: GLenum; width, height, depth: GLsizei; border: GLint;
    imageSize: GLsizei; const data: Pointer); extdecl;
  glCompressedTexImage2D: procedure (target: GLenum; level: GLint;
    internalformat: GLenum;width, height: GLsizei; border: GLint;
    imageSize: GLsizei; const data: Pointer); extdecl;
  glCompressedTexImage1D: procedure (target: GLenum; level: GLint;
    internalformat: GLenum; width: GLsizei; border: GLint; imageSize: GLsizei;
    const data: Pointer); extdecl;
  glCompressedTexSubImage3D: procedure (target: GLenum; level: GLint;
    xoffset, yoffset, zoffset: GLint; width, height, depth: GLsizei;
    format: GLenum; imageSize: GLsizei; const data: Pointer); extdecl;
  glCompressedTexSubImage2D: procedure (target: GLenum; level: GLint;
    xoffset, yoffset: GLint; width, height: GLsizei; format: GLenum;
    imageSize: GLsizei; const data: Pointer); extdecl;
  glCompressedTexSubImage1D: procedure (target: GLenum; level: GLint;
    xoffset: GLint; width: GLsizei; format: GLenum; imageSize: GLsizei;
    const data: Pointer); extdecl;
  glGetCompressedTexImage: procedure (target: GLenum; level: GLint;
    img: Pointer); extdecl;

const
  GL_NORMAL_MAP    		    = $8511;
  GL_REFLECTION_MAP     	    = $8512;
  GL_TEXTURE_CUBE_MAP    	    = $8513;
  GL_TEXTURE_BINDING_CUBE_MAP       = $8514;
  GL_TEXTURE_CUBE_MAP_POSITIVE_X    = $8515;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_X    = $8516;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Y    = $8517;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Y    = $8518;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Z    = $8519;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Z    = $851A;
  GL_PROXY_TEXTURE_CUBE_MAP         = $851B;
  GL_MAX_CUBE_MAP_TEXTURE_SIZE      = $851C;

const
  GL_MULTISAMPLE    		    = $809D;
  GL_SAMPLE_ALPHA_TO_COVERAGE       = $809E;
  GL_SAMPLE_ALPHA_TO_ONE    	    = $809F;
  GL_SAMPLE_COVERAGE    	    = $80A0;
  GL_SAMPLE_BUFFERS     	    = $80A8;
  GL_SAMPLES    		    = $80A9;
  GL_SAMPLE_COVERAGE_VALUE    	    = $80AA;
  GL_SAMPLE_COVERAGE_INVERT         = $80AB;
  GL_MULTISAMPLE_BIT    	    = $20000000;

var
  glSampleCoverage: procedure (value: GLclampf; invert: GLboolean); extdecl;
  glSamplePass: procedure (pass: GLenum); extdecl;

const
  GL_TEXTURE0    		    = $84C0;
  GL_TEXTURE1    		    = $84C1;
  GL_TEXTURE2    		    = $84C2;
  GL_TEXTURE3    		    = $84C3;
  GL_TEXTURE4    		    = $84C4;
  GL_TEXTURE5    		    = $84C5;
  GL_TEXTURE6    		    = $84C6;
  GL_TEXTURE7    		    = $84C7;
  GL_TEXTURE8    		    = $84C8;
  GL_TEXTURE9    		    = $84C9;
  GL_TEXTURE10    		    = $84CA;
  GL_TEXTURE11    		    = $84CB;
  GL_TEXTURE12    		    = $84CC;
  GL_TEXTURE13    		    = $84CD;
  GL_TEXTURE14    		    = $84CE;
  GL_TEXTURE15    		    = $84CF;
  GL_TEXTURE16    		    = $84D0;
  GL_TEXTURE17    		    = $84D1;
  GL_TEXTURE18    		    = $84D2;
  GL_TEXTURE19    		    = $84D3;
  GL_TEXTURE20    		    = $84D4;
  GL_TEXTURE21    		    = $84D5;
  GL_TEXTURE22    		    = $84D6;
  GL_TEXTURE23    		    = $84D7;
  GL_TEXTURE24    		    = $84D8;
  GL_TEXTURE25    		    = $84D9;
  GL_TEXTURE26    		    = $84DA;
  GL_TEXTURE27    		    = $84DB;
  GL_TEXTURE28    		    = $84DC;
  GL_TEXTURE29    		    = $84DD;
  GL_TEXTURE30    		    = $84DE;
  GL_TEXTURE31    		    = $84DF;
  GL_ACTIVE_TEXTURE     	    = $84E0;
  GL_CLIENT_ACTIVE_TEXTURE    	    = $84E1;
  GL_MAX_TEXTURE_UNITS    	    = $84E2;

var
  glActiveTexture: procedure(texture: GLenum); extdecl;
  glClientActiveTexture: procedure(texture: GLenum); extdecl;
  glMultiTexCoord1d: procedure(target: GLenum; s: GLdouble); extdecl;
  glMultiTexCoord1dv: procedure(target: GLenum; const v: PGLdouble); extdecl;
  glMultiTexCoord1f: procedure(target: GLenum; s: GLfloat); extdecl;
  glMultiTexCoord1fv: procedure(target: GLenum; const v: PGLfloat); extdecl;
  glMultiTexCoord1i: procedure(target: GLenum; s: GLint); extdecl;
  glMultiTexCoord1iv: procedure(target: GLenum; const v: PGLint); extdecl;
  glMultiTexCoord1s: procedure(target: GLenum; s: GLshort); extdecl;
  glMultiTexCoord1sv: procedure(target: GLenum; const v: PGLshort); extdecl;
  glMultiTexCoord2d: procedure(target: GLenum; s, t: GLdouble); extdecl;
  glMultiTexCoord2dv: procedure(target: GLenum; const v: PGLdouble); extdecl;
  glMultiTexCoord2f: procedure(target: GLenum; s, t: GLfloat); extdecl;
  glMultiTexCoord2fv: procedure(target: GLenum; const v: PGLfloat); extdecl;
  glMultiTexCoord2i: procedure(target: GLenum; s, t: GLint); extdecl;
  glMultiTexCoord2iv: procedure(target: GLenum; const v: PGLint); extdecl;
  glMultiTexCoord2s: procedure(target: GLenum; s, t: GLshort); extdecl;
  glMultiTexCoord2sv: procedure(target: GLenum; const v: PGLshort); extdecl;
  glMultiTexCoord3d: procedure(target: GLenum; s, t, r: GLdouble); extdecl;
  glMultiTexCoord3dv: procedure(target: GLenum; const v: PGLdouble); extdecl;
  glMultiTexCoord3f: procedure(target: GLenum; s, t, r: GLfloat); extdecl;
  glMultiTexCoord3fv: procedure(target: GLenum; const v: PGLfloat); extdecl;
  glMultiTexCoord3i: procedure(target: GLenum; s, t, r: GLint); extdecl;
  glMultiTexCoord3iv: procedure(target: GLenum; const v: PGLint); extdecl;
  glMultiTexCoord3s: procedure(target: GLenum; s, t, r: GLshort); extdecl;
  glMultiTexCoord3sv: procedure(target: GLenum; const v: PGLshort); extdecl;
  glMultiTexCoord4d: procedure(target: GLenum; s, t, r, q: GLdouble); extdecl;
  glMultiTexCoord4dv: procedure(target: GLenum; const v: PGLdouble); extdecl;
  glMultiTexCoord4f: procedure(target: GLenum; s, t, r, q: GLfloat); extdecl;
  glMultiTexCoord4fv: procedure(target: GLenum; const v: PGLfloat); extdecl;
  glMultiTexCoord4i: procedure(target: GLenum; s, t, r, q: GLint); extdecl;
  glMultiTexCoord4iv: procedure(target: GLenum; const v: PGLint); extdecl;
  glMultiTexCoord4s: procedure(target: GLenum; s, t, r, q: GLshort); extdecl;
  glMultiTexCoord4sv: procedure(target: GLenum; const v: PGLshort); extdecl;

const
  GL_COMBINE                        = $8570;
  GL_COMBINE_RGB                    = $8571;
  GL_COMBINE_ALPHA                  = $8572;
  GL_SOURCE0_RGB                    = $8580;
  GL_SOURCE1_RGB                    = $8581;
  GL_SOURCE2_RGB                    = $8582;
  GL_SOURCE0_ALPHA                  = $8588;
  GL_SOURCE1_ALPHA                  = $8589;
  GL_SOURCE2_ALPHA                  = $858A;
  GL_OPERAND0_RGB                   = $8590;
  GL_OPERAND1_RGB                   = $8591;
  GL_OPERAND2_RGB                   = $8592;
  GL_OPERAND0_ALPHA                 = $8598;
  GL_OPERAND1_ALPHA                 = $8599;
  GL_OPERAND2_ALPHA                 = $859A;
  GL_RGB_SCALE                      = $8573;
  GL_ADD_SIGNED                     = $8574;
  GL_INTERPOLATE                    = $8575;
  GL_SUBTRACT                       = $84E7;
  GL_CONSTANT                       = $8576;
  GL_PRIMARY_COLOR                  = $8577;
  GL_PREVIOUS                       = $8578;

const
  GL_DOT3_RGB                       = $86AE;
  GL_DOT3_RGBA                      = $86AF;

const
  GL_CLAMP_TO_BORDER    	    = $812D;

const
  GL_TRANSPOSE_MODELVIEW_MATRIX     = $84E3;
  GL_TRANSPOSE_PROJECTION_MATRIX    = $84E4;
  GL_TRANSPOSE_TEXTURE_MATRIX       = $84E5;
  GL_TRANSPOSE_COLOR_MATRIX         = $84E6;

var
  glLoadTransposeMatrixf: procedure(const m: PGLfloat); extdecl;
  glLoadTransposeMatrixd: procedure(const m: PGLdouble); extdecl;
  glMultTransposeMatrixf: procedure(const m: PGLfloat); extdecl;
  glMultTransposeMatrixd: procedure(const m: PGLdouble); extdecl;

//*** ARB_multitexture
const
  GL_TEXTURE0_ARB		    = $84C0;
  GL_TEXTURE1_ARB		    = $84C1;
  GL_TEXTURE2_ARB		    = $84C2;
  GL_TEXTURE3_ARB		    = $84C3;
  GL_TEXTURE4_ARB		    = $84C4;
  GL_TEXTURE5_ARB		    = $84C5;
  GL_TEXTURE6_ARB		    = $84C6;
  GL_TEXTURE7_ARB		    = $84C7;
  GL_TEXTURE8_ARB		    = $84C8;
  GL_TEXTURE9_ARB		    = $84C9;
  GL_TEXTURE10_ARB		    = $84CA;
  GL_TEXTURE11_ARB		    = $84CB;
  GL_TEXTURE12_ARB		    = $84CC;
  GL_TEXTURE13_ARB		    = $84CD;
  GL_TEXTURE14_ARB		    = $84CE;
  GL_TEXTURE15_ARB		    = $84CF;
  GL_TEXTURE16_ARB		    = $84D0;
  GL_TEXTURE17_ARB		    = $84D1;
  GL_TEXTURE18_ARB		    = $84D2;
  GL_TEXTURE19_ARB		    = $84D3;
  GL_TEXTURE20_ARB		    = $84D4;
  GL_TEXTURE21_ARB		    = $84D5;
  GL_TEXTURE22_ARB		    = $84D6;
  GL_TEXTURE23_ARB		    = $84D7;
  GL_TEXTURE24_ARB		    = $84D8;
  GL_TEXTURE25_ARB		    = $84D9;
  GL_TEXTURE26_ARB		    = $84DA;
  GL_TEXTURE27_ARB		    = $84DB;
  GL_TEXTURE28_ARB		    = $84DC;
  GL_TEXTURE29_ARB		    = $84DD;
  GL_TEXTURE30_ARB		    = $84DE;
  GL_TEXTURE31_ARB		    = $84DF;
  GL_ACTIVE_TEXTURE_ARB 	    = $84E0;
  GL_CLIENT_ACTIVE_TEXTURE_ARB	    = $84E1;
  GL_MAX_TEXTURE_UNITS_ARB	    = $84E2;

var
  glActiveTextureARB: procedure(texture: GLenum); extdecl;
  glClientActiveTextureARB: procedure(texture: GLenum); extdecl;
  glMultiTexCoord1dARB: procedure(target: GLenum; s: GLdouble); extdecl;
  glMultiTexCoord1dvARB: procedure(target: GLenum; const v: PGLdouble); extdecl;
  glMultiTexCoord1fARB: procedure(target: GLenum; s: GLfloat); extdecl;
  glMultiTexCoord1fvARB: procedure(target: GLenum; const v: PGLfloat); extdecl;
  glMultiTexCoord1iARB: procedure(target: GLenum; s: GLint); extdecl;
  glMultiTexCoord1ivARB: procedure(target: GLenum; const v: PGLint); extdecl;
  glMultiTexCoord1sARB: procedure(target: GLenum; s: GLshort); extdecl;
  glMultiTexCoord1svARB: procedure(target: GLenum; const v: PGLshort); extdecl;
  glMultiTexCoord2dARB: procedure(target: GLenum; s, t: GLdouble); extdecl;
  glMultiTexCoord2dvARB: procedure(target: GLenum; const v: PGLdouble); extdecl;
  glMultiTexCoord2fARB: procedure(target: GLenum; s, t: GLfloat); extdecl;
  glMultiTexCoord2fvARB: procedure(target: GLenum; const v: PGLfloat); extdecl;
  glMultiTexCoord2iARB: procedure(target: GLenum; s, t: GLint); extdecl;
  glMultiTexCoord2ivARB: procedure(target: GLenum; const v: PGLint); extdecl;
  glMultiTexCoord2sARB: procedure(target: GLenum; s, t: GLshort); extdecl;
  glMultiTexCoord2svARB: procedure(target: GLenum; const v: PGLshort); extdecl;
  glMultiTexCoord3dARB: procedure(target: GLenum; s, t, r: GLdouble); extdecl;
  glMultiTexCoord3dvARB: procedure(target: GLenum; const v: PGLdouble); extdecl;
  glMultiTexCoord3fARB: procedure(target: GLenum; s, t, r: GLfloat); extdecl;
  glMultiTexCoord3fvARB: procedure(target: GLenum; const v: PGLfloat); extdecl;
  glMultiTexCoord3iARB: procedure(target: GLenum; s, t, r: GLint); extdecl;
  glMultiTexCoord3ivARB: procedure(target: GLenum; const v: PGLint); extdecl;
  glMultiTexCoord3sARB: procedure(target: GLenum; s, t, r: GLshort); extdecl;
  glMultiTexCoord3svARB: procedure(target: GLenum; const v: PGLshort); extdecl;
  glMultiTexCoord4dARB: procedure(target: GLenum; s, t, r, q: GLdouble); extdecl;
  glMultiTexCoord4dvARB: procedure(target: GLenum; const v: PGLdouble); extdecl;
  glMultiTexCoord4fARB: procedure(target: GLenum; s, t, r, q: GLfloat); extdecl;
  glMultiTexCoord4fvARB: procedure(target: GLenum; const v: PGLfloat); extdecl;
  glMultiTexCoord4iARB: procedure(target: GLenum; s, t, r, q: GLint); extdecl;
  glMultiTexCoord4ivARB: procedure(target: GLenum; const v: PGLint); extdecl;
  glMultiTexCoord4sARB: procedure(target: GLenum; s, t, r, q: GLshort); extdecl;
  glMultiTexCoord4svARB: procedure(target: GLenum; const v: PGLshort); extdecl;

//*** ARB_transpose_matrix
const
  GL_TRANSPOSE_MODELVIEW_MATRIX_ARB = $84E3;
  GL_TRANSPOSE_PROJECTION_MATRIX_ARB = $84E4;
  GL_TRANSPOSE_TEXTURE_MATRIX_ARB   = $84E5;
  GL_TRANSPOSE_COLOR_MATRIX_ARB     = $84E6;

var
  glLoadTransposeMatrixfARB: procedure(const m: PGLfloat); extdecl;
  glLoadTransposeMatrixdARB: procedure(const m: PGLdouble); extdecl;
  glMultTransposeMatrixfARB: procedure(const m: PGLfloat); extdecl;
  glMultTransposeMatrixdARB: procedure(const m: PGLdouble); extdecl;

//*** ARB_multisample
const
  GL_MULTISAMPLE_ARB		    = $809D;
  GL_SAMPLE_ALPHA_TO_COVERAGE_ARB   = $809E;
  GL_SAMPLE_ALPHA_TO_ONE_ARB	    = $809F;
  GL_SAMPLE_COVERAGE_ARB	    = $80A0;
  GL_SAMPLE_BUFFERS_ARB 	    = $80A8;
  GL_SAMPLES_ARB		    = $80A9;
  GL_SAMPLE_COVERAGE_VALUE_ARB	    = $80AA;
  GL_SAMPLE_COVERAGE_INVERT_ARB     = $80AB;
  GL_MULTISAMPLE_BIT_ARB	    = $20000000;

var
  glSampleCoverageARB: procedure (value: GLclampf; invert: GLboolean); extdecl;
  glSamplePassARB: procedure (pass: GLenum); extdecl;

//*** ARB_texture_cube_map
const
  GL_NORMAL_MAP_ARB		    = $8511;
  GL_REFLECTION_MAP_ARB 	    = $8512;
  GL_TEXTURE_CUBE_MAP_ARB	    = $8513;
  GL_TEXTURE_BINDING_CUBE_MAP_ARB   = $8514;
  GL_TEXTURE_CUBE_MAP_POSITIVE_X_ARB = $8515;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_X_ARB = $8516;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Y_ARB = $8517;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_ARB = $8518;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Z_ARB = $8519;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_ARB = $851A;
  GL_PROXY_TEXTURE_CUBE_MAP_ARB     = $851B;
  GL_MAX_CUBE_MAP_TEXTURE_SIZE_ARB  = $851C;

//*** ARB_texture_compression
const
  GL_COMPRESSED_ALPHA_ARB	    = $84E9;
  GL_COMPRESSED_LUMINANCE_ARB	    = $84EA;
  GL_COMPRESSED_LUMINANCE_ALPHA_ARB = $84EB;
  GL_COMPRESSED_INTENSITY_ARB	    = $84EC;
  GL_COMPRESSED_RGB_ARB 	    = $84ED;
  GL_COMPRESSED_RGBA_ARB	    = $84EE;
  GL_TEXTURE_COMPRESSION_HINT_ARB   = $84EF;
  GL_TEXTURE_IMAGE_SIZE_ARB	    = $86A0;
  GL_TEXTURE_COMPRESSED_ARB	    = $86A1;
  GL_NUM_COMPRESSED_TEXTURE_FORMATS_ARB = $86A2;
  GL_COMPRESSED_TEXTURE_FORMATS_ARB = $86A3;

var
  glCompressedTexImage3DARB: procedure (target: GLenum; level: GLint;
    internalformat: GLenum; width, height, depth: GLsizei; border: GLint;
    imageSize: GLsizei; const data: Pointer); extdecl;
  glCompressedTexImage2DARB: procedure (target: GLenum; level: GLint;
    internalformat: GLenum;width, height: GLsizei; border: GLint;
    imageSize: GLsizei; const data: Pointer); extdecl;
  glCompressedTexImage1DARB: procedure (target: GLenum; level: GLint;
    internalformat: GLenum; width: GLsizei; border: GLint; imageSize: GLsizei;
    const data: Pointer); extdecl;
  glCompressedTexSubImage3DARB: procedure (target: GLenum; level: GLint;
    xoffset, yoffset, zoffset: GLint; width, height, depth: GLsizei;
    format: GLenum; imageSize: GLsizei; const data: Pointer); extdecl;
  glCompressedTexSubImage2DARB: procedure (target: GLenum; level: GLint;
    xoffset, yoffset: GLint; width, height: GLsizei; format: GLenum;
    imageSize: GLsizei; const data: Pointer); extdecl;
  glCompressedTexSubImage1DARB: procedure (target: GLenum; level: GLint;
    xoffset: GLint; width: GLsizei; format: GLenum; imageSize: GLsizei;
    const data: Pointer); extdecl;
  glGetCompressedTexImageARB: procedure (target: GLenum; level: GLint;
    img: Pointer); extdecl;

//*** ARB_texture_env_combine
const
  GL_COMBINE_ARB                    = $8570;
  GL_COMBINE_RGB_ARB                = $8571;
  GL_COMBINE_ALPHA_ARB              = $8572;
  GL_SOURCE0_RGB_ARB                = $8580;
  GL_SOURCE1_RGB_ARB                = $8581;
  GL_SOURCE2_RGB_ARB                = $8582;
  GL_SOURCE0_ALPHA_ARB              = $8588;
  GL_SOURCE1_ALPHA_ARB              = $8589;
  GL_SOURCE2_ALPHA_ARB              = $858A;
  GL_OPERAND0_RGB_ARB               = $8590;
  GL_OPERAND1_RGB_ARB               = $8591;
  GL_OPERAND2_RGB_ARB               = $8592;
  GL_OPERAND0_ALPHA_ARB             = $8598;
  GL_OPERAND1_ALPHA_ARB             = $8599;
  GL_OPERAND2_ALPHA_ARB             = $859A;
  GL_RGB_SCALE_ARB                  = $8573;
  GL_ADD_SIGNED_ARB                 = $8574;
  GL_INTERPOLATE_ARB                = $8575;
  GL_SUBTRACT_ARB                   = $84E7;
  GL_CONSTANT_ARB                   = $8576;
  GL_PRIMARY_COLOR_ARB              = $8577;
  GL_PREVIOUS_ARB                   = $8578;

//*** ARB_texture_env_dot3
const
  GL_DOT3_RGB_ARB                   = $86AE;
  GL_DOT3_RGBA_ARB                  = $86AF;

//*** EXT_abgr
const
  GL_ABGR_EXT			    = $8000;

//*** EXT_blend_color
const
  GL_CONSTANT_COLOR_EXT 	    = $8001;
  GL_ONE_MINUS_CONSTANT_COLOR_EXT   = $8002;
  GL_CONSTANT_ALPHA_EXT 	    = $8003;
  GL_ONE_MINUS_CONSTANT_ALPHA_EXT   = $8004;
  GL_BLEND_COLOR_EXT		    = $8005;

var
  glBlendColorEXT: procedure (red, green, blue, alpha: GLclampf); extdecl;

//*** EXT_polygon_offset
const
  GL_POLYGON_OFFSET_EXT 	    = $8037;
  GL_POLYGON_OFFSET_FACTOR_EXT	    = $8038;
  GL_POLYGON_OFFSET_BIAS_EXT	    = $8039;

var
  glPolygonOffsetEXT: procedure (factor, bias: GLfloat); extdecl;

//*** EXT_texture
const
  GL_ALPHA4_EXT 		    = $803B;
  GL_ALPHA8_EXT 		    = $803C;
  GL_ALPHA12_EXT		    = $803D;
  GL_ALPHA16_EXT		    = $803E;
  GL_LUMINANCE4_EXT		    = $803F;
  GL_LUMINANCE8_EXT		    = $8040;
  GL_LUMINANCE12_EXT		    = $8041;
  GL_LUMINANCE16_EXT		    = $8042;
  GL_LUMINANCE4_ALPHA4_EXT	    = $8043;
  GL_LUMINANCE6_ALPHA2_EXT	    = $8044;
  GL_LUMINANCE8_ALPHA8_EXT	    = $8045;
  GL_LUMINANCE12_ALPHA4_EXT	    = $8046;
  GL_LUMINANCE12_ALPHA12_EXT	    = $8047;
  GL_LUMINANCE16_ALPHA16_EXT	    = $8048;
  GL_INTENSITY_EXT		    = $8049;
  GL_INTENSITY4_EXT		    = $804A;
  GL_INTENSITY8_EXT		    = $804B;
  GL_INTENSITY12_EXT		    = $804C;
  GL_INTENSITY16_EXT		    = $804D;
  GL_RGB2_EXT			    = $804E;
  GL_RGB4_EXT			    = $804F;
  GL_RGB5_EXT			    = $8050;
  GL_RGB8_EXT			    = $8051;
  GL_RGB10_EXT			    = $8052;
  GL_RGB12_EXT			    = $8053;
  GL_RGB16_EXT			    = $8054;
  GL_RGBA2_EXT			    = $8055;
  GL_RGBA4_EXT			    = $8056;
  GL_RGB5_A1_EXT		    = $8057;
  GL_RGBA8_EXT			    = $8058;
  GL_RGB10_A2_EXT		    = $8059;
  GL_RGBA12_EXT 		    = $805A;
  GL_RGBA16_EXT 		    = $805B;
  GL_TEXTURE_RED_SIZE_EXT	    = $805C;
  GL_TEXTURE_GREEN_SIZE_EXT	    = $805D;
  GL_TEXTURE_BLUE_SIZE_EXT	    = $805E;
  GL_TEXTURE_ALPHA_SIZE_EXT	    = $805F;
  GL_TEXTURE_LUMINANCE_SIZE_EXT     = $8060;
  GL_TEXTURE_INTENSITY_SIZE_EXT     = $8061;
  GL_REPLACE_EXT		    = $8062;
  GL_PROXY_TEXTURE_1D_EXT	    = $8063;
  GL_PROXY_TEXTURE_2D_EXT	    = $8064;
  GL_TEXTURE_TOO_LARGE_EXT	    = $8065;

//*** EXT_texture3D
const
  GL_PACK_SKIP_IMAGES		    = $806B;
  GL_PACK_SKIP_IMAGES_EXT	    = $806B;
  GL_PACK_IMAGE_HEIGHT		    = $806C;
  GL_PACK_IMAGE_HEIGHT_EXT	    = $806C;
  GL_UNPACK_SKIP_IMAGES 	    = $806D;
  GL_UNPACK_SKIP_IMAGES_EXT	    = $806D;
  GL_UNPACK_IMAGE_HEIGHT	    = $806E;
  GL_UNPACK_IMAGE_HEIGHT_EXT	    = $806E;
  GL_TEXTURE_3D 		    = $806F;
  GL_TEXTURE_3D_EXT		    = $806F;
  GL_PROXY_TEXTURE_3D		    = $8070;
  GL_PROXY_TEXTURE_3D_EXT	    = $8070;
  GL_TEXTURE_DEPTH		    = $8071;
  GL_TEXTURE_DEPTH_EXT		    = $8071;
  GL_TEXTURE_WRAP_R		    = $8072;
  GL_TEXTURE_WRAP_R_EXT 	    = $8072;
  GL_MAX_3D_TEXTURE_SIZE	    = $8073;
  GL_MAX_3D_TEXTURE_SIZE_EXT	    = $8073;

var
  glTexImage3DEXT: procedure(target: GLenum; level: GLint;
    internalformat: GLenum; width, height, depth: GLsizei; border: GLint;
    format, atype: GLenum; const pixels: Pointer); extdecl;
  glTexSubImage3DEXT: procedure(target: GLenum;
    level, xoffset, yoffset, zoffset: GLint; width, height, depth: GLsizei;
    format, atype: GLenum; const pixels: Pointer); extdecl;

//*** SGIS_texture_filter4
const
  GL_FILTER4_SGIS		    = $8146;
  GL_TEXTURE_FILTER4_SIZE_SGIS	    = $8147;

var
  glGetTexFilterFuncSGIS: procedure(target, filter: GLenum; weights: PGLfloat); extdecl;
  glTexFilterFuncSGIS: procedure(target, filter: GLenum; n: GLsizei;
    const weights: PGLfloat); extdecl;

//*** EXT_subtexture
  glTexSubImage1DEXT: procedure(target: GLenum; level, xoffset: GLint;
    width: GLsizei; format, atype: GLenum; const pixels: Pointer); extdecl;
  glTexSubImage2DEXT: procedure(target: GLenum; level, xoffset, yoffset: GLint;
    width, height: GLsizei; format, atype: GLenum; const pixels: Pointer); extdecl;

//*** EXT_copy_texture
  glCopyTexImage1DEXT: procedure (target: GLenum; level: GLint;
    internalformat: GLenum; x, y: GLint; width: GLsizei; border: GLint); extdecl;
  glCopyTexImage2DEXT: procedure (target: GLenum; level: GLint;
    internalformat: GLenum; x, y: GLint; width, height: GLsizei; border: GLint); extdecl;
  glCopyTexSubImage1DEXT: procedure (target: GLenum; level: GLint;
    xoffset, x, y: GLint; width: GLsizei); extdecl;
  glCopyTexSubImage2DEXT: procedure (target: GLenum; level: GLint;
    xoffset, yoffset, x, y: GLint; width, height: GLsizei); extdecl;
  glCopyTexSubImage3DEXT: procedure (target: GLenum; level: GLint;
    xoffset, yoffset, zoffset, x, y: GLint; width, height: GLsizei); extdecl;

//*** EXT_histogram
const
  GL_HISTOGRAM_EXT		    = $8024;
  GL_PROXY_HISTOGRAM_EXT	    = $8025;
  GL_HISTOGRAM_WIDTH_EXT	    = $8026;
  GL_HISTOGRAM_FORMAT_EXT	    = $8027;
  GL_HISTOGRAM_RED_SIZE_EXT	    = $8028;
  GL_HISTOGRAM_GREEN_SIZE_EXT	    = $8029;
  GL_HISTOGRAM_BLUE_SIZE_EXT	    = $802A;
  GL_HISTOGRAM_ALPHA_SIZE_EXT	    = $802B;
  GL_HISTOGRAM_LUMINANCE_SIZE_EXT   = $802C;
  GL_HISTOGRAM_SINK_EXT 	    = $802D;
  GL_MINMAX_EXT 		    = $802E;
  GL_MINMAX_FORMAT_EXT		    = $802F;
  GL_MINMAX_SINK_EXT		    = $8030;
  GL_TABLE_TOO_LARGE_EXT	    = $8031;

var
  glGetHistogramEXT: procedure(target: GLenum; reset: GLboolean;
    format, atype: GLenum; values: Pointer); extdecl;
  glGetHistogramParameterfvEXT: procedure(target, pname: GLenum;
    params: PGLfloat); extdecl;
  glGetHistogramParameterivEXT: procedure(target, pname: GLenum;
    params: PGLint); extdecl;
  glGetMinmaxEXT: procedure(target: GLenum; reset: GLboolean;
    format, atype: GLenum; values: Pointer); extdecl;
  glGetMinmaxParameterfvEXT: procedure(target, pname: GLenum; params: PGLfloat); extdecl;
  glGetMinmaxParameterivEXT: procedure(target, pname: GLenum; params: PGLint); extdecl;
  glHistogramEXT: procedure(target: GLenum; width: GLsizei;
    internalformat: GLenum; sink: GLboolean); extdecl;
  glMinmaxEXT: procedure(target, internalformat: GLenum; sink: GLboolean); extdecl;
  glResetHistogramEXT: procedure(target: GLenum); extdecl;
  glResetMinmaxEXT: procedure(target: GLenum); extdecl;

//*** EXT_convolution
const
  GL_CONVOLUTION_1D_EXT 	    = $8010;
  GL_CONVOLUTION_2D_EXT 	    = $8011;
  GL_SEPARABLE_2D_EXT		    = $8012;
  GL_CONVOLUTION_BORDER_MODE_EXT    = $8013;
  GL_CONVOLUTION_FILTER_SCALE_EXT   = $8014;
  GL_CONVOLUTION_FILTER_BIAS_EXT    = $8015;
  GL_REDUCE_EXT 		    = $8016;
  GL_CONVOLUTION_FORMAT_EXT	    = $8017;
  GL_CONVOLUTION_WIDTH_EXT	    = $8018;
  GL_CONVOLUTION_HEIGHT_EXT	    = $8019;
  GL_MAX_CONVOLUTION_WIDTH_EXT	    = $801A;
  GL_MAX_CONVOLUTION_HEIGHT_EXT     = $801B;
  GL_POST_CONVOLUTION_RED_SCALE_EXT = $801C;
  GL_POST_CONVOLUTION_GREEN_SCALE_EXT = $801D;
  GL_POST_CONVOLUTION_BLUE_SCALE_EXT = $801E;
  GL_POST_CONVOLUTION_ALPHA_SCALE_EXT = $801F;
  GL_POST_CONVOLUTION_RED_BIAS_EXT  = $8020;
  GL_POST_CONVOLUTION_GREEN_BIAS_EXT = $8021;
  GL_POST_CONVOLUTION_BLUE_BIAS_EXT = $8022;
  GL_POST_CONVOLUTION_ALPHA_BIAS_EXT = $8023;

var
  glConvolutionFilter1DEXT: procedure(target, internalformat: GLenum;
    width: GLsizei; format, atype: GLenum; const image: Pointer); extdecl;
  glConvolutionFilter2DEXT: procedure(target, internalformat: GLenum;
    width, height: GLsizei; format, atype: GLenum; const image: Pointer); extdecl;
  glConvolutionParameterfEXT: procedure(target, pname: GLenum; params: GLfloat); extdecl;
  glConvolutionParameterfvEXT: procedure(target, pname: GLenum; const params: PGLfloat); extdecl;
  glConvolutionParameteriEXT: procedure(target, pname: GLenum; params: GLint); extdecl;
  glConvolutionParameterivEXT: procedure(target, pname: GLenum; const params: PGLint); extdecl;
  glCopyConvolutionFilter1DEXT: procedure(target, internalformat: GLenum; x, y: GLint; width: GLsizei); extdecl;
  glCopyConvolutionFilter2DEXT: procedure(target, internalformat: GLenum; x, y: GLint; width, height: GLsizei); extdecl;
  glGetConvolutionFilterEXT: procedure(target, format, atype: GLenum; image: Pointer); extdecl;
  glGetConvolutionParameterfvEXT: procedure(target, pname: GLenum; params: PGLfloat); extdecl;
  glGetConvolutionParameterivEXT: procedure(target, pname: GLenum; params: PGLint); extdecl;
  glGetSeparableFilterEXT: procedure(target, format, atype: GLenum; row, column, span: Pointer); extdecl;
  glSeparableFilter2DEXT: procedure(target, internalformat: GLenum; width, height: GLsizei; format, atype: GLenum; const row, column: Pointer); extdecl;

//*** SGI_color_matrix
const
  GL_COLOR_MATRIX_SGI		    = $80B1;
  GL_COLOR_MATRIX_STACK_DEPTH_SGI   = $80B2;
  GL_MAX_COLOR_MATRIX_STACK_DEPTH_SGI = $80B3;
  GL_POST_COLOR_MATRIX_RED_SCALE_SGI = $80B4;
  GL_POST_COLOR_MATRIX_GREEN_SCALE_SGI = $80B5;
  GL_POST_COLOR_MATRIX_BLUE_SCALE_SGI = $80B6;
  GL_POST_COLOR_MATRIX_ALPHA_SCALE_SGI = $80B7;
  GL_POST_COLOR_MATRIX_RED_BIAS_SGI = $80B8;
  GL_POST_COLOR_MATRIX_GREEN_BIAS_SGI = $80B9;
  GL_POST_COLOR_MATRIX_BLUE_BIAS_SGI = $80BA;
  GL_POST_COLOR_MATRIX_ALPHA_BIAS_SGI = $80BB;

//*** SGI_color_table
const
  GL_COLOR_TABLE_SGI		    = $80D0;
  GL_POST_CONVOLUTION_COLOR_TABLE_SGI = $80D1;
  GL_POST_COLOR_MATRIX_COLOR_TABLE_SGI = $80D2;
  GL_PROXY_COLOR_TABLE_SGI	    = $80D3;
  GL_PROXY_POST_CONVOLUTION_COLOR_TABLE_SGI = $80D4;
  GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE_SGI = $80D5;
  GL_COLOR_TABLE_SCALE_SGI	    = $80D6;
  GL_COLOR_TABLE_BIAS_SGI	    = $80D7;
  GL_COLOR_TABLE_FORMAT_SGI	    = $80D8;
  GL_COLOR_TABLE_WIDTH_SGI	    = $80D9;
  GL_COLOR_TABLE_RED_SIZE_SGI	    = $80DA;
  GL_COLOR_TABLE_GREEN_SIZE_SGI     = $80DB;
  GL_COLOR_TABLE_BLUE_SIZE_SGI	    = $80DC;
  GL_COLOR_TABLE_ALPHA_SIZE_SGI     = $80DD;
  GL_COLOR_TABLE_LUMINANCE_SIZE_SGI = $80DE;
  GL_COLOR_TABLE_INTENSITY_SIZE_SGI = $80DF;

var
  glColorTableSGI: procedure(target, internalformat: GLenum; widthL: GLsizei;
    format, atype: GLenum; const table: Pointer); extdecl;
  glColorTableParameterfvSGI: procedure(target, pname: GLenum;
    const params: PGLfloat); extdecl;
  glColorTableParameterivSGI: procedure(target, pname: GLenum;
    const params: PGLint); extdecl;
  glCopyColorTableSGI: procedure(target, internamformat: GLenum; x, y: GLint;
    width: GLsizei); extdecl;
  glGetColorTableSGI: procedure(target, format, atype: GLenum; table: Pointer); extdecl;
  glGetColorTableParameterfvSGI: procedure(target, pname: GLenum;
    params: PGLfloat); extdecl;
  glGetColorTableParameterivSGI: procedure(target, pname: GLenum;
    params: PGLint); extdecl;

//*** SGIS_pixel_texture
const
  GL_PIXEL_TEXTURE_SGIS 	    = $8353;
  GL_PIXEL_FRAGMENT_RGB_SOURCE_SGIS = $8354;
  GL_PIXEL_FRAGMENT_ALPHA_SOURCE_SGIS = $8355;
  GL_PIXEL_GROUP_COLOR_SGIS	    = $8356;

var
  glPixelTexGenSGIX: procedure (mode: GLenum); extdecl;

//*** SGIX_pixel_texture
const
  GL_PIXEL_TEX_GEN_SGIX 	    = $8139;
  GL_PIXEL_TEX_GEN_MODE_SGIX	    = $832B;

var
  glPixelTexGenParameteriSGIS: procedure(pname: GLenum; param: GLint); extdecl;
  glPixelTexGenParameterivSGIS: procedure(pname: GLenum; const params: PGLint); extdecl;
  glPixelTexGenParameterfSGIS: procedure(pname: GLenum; param: GLfloat); extdecl;
  glPixelTexGenParameterfvSGIS: procedure(pname: GLenum;
    const params: PGLfloat); extdecl;
  glGetPixelTexGenParameterivSGIS: procedure(pname: GLenum; params: PGLint); extdecl;
  glGetPixelTexGenParameterfvSGIS: procedure(pname: GLenum; params: PGLfloat); extdecl;

//*** SGIS_texture4D
const
  GL_PACK_SKIP_VOLUMES_SGIS	    = $8130;
  GL_PACK_IMAGE_DEPTH_SGIS	    = $8131;
  GL_UNPACK_SKIP_VOLUMES_SGIS	    = $8132;
  GL_UNPACK_IMAGE_DEPTH_SGIS	    = $8133;
  GL_TEXTURE_4D_SGIS		    = $8134;
  GL_PROXY_TEXTURE_4D_SGIS	    = $8135;
  GL_TEXTURE_4DSIZE_SGIS	    = $8136;
  GL_TEXTURE_WRAP_Q_SGIS	    = $8137;
  GL_MAX_4D_TEXTURE_SIZE_SGIS	    = $8138;
  GL_TEXTURE_4D_BINDING_SGIS	    = $814F;

var
  glTexImage4DSGIS: procedure(target: GLenum; level: GLint;
    internalformat: GLenum; width, height, depth, size4d: GLsizei;
    border: GLint; format, atype: GLenum; const pixels: Pointer); extdecl;
  glTexSubImage4DSGIS: procedure(target: GLenum; level: GLint;
    xoffset, yoffset, zoffset, woffset: GLint;
    width, height, depth, size4d: GLsizei; format, atype: GLenum;
    const pixels: Pointer); extdecl;

//*** SGI_texture_color_table
const
  GL_TEXTURE_COLOR_TABLE_SGI	    = $80BC;
  GL_PROXY_TEXTURE_COLOR_TABLE_SGI  = $80BD;

//*** EXT_cmyka
const
  GL_CMYK_EXT			    = $800C;
  GL_CMYKA_EXT			    = $800D;
  GL_PACK_CMYK_HINT_EXT 	    = $800E;
  GL_UNPACK_CMYK_HINT_EXT	    = $800F;

//*** EXT_texture_object
const
  GL_TEXTURE_PRIORITY_EXT	    = $8066;
  GL_TEXTURE_RESIDENT_EXT	    = $8067;
  GL_TEXTURE_1D_BINDING_EXT	    = $8068;
  GL_TEXTURE_2D_BINDING_EXT	    = $8069;
  GL_TEXTURE_3D_BINDING_EXT	    = $806A;

var
  glAreTexturesResidentEXT: function(n: GLsizei; const textures: PGLuint;
    residences: PGLboolean): GLboolean; extdecl;
  glBindTextureEXT: procedure(target: GLenum; texture: GLuint); extdecl;
  glDeleteTexturesEXT: procedure(n: GLsizei; const textures: PGLuint); extdecl;
  glGenTexturesEXT: procedure(n: GLsizei; textures: PGLuint); extdecl;
  glIsTextureEXT: function(texture: GLuint): GLboolean; extdecl;
  glPrioritizeTexturesEXT: procedure(n: GLsizei; const textures: PGLuint;
    const priorities: PGLclampf); extdecl;

//*** SGIS_detail_texture
const
  GL_DETAIL_TEXTURE_2D_SGIS	    = $8095;
  GL_DETAIL_TEXTURE_2D_BINDING_SGIS = $8096;
  GL_LINEAR_DETAIL_SGIS 	    = $8097;
  GL_LINEAR_DETAIL_ALPHA_SGIS	    = $8098;
  GL_LINEAR_DETAIL_COLOR_SGIS	    = $8099;
  GL_DETAIL_TEXTURE_LEVEL_SGIS	    = $809A;
  GL_DETAIL_TEXTURE_MODE_SGIS	    = $809B;
  GL_DETAIL_TEXTURE_FUNC_POINTS_SGIS = $809C;

var
  glDetailTexFuncSGIS: procedure(target: GLenum; n: GLsizei;
    const points: PGLfloat); extdecl;
  glGetDetailTexFuncSGIS: procedure(target: GLenum; points: PGLfloat); extdecl;

//*** SGIS_sharpen_texture
const
  GL_LINEAR_SHARPEN_SGIS	    = $80AD;
  GL_LINEAR_SHARPEN_ALPHA_SGIS	    = $80AE;
  GL_LINEAR_SHARPEN_COLOR_SGIS	    = $80AF;
  GL_SHARPEN_TEXTURE_FUNC_POINTS_SGIS = $80B0;

var
  glSharpenTexFuncSGIS: procedure(target: GLenum; n: GLsizei;
    const points: PGLfloat); extdecl;
  glGetSharpenTexFuncSGIS: procedure(target: GLenum; points: PGLfloat); extdecl;

//*** EXT_packed_pixels
const
  GL_UNSIGNED_BYTE_3_3_2_EXT	    = $8032;
  GL_UNSIGNED_SHORT_4_4_4_4_EXT     = $8033;
  GL_UNSIGNED_SHORT_5_5_5_1_EXT     = $8034;
  GL_UNSIGNED_INT_8_8_8_8_EXT	    = $8035;
  GL_UNSIGNED_INT_10_10_10_2_EXT    = $8036;

//*** SGIS_texture_lod
const
  GL_TEXTURE_MIN_LOD_SGIS	    = $813A;
  GL_TEXTURE_MAX_LOD_SGIS	    = $813B;
  GL_TEXTURE_BASE_LEVEL_SGIS	    = $813C;
  GL_TEXTURE_MAX_LEVEL_SGIS	    = $813D;

//*** SGIS_multisample
const
  GL_MULTISAMPLE_SGIS		    = $809D;
  GL_SAMPLE_ALPHA_TO_MASK_SGIS	    = $809E;
  GL_SAMPLE_ALPHA_TO_ONE_SGIS	    = $809F;
  GL_SAMPLE_MASK_SGIS		    = $80A0;
  GL_1PASS_SGIS 		    = $80A1;
  GL_2PASS_0_SGIS		    = $80A2;
  GL_2PASS_1_SGIS		    = $80A3;
  GL_4PASS_0_SGIS		    = $80A4;
  GL_4PASS_1_SGIS		    = $80A5;
  GL_4PASS_2_SGIS		    = $80A6;
  GL_4PASS_3_SGIS		    = $80A7;
  GL_SAMPLE_BUFFERS_SGIS	    = $80A8;
  GL_SAMPLES_SGIS		    = $80A9;
  GL_SAMPLE_MASK_VALUE_SGIS	    = $80AA;
  GL_SAMPLE_MASK_INVERT_SGIS	    = $80AB;
  GL_SAMPLE_PATTERN_SGIS	    = $80AC;

var
  glSampleMaskSGIS: procedure(value: GLclampf; invert: GLboolean); extdecl;
  glSamplePatternSGIS: procedure(pattern: GLenum); extdecl;

//*** EXT_rescale_normal
const
  GL_RESCALE_NORMAL_EXT 	    = $803A;

//*** EXT_vertex_array
const
  GL_VERTEX_ARRAY_EXT		    = $8074;
  GL_NORMAL_ARRAY_EXT		    = $8075;
  GL_COLOR_ARRAY_EXT		    = $8076;
  GL_INDEX_ARRAY_EXT		    = $8077;
  GL_TEXTURE_COORD_ARRAY_EXT	    = $8078;
  GL_EDGE_FLAG_ARRAY_EXT	    = $8079;
  GL_VERTEX_ARRAY_SIZE_EXT	    = $807A;
  GL_VERTEX_ARRAY_TYPE_EXT	    = $807B;
  GL_VERTEX_ARRAY_STRIDE_EXT	    = $807C;
  GL_VERTEX_ARRAY_COUNT_EXT	    = $807D;
  GL_NORMAL_ARRAY_TYPE_EXT	    = $807E;
  GL_NORMAL_ARRAY_STRIDE_EXT	    = $807F;
  GL_NORMAL_ARRAY_COUNT_EXT	    = $8080;
  GL_COLOR_ARRAY_SIZE_EXT	    = $8081;
  GL_COLOR_ARRAY_TYPE_EXT	    = $8082;
  GL_COLOR_ARRAY_STRIDE_EXT	    = $8083;
  GL_COLOR_ARRAY_COUNT_EXT	    = $8084;
  GL_INDEX_ARRAY_TYPE_EXT	    = $8085;
  GL_INDEX_ARRAY_STRIDE_EXT	    = $8086;
  GL_INDEX_ARRAY_COUNT_EXT	    = $8087;
  GL_TEXTURE_COORD_ARRAY_SIZE_EXT   = $8088;
  GL_TEXTURE_COORD_ARRAY_TYPE_EXT   = $8089;
  GL_TEXTURE_COORD_ARRAY_STRIDE_EXT = $808A;
  GL_TEXTURE_COORD_ARRAY_COUNT_EXT  = $808B;
  GL_EDGE_FLAG_ARRAY_STRIDE_EXT     = $808C;
  GL_EDGE_FLAG_ARRAY_COUNT_EXT	    = $808D;
  GL_VERTEX_ARRAY_POINTER_EXT	    = $808E;
  GL_NORMAL_ARRAY_POINTER_EXT	    = $808F;
  GL_COLOR_ARRAY_POINTER_EXT	    = $8090;
  GL_INDEX_ARRAY_POINTER_EXT	    = $8091;
  GL_TEXTURE_COORD_ARRAY_POINTER_EXT = $8092;
  GL_EDGE_FLAG_ARRAY_POINTER_EXT    = $8093;

type
  PPointer = ^Pointer;

var
  glArrayElementEXT: procedure(i: GLint); extdecl;
  glColorPointerEXT: procedure(size: GLint; atype: GLenum;
    stride, count: GLsizei; const ptr: Pointer); extdecl;
  glDrawArraysEXT: procedure(mode: GLenum; first: GLint; count: GLsizei); extdecl;
  glEdgeFlagPointerEXT: procedure(stride, count: GLsizei;
    const ptr: PGLboolean); extdecl;
  glGetPointervEXT: procedure(pname: GLenum; params: PPointer); extdecl;
  glIndexPointerEXT: procedure(atype: GLenum; stride, count: GLsizei;
    const ptr: Pointer); extdecl;
  glNormalPointerEXT: procedure(atype: GLenum; stride, count: GLsizei;
    const ptr: Pointer); extdecl;
  glTexCoordPointerEXT: procedure(size: GLint; atype: GLenum;
    stride, count: GLsizei; const ptr: Pointer); extdecl;
  glVertexPointerEXT: procedure(size: GLint; atype: GLenum;
    stride, count: GLsizei; const ptr: Pointer); extdecl;

//*** EXT_misc_attribute

//*** SGIS_generate_mipmap
const
  GL_GENERATE_MIPMAP_SGIS	    = $8191;
  GL_GENERATE_MIPMAP_HINT_SGIS	    = $8192;

//*** SGIX_clipmap
const
  GL_LINEAR_CLIPMAP_LINEAR_SGIX     = $8170;
  GL_TEXTURE_CLIPMAP_CENTER_SGIX    = $8171;
  GL_TEXTURE_CLIPMAP_FRAME_SGIX     = $8172;
  GL_TEXTURE_CLIPMAP_OFFSET_SGIX    = $8173;
  GL_TEXTURE_CLIPMAP_VIRTUAL_DEPTH_SGIX = $8174;
  GL_TEXTURE_CLIPMAP_LOD_OFFSET_SGIX = $8175;
  GL_TEXTURE_CLIPMAP_DEPTH_SGIX     = $8176;
  GL_MAX_CLIPMAP_DEPTH_SGIX	    = $8177;
  GL_MAX_CLIPMAP_VIRTUAL_DEPTH_SGIX = $8178;
  GL_NEAREST_CLIPMAP_NEAREST_SGIX   = $844D;
  GL_NEAREST_CLIPMAP_LINEAR_SGIX    = $844E;
  GL_LINEAR_CLIPMAP_NEAREST_SGIX    = $844F;

//*** SGIX_shadow
const
  GL_TEXTURE_COMPARE_SGIX	    = $819A;
  GL_TEXTURE_COMPARE_OPERATOR_SGIX  = $819B;
  GL_TEXTURE_LEQUAL_R_SGIX	    = $819C;
  GL_TEXTURE_GEQUAL_R_SGIX	    = $819D;

//*** SGIS_texture_edge_clamp
const
  GL_CLAMP_TO_EDGE_SGIS 	    = $812F;

//*** SGIS_texture_border_clamp
const
  GL_CLAMP_TO_BORDER_SGIS	    = $812D;

//*** EXT_blend_minmax
const
  GL_FUNC_ADD_EXT		    = $8006;
  GL_MIN_EXT			    = $8007;
  GL_MAX_EXT			    = $8008;
  GL_BLEND_EQUATION_EXT 	    = $8009;

var
  glBlendEquationEXT: procedure(mode: GLenum); extdecl;

//*** EXT_blend_subtract
const
  GL_FUNC_SUBTRACT_EXT		    = $800A;
  GL_FUNC_REVERSE_SUBTRACT_EXT	    = $800B;

//*** EXT_blend_logic_op

//*** SGIX_interlace
const
  GL_INTERLACE_SGIX		    = $8094;

//*** SGIX_pixel_tiles
const
  GL_PIXEL_TILE_BEST_ALIGNMENT_SGIX = $813E;
  GL_PIXEL_TILE_CACHE_INCREMENT_SGIX = $813F;
  GL_PIXEL_TILE_WIDTH_SGIX	    = $8140;
  GL_PIXEL_TILE_HEIGHT_SGIX	    = $8141;
  GL_PIXEL_TILE_GRID_WIDTH_SGIX     = $8142;
  GL_PIXEL_TILE_GRID_HEIGHT_SGIX    = $8143;
  GL_PIXEL_TILE_GRID_DEPTH_SGIX     = $8144;
  GL_PIXEL_TILE_CACHE_SIZE_SGIX     = $8145;

//*** SGIS_texture_select
const
  GL_DUAL_ALPHA4_SGIS		    = $8110;
  GL_DUAL_ALPHA8_SGIS		    = $8111;
  GL_DUAL_ALPHA12_SGIS		    = $8112;
  GL_DUAL_ALPHA16_SGIS		    = $8113;
  GL_DUAL_LUMINANCE4_SGIS	    = $8114;
  GL_DUAL_LUMINANCE8_SGIS	    = $8115;
  GL_DUAL_LUMINANCE12_SGIS	    = $8116;
  GL_DUAL_LUMINANCE16_SGIS	    = $8117;
  GL_DUAL_INTENSITY4_SGIS	    = $8118;
  GL_DUAL_INTENSITY8_SGIS	    = $8119;
  GL_DUAL_INTENSITY12_SGIS	    = $811A;
  GL_DUAL_INTENSITY16_SGIS	    = $811B;
  GL_DUAL_LUMINANCE_ALPHA4_SGIS     = $811C;
  GL_DUAL_LUMINANCE_ALPHA8_SGIS     = $811D;
  GL_QUAD_ALPHA4_SGIS		    = $811E;
  GL_QUAD_ALPHA8_SGIS		    = $811F;
  GL_QUAD_LUMINANCE4_SGIS	    = $8120;
  GL_QUAD_LUMINANCE8_SGIS	    = $8121;
  GL_QUAD_INTENSITY4_SGIS	    = $8122;
  GL_QUAD_INTENSITY8_SGIS	    = $8123;
  GL_DUAL_TEXTURE_SELECT_SGIS	    = $8124;
  GL_QUAD_TEXTURE_SELECT_SGIS	    = $8125;

//*** SGIX_sprite
const
  GL_SPRITE_SGIX		    = $8148;
  GL_SPRITE_MODE_SGIX		    = $8149;
  GL_SPRITE_AXIS_SGIX		    = $814A;
  GL_SPRITE_TRANSLATION_SGIX	    = $814B;
  GL_SPRITE_AXIAL_SGIX		    = $814C;
  GL_SPRITE_OBJECT_ALIGNED_SGIX     = $814D;
  GL_SPRITE_EYE_ALIGNED_SGIX	    = $814E;

var
  glSpriteParameterfSGIX: procedure(pname: GLenum; param: GLfloat); extdecl;
  glSpriteParameterfvSGIX: procedure(pname: GLenum; const params: PGLfloat); extdecl;
  glSpriteParameteriSGIX: procedure(pname: GLenum; param: GLint); extdecl;
  glSpriteParameterivSGIX: procedure(pname: GLenum; const params: PGLint); extdecl;

//*** SGIX_texture_multi_buffer
const
  GL_TEXTURE_MULTI_BUFFER_HINT_SGIX = $812E;

//*** SGIS_point_parameters
const
  GL_POINT_SIZE_MIN_EXT 	    = $8126;
  GL_POINT_SIZE_MIN_SGIS	    = $8126;
  GL_POINT_SIZE_MAX_EXT 	    = $8127;
  GL_POINT_SIZE_MAX_SGIS	    = $8127;
  GL_POINT_FADE_THRESHOLD_SIZE_EXT  = $8128;
  GL_POINT_FADE_THRESHOLD_SIZE_SGIS = $8128;
  GL_DISTANCE_ATTENUATION_EXT	    = $8129;
  GL_DISTANCE_ATTENUATION_SGIS	    = $8129;

var
  glPointParameterfEXT: procedure(pname: GLenum; param: GLfloat); extdecl;
  glPointParameterfvEXT: procedure(pname: GLenum; const params: PGLfloat); extdecl;
  glPointParameterfSGIS: procedure(pname: GLenum; param: GLfloat); extdecl;
  glPointParameterfvSGIS: procedure(pname: GLenum; const params: PGLfloat); extdecl;

//*** SGIX_instruments
const
  GL_INSTRUMENT_BUFFER_POINTER_SGIX = $8180;
  GL_INSTRUMENT_MEASUREMENTS_SGIX   = $8181;

var
  glGetInstrumentsSGIX: function: GLint; extdecl;
  glInstrumentsBufferSGIX: procedure(size: GLsizei; buffer: PGLint); extdecl;
  glPollInstrumentsSGIX: function(marker_p: PGLint): GLint; extdecl;
  glReadInstrumentsSGIX: procedure(marker: GLint); extdecl;
  glStartInstrumentsSGIX: procedure; extdecl;
  glStopInstrumentsSGIX: procedure(marker: GLint); extdecl;

//*** SGIX_texture_scale_bias
const
  GL_POST_TEXTURE_FILTER_BIAS_SGIX  = $8179;
  GL_POST_TEXTURE_FILTER_SCALE_SGIX = $817A;
  GL_POST_TEXTURE_FILTER_BIAS_RANGE_SGIX = $817B;
  GL_POST_TEXTURE_FILTER_SCALE_RANGE_SGIX = $817C;

//*** SGIX_framezoom
const
  GL_FRAMEZOOM_SGIX		    = $818B;
  GL_FRAMEZOOM_FACTOR_SGIX	    = $818C;
  GL_MAX_FRAMEZOOM_FACTOR_SGIX	    = $818D;

var
  glFrameZoomSGIX: procedure(factor: GLint); extdecl;

//*** SGIX_tag_sample_buffer
var
  glTagSampleBufferSGIX: procedure; extdecl;

//*** SGIX_reference_plane
const
  GL_REFERENCE_PLANE_SGIX	    = $817D;
  GL_REFERENCE_PLANE_EQUATION_SGIX  = $817E;

var
  glReferencePlaneSGIX: procedure(const equation: PGLdouble); extdecl;

//*** SGIX_flush_raster
var
  glFlushRasterSGIX: procedure; extdecl;

//*** SGIX_depth_texture
const
  GL_DEPTH_COMPONENT16_SGIX	    = $81A5;
  GL_DEPTH_COMPONENT24_SGIX	    = $81A6;
  GL_DEPTH_COMPONENT32_SGIX	    = $81A7;

//*** SGIS_fog_function
const
  GL_FOG_FUNC_SGIS		    = $812A;
  GL_FOG_FUNC_POINTS_SGIS	    = $812B;
  GL_MAX_FOG_FUNC_POINTS_SGIS	    = $812C;

var
  glFogFuncSGIS: procedure(n: GLsizei; const points: PGLfloat); extdecl;
  glGetFogFuncSGIS: procedure(const points: PGLfloat); extdecl;

//*** SGIX_fog_offset
const
  GL_FOG_OFFSET_SGIX		    = $8198;
  GL_FOG_OFFSET_VALUE_SGIX	    = $8199;

//*** HP_image_transform
const
  GL_IMAGE_SCALE_X_HP		    = $8155;
  GL_IMAGE_SCALE_Y_HP		    = $8156;
  GL_IMAGE_TRANSLATE_X_HP	    = $8157;
  GL_IMAGE_TRANSLATE_Y_HP	    = $8158;
  GL_IMAGE_ROTATE_ANGLE_HP	    = $8159;
  GL_IMAGE_ROTATE_ORIGIN_X_HP	    = $815A;
  GL_IMAGE_ROTATE_ORIGIN_Y_HP	    = $815B;
  GL_IMAGE_MAG_FILTER_HP	    = $815C;
  GL_IMAGE_MIN_FILTER_HP	    = $815D;
  GL_IMAGE_CUBIC_WEIGHT_HP	    = $815E;
  GL_CUBIC_HP			    = $815F;
  GL_AVERAGE_HP 		    = $8160;
  GL_IMAGE_TRANSFORM_2D_HP	    = $8161;
  GL_POST_IMAGE_TRANSFORM_COLOR_TABLE_HP = $8162;
  GL_PROXY_POST_IMAGE_TRANSFORM_COLOR_TABLE_HP = $8163;

var
  glImageTransformParameteriHP: procedure(target, pname: GLenum; param: GLint); extdecl;
  glImageTransformParameterfHP: procedure(target, pname: GLenum;
    param: GLfloat); extdecl;
  glImageTransformParameterivHP: procedure(target, pname: GLenum;
    const params: PGLint); extdecl;
  glImageTransformParameterfvHP: procedure(target, pname: GLenum;
    const params: PGLfloat); extdecl;
  glGetImageTransformParameterivHP: procedure(target, pname: GLenum;
    params: PGLint); extdecl;
  glGetImageTransformParameterfvHP: procedure(target, pname: GLenum;
    params: PGLfloat); extdecl;

//*** HP_convolution_border_modes
const
  GL_IGNORE_BORDER_HP		    = $8150;
  GL_CONSTANT_BORDER_HP 	    = $8151;
  GL_REPLICATE_BORDER_HP	    = $8153;
  GL_CONVOLUTION_BORDER_COLOR_HP    = $8154;

//*** INGR_palette_buffer

//*** SGIX_texture_add_env
const
  GL_TEXTURE_ENV_BIAS_SGIX	    = $80BE;

//*** EXT_color_subtable
var
  glColorSubTableEXT: procedure(target: GLenum; start, count: GLsizei;
    format, atype: GLenum; const data: Pointer); extdecl;
  glCopyColorSubTableEXT: procedure(target: GLenum; start: GLsizei; x, y: GLint;
    width: GLsizei); extdecl;

//*** PGI_vertex_hints
const
  GL_VERTEX_DATA_HINT_PGI	    = $1A22A;
  GL_VERTEX_CONSISTENT_HINT_PGI     = $1A22B;
  GL_MATERIAL_SIDE_HINT_PGI	    = $1A22C;
  GL_MAX_VERTEX_HINT_PGI	    = $1A22D;
  GL_COLOR3_BIT_PGI		    = $00010000;
  GL_COLOR4_BIT_PGI		    = $00020000;
  GL_EDGEFLAG_BIT_PGI		    = $00040000;
  GL_INDEX_BIT_PGI		    = $00080000;
  GL_MAT_AMBIENT_BIT_PGI	    = $00100000;
  GL_MAT_AMBIENT_AND_DIFFUSE_BIT_PGI = $00200000;
  GL_MAT_DIFFUSE_BIT_PGI	    = $00400000;
  GL_MAT_EMISSION_BIT_PGI	    = $00800000;
  GL_MAT_COLOR_INDEXES_BIT_PGI	    = $01000000;
  GL_MAT_SHININESS_BIT_PGI	    = $02000000;
  GL_MAT_SPECULAR_BIT_PGI	    = $04000000;
  GL_NORMAL_BIT_PGI		    = $08000000;
  GL_TEXCOORD1_BIT_PGI		    = $10000000;
  GL_TEXCOORD2_BIT_PGI		    = $20000000;
  GL_TEXCOORD3_BIT_PGI		    = $40000000;
  GL_TEXCOORD4_BIT_PGI		    = $80000000;
  GL_VERTEX23_BIT_PGI		    = $00000004;
  GL_VERTEX4_BIT_PGI		    = $00000008;

//*** PGI_misc_hints
const
  GL_PREFER_DOUBLEBUFFER_HINT_PGI   = $1A1F8;
  GL_CONSERVE_MEMORY_HINT_PGI	    = $1A1FD;
  GL_RECLAIM_MEMORY_HINT_PGI	    = $1A1FE;
  GL_NATIVE_GRAPHICS_HANDLE_PGI     = $1A202;
  GL_NATIVE_GRAPHICS_BEGIN_HINT_PGI = $1A203;
  GL_NATIVE_GRAPHICS_END_HINT_PGI   = $1A204;
  GL_ALWAYS_FAST_HINT_PGI	    = $1A20C;
  GL_ALWAYS_SOFT_HINT_PGI	    = $1A20D;
  GL_ALLOW_DRAW_OBJ_HINT_PGI	    = $1A20E;
  GL_ALLOW_DRAW_WIN_HINT_PGI	    = $1A20F;
  GL_ALLOW_DRAW_FRG_HINT_PGI	    = $1A210;
  GL_ALLOW_DRAW_MEM_HINT_PGI	    = $1A211;
  GL_STRICT_DEPTHFUNC_HINT_PGI	    = $1A216;
  GL_STRICT_LIGHTING_HINT_PGI	    = $1A217;
  GL_STRICT_SCISSOR_HINT_PGI	    = $1A218;
  GL_FULL_STIPPLE_HINT_PGI	    = $1A219;
  GL_CLIP_NEAR_HINT_PGI 	    = $1A220;
  GL_CLIP_FAR_HINT_PGI		    = $1A221;
  GL_WIDE_LINE_HINT_PGI 	    = $1A222;
  GL_BACK_NORMALS_HINT_PGI	    = $1A223;

var
  glHintPGI: procedure(target: GLenum; mode: GLint); extdecl;

//*** EXT_paletted_texture
const
  GL_COLOR_INDEX1_EXT		    = $80E2;
  GL_COLOR_INDEX2_EXT		    = $80E3;
  GL_COLOR_INDEX4_EXT		    = $80E4;
  GL_COLOR_INDEX8_EXT		    = $80E5;
  GL_COLOR_INDEX12_EXT		    = $80E6;
  GL_COLOR_INDEX16_EXT		    = $80E7;
  GL_TEXTURE_INDEX_SIZE_EXT	    = $80ED;

var
  glColorTableEXT: procedure(target, internalformat: GLenum; width: GLsizei;
    format, atype: GLenum; const table: Pointer); extdecl;
  glGetColorTableEXT: procedure(target, format, atype: GLenum; data: Pointer); extdecl;
  glGetColorTableParameterivEXT: procedure(target, pname: GLenum;
    params: PGLint); extdecl;
  glGetColorTableParameterfvEXT: procedure(target, pname: GLenum;
    params: PGLfloat); extdecl;

//*** EXT_clip_volume_hint
const
  GL_CLIP_VOLUME_CLIPPING_HINT_EXT  = $80F0;

//*** SGIX_list_priority
const
  GL_LIST_PRIORITY_SGIX 	    = $8182;

var
  glGetListParameterfvSGIX: procedure(list: GLuint; pname: GLenum; params: PGLfloat); extdecl;
  glGetListParameterivSGIX: procedure(list: GLuint; pname: GLenum; params: PGLint); extdecl;
  glListParameterfSGIX: procedure(list: GLuint; pname: GLenum; param: GLfloat); extdecl;
  glListParameterfvSGIX: procedure(list: GLuint; pname: GLenum; const params: PGLfloat); extdecl;
  glListParameteriSGIX: procedure(list: GLuint; pname: GLenum; param: GLint); extdecl;
  glListParameterivSGIX: procedure(list: GLuint; pname: GLenum; const params: PGLint); extdecl;

//*** SGIX_ir_instrument1
const
  GL_IR_INSTRUMENT1_SGIX	    = $817F;

//*** SGIX_calligraphic_fragment
const
  GL_CALLIGRAPHIC_FRAGMENT_SGIX     = $8183;

//*** SGIX_texture_lod_bias
const
  GL_TEXTURE_LOD_BIAS_S_SGIX	    = $818E;
  GL_TEXTURE_LOD_BIAS_T_SGIX	    = $818F;
  GL_TEXTURE_LOD_BIAS_R_SGIX	    = $8190;

//*** SGIX_shadow_ambient
const
  GL_SHADOW_AMBIENT_SGIX	    = $80BF;

//*** EXT_index_texture

//*** EXT_index_material
const
  GL_INDEX_MATERIAL_EXT 	    = $81B8;
  GL_INDEX_MATERIAL_PARAMETER_EXT   = $81B9;
  GL_INDEX_MATERIAL_FACE_EXT	    = $81BA;

var
  glIndexMaterialEXT: procedure(face, mode: GLenum); extdecl;

//*** EXT_index_func
const
  GL_INDEX_TEST_EXT		    = $81B5;
  GL_INDEX_TEST_FUNC_EXT	    = $81B6;
  GL_INDEX_TEST_REF_EXT 	    = $81B7;

var
  glIndexFuncEXT: procedure(func: GLenum; ref: GLclampf); extdecl;

//*** EXT_index_array_formats
const
  GL_IUI_V2F_EXT		    = $81AD;
  GL_IUI_V3F_EXT		    = $81AE;
  GL_IUI_N3F_V2F_EXT		    = $81AF;
  GL_IUI_N3F_V3F_EXT		    = $81B0;
  GL_T2F_IUI_V2F_EXT		    = $81B1;
  GL_T2F_IUI_V3F_EXT		    = $81B2;
  GL_T2F_IUI_N3F_V2F_EXT	    = $81B3;
  GL_T2F_IUI_N3F_V3F_EXT	    = $81B4;

//*** EXT_compiled_vertex_array
const
  GL_ARRAY_ELEMENT_LOCK_FIRST_EXT   = $81A8;
  GL_ARRAY_ELEMENT_LOCK_COUNT_EXT   = $81A9;

var
  glLockArraysEXT: procedure(first: GLint; count: GLsizei); extdecl;
  glUnlockArraysEXT: procedure; extdecl;

//*** EXT_cull_vertex
const
  GL_CULL_VERTEX_EXT		    = $81AA;
  GL_CULL_VERTEX_EYE_POSITION_EXT   = $81AB;
  GL_CULL_VERTEX_OBJECT_POSITION_EXT = $81AC;

var
  glCullParameterdvEXT: procedure(pname: GLenum; params: PGLdouble); extdecl;
  glCullParameterfvEXT: procedure(pname: GLenum; params: PGLfloat); extdecl;

//*** SGIX_ycrcb
const
  GL_YCRCB_422_SGIX		    = $81BB;
  GL_YCRCB_444_SGIX		    = $81BC;

//*** SGIX_fragment_lighting
const
  GL_FRAGMENT_LIGHTING_SGIX	    = $8400;
  GL_FRAGMENT_COLOR_MATERIAL_SGIX   = $8401;
  GL_FRAGMENT_COLOR_MATERIAL_FACE_SGIX = $8402;
  GL_FRAGMENT_COLOR_MATERIAL_PARAMETER_SGIX = $8403;
  GL_MAX_FRAGMENT_LIGHTS_SGIX	    = $8404;
  GL_MAX_ACTIVE_LIGHTS_SGIX	    = $8405;
  GL_CURRENT_RASTER_NORMAL_SGIX     = $8406;
  GL_LIGHT_ENV_MODE_SGIX	    = $8407;
  GL_FRAGMENT_LIGHT_MODEL_LOCAL_VIEWER_SGIX = $8408;
  GL_FRAGMENT_LIGHT_MODEL_TWO_SIDE_SGIX = $8409;
  GL_FRAGMENT_LIGHT_MODEL_AMBIENT_SGIX = $840A;
  GL_FRAGMENT_LIGHT_MODEL_NORMAL_INTERPOLATION_SGIX = $840B;
  GL_FRAGMENT_LIGHT0_SGIX	    = $840C;
  GL_FRAGMENT_LIGHT1_SGIX	    = $840D;
  GL_FRAGMENT_LIGHT2_SGIX	    = $840E;
  GL_FRAGMENT_LIGHT3_SGIX	    = $840F;
  GL_FRAGMENT_LIGHT4_SGIX	    = $8410;
  GL_FRAGMENT_LIGHT5_SGIX	    = $8411;
  GL_FRAGMENT_LIGHT6_SGIX	    = $8412;
  GL_FRAGMENT_LIGHT7_SGIX	    = $8413;

var
  glFragmentColorMaterialSGIX: procedure(face, mode: GLenum); extdecl;
  glFragmentLightfSGIX: procedure(light, pname: GLenum; param: GLfloat); extdecl;
  glFragmentLightfvSGIX: procedure(light, pname: GLenum; const params: PGLfloat); extdecl;
  glFragmentLightiSGIX: procedure(light, pname: GLenum; param: GLint); extdecl;
  glFragmentLightivSGIX: procedure(light, pname: GLenum; const params: PGLint); extdecl;
  glFragmentLightModelfSGIX: procedure(pname: GLenum; param: GLfloat); extdecl;
  glFragmentLightModelfvSGIX: procedure(pname: GLenum; const params: PGLfloat); extdecl;
  glFragmentLightModeliSGIX: procedure(pname: GLenum; param: GLint); extdecl;
  glFragmentLightModelivSGIX: procedure(pname: GLenum; const params: PGLint); extdecl;
  glFragmentMaterialfSGIX: procedure(face, pname: GLenum; param: GLfloat); extdecl;
  glFragmentMaterialfvSGIX: procedure(face, pname: GLenum; const params: PGLfloat); extdecl;
  glFragmentMaterialiSGIX: procedure(face, pname: GLenum; param: GLint); extdecl;
  glFragmentMaterialivSGIX: procedure(face, pname: GLenum; const params: PGLint); extdecl;
  glGetFragmentLightfvSGIX: procedure(light, pname: GLenum; params: PGLfloat); extdecl;
  glGetFragmentLightivSGIX: procedure(light, pname: GLenum; params: PGLint); extdecl;
  glGetFragmentMaterialfvSGIX: procedure(light, pname: GLenum; params: PGLfloat); extdecl;
  glGetFragmentMaterialivSGIX: procedure(light, pname: GLenum; params: PGLint); extdecl;
  glLightEnviSGIX: procedure(pname: GLenum; param: GLint); extdecl;

//*** IBM_rasterpos_clip
const
  GL_RASTER_POSITION_UNCLIPPED_IBM  = $19262;

//*** HP_texture_lighting
const
  GL_TEXTURE_LIGHTING_MODE_HP	    = $8167;
  GL_TEXTURE_POST_SPECULAR_HP	    = $8168;
  GL_TEXTURE_PRE_SPECULAR_HP	    = $8169;

//*** EXT_draw_range_elements
const
  GL_MAX_ELEMENTS_VERTICES_EXT	    = $80E8;
  GL_MAX_ELEMENTS_INDICES_EXT	    = $80E9;

var
  glDrawRangeElementsEXT: procedure(mode: GLenum; start, aend: GLuint;
    count: GLsizei; atype: GLenum; const indices: Pointer); extdecl;

//*** WIN_phong_shading
const
  GL_PHONG_WIN			    = $80EA;
  GL_PHONG_HINT_WIN		    = $80EB;

//*** WIN_specular_fog
const
  GL_FOG_SPECULAR_TEXTURE_WIN	    = $80EC;

//*** EXT_light_texture
const
  GL_FRAGMENT_MATERIAL_EXT	    = $8349;
  GL_FRAGMENT_NORMAL_EXT	    = $834A;
  GL_FRAGMENT_COLOR_EXT 	    = $834C;
  GL_ATTENUATION_EXT		    = $834D;
  GL_SHADOW_ATTENUATION_EXT	    = $834E;
  GL_TEXTURE_APPLICATION_MODE_EXT   = $834F;
  GL_TEXTURE_LIGHT_EXT		    = $8350;
  GL_TEXTURE_MATERIAL_FACE_EXT	    = $8351;
  GL_TEXTURE_MATERIAL_PARAMETER_EXT = $8352;

var
  glApplyTextureEXT: procedure(mode: GLenum); extdecl;
  glTextureLightEXT: procedure(pname: GLenum); extdecl;
  glTextureMaterialEXT: procedure(face, mode: GLenum); extdecl;

//*** SGIX_blend_alpha_minmax
const
  GL_ALPHA_MIN_SGIX		    = $8320;
  GL_ALPHA_MAX_SGIX		    = $8321;

//*** EXT_bgra
const
  GL_BGR_EXT			    = $80E0;
  GL_BGRA_EXT			    = $80E1;

//*** INTEL_texture_scissor

//*** INTEL_parallel_arrays
const
  GL_PARALLEL_ARRAYS_INTEL	    = $83F4;
  GL_VERTEX_ARRAY_PARALLEL_POINTERS_INTEL = $83F5;
  GL_NORMAL_ARRAY_PARALLEL_POINTERS_INTEL = $83F6;
  GL_COLOR_ARRAY_PARALLEL_POINTERS_INTEL = $83F7;
  GL_TEXTURE_COORD_ARRAY_PARALLEL_POINTERS_INTEL = $83F8;

var
  glVertexPointervINTEL: procedure(size: GLint; atype: GLenum;
    const ptr: PPointer); extdecl;
  glNormalPointervINTEL: procedure(atype: GLenum; const ptr: PPointer); extdecl;
  glColorPointervINTEL: procedure(size: GLint; atype: GLenum;
    const ptr: PPointer); extdecl;
  glTexCoordPointervINTEL: procedure(size: GLint; atype: GLenum;
    const ptr: PPointer); extdecl;

//*** HP_occlusion_test
const
  GL_OCCLUSION_TEST_HP		    = $8165;
  GL_OCCLUSION_TEST_RESULT_HP	    = $8166;

//*** EXT_pixel_transform
const
  GL_PIXEL_TRANSFORM_2D_EXT	    = $8330;
  GL_PIXEL_MAG_FILTER_EXT	    = $8331;
  GL_PIXEL_MIN_FILTER_EXT	    = $8332;
  GL_PIXEL_CUBIC_WEIGHT_EXT	    = $8333;
  GL_CUBIC_EXT			    = $8334;
  GL_AVERAGE_EXT		    = $8335;
  GL_PIXEL_TRANSFORM_2D_STACK_DEPTH_EXT = $8336;
  GL_MAX_PIXEL_TRANSFORM_2D_STACK_DEPTH_EXT = $8337;
  GL_PIXEL_TRANSFORM_2D_MATRIX_EXT  = $8338;

var
  glPixelTransformParameteriEXT: procedure(target, pname: GLenum; param: GLint); extdecl;
  glPixelTransformParameterfEXT: procedure(target, pname: GLenum;
    param: GLfloat); extdecl;
  glPixelTransformParameterivEXT: procedure(target, pname: GLenum;
    const params: PGLint); extdecl;
  glPixelTransformParameterfvEXT: procedure(target, pname: GLenum;
    const params: PGLfloat); extdecl;

//*** EXT_pixel_transform_color_table

//*** EXT_shared_texture_palette
const
  GL_SHARED_TEXTURE_PALETTE_EXT     = $81FB;

//*** EXT_separate_specular_color
const
  GL_LIGHT_MODEL_COLOR_CONTROL_EXT  = $81F8;
  GL_SINGLE_COLOR_EXT		    = $81F9;
  GL_SEPARATE_SPECULAR_COLOR_EXT    = $81FA;

//*** EXT_secondary_color
const
  GL_COLOR_SUM_EXT		    = $8458;
  GL_CURRENT_SECONDARY_COLOR_EXT    = $8459;
  GL_SECONDARY_COLOR_ARRAY_SIZE_EXT = $845A;
  GL_SECONDARY_COLOR_ARRAY_TYPE_EXT = $845B;
  GL_SECONDARY_COLOR_ARRAY_STRIDE_EXT = $845C;
  GL_SECONDARY_COLOR_ARRAY_POINTER_EXT = $845D;
  GL_SECONDARY_COLOR_ARRAY_EXT	    = $845E;

var
  glSecondaryColor3bEXT: procedure(r, g, b: GLbyte); extdecl;
  glSecondaryColor3bvEXT: procedure(const v: PGLbyte); extdecl;
  glSecondaryColor3dEXT: procedure(r, g, b: GLdouble); extdecl;
  glSecondaryColor3dvEXT: procedure(const v: PGLdouble); extdecl;
  glSecondaryColor3fEXT: procedure(r, g, b: GLfloat); extdecl;
  glSecondaryColor3fvEXT: procedure(const v: PGLfloat); extdecl;
  glSecondaryColor3iEXT: procedure(r, g, b: GLint); extdecl;
  glSecondaryColor3ivEXT: procedure(const v: PGLint); extdecl;
  glSecondaryColor3sEXT: procedure(r, g, b: GLshort); extdecl;
  glSecondaryColor3svEXT: procedure(const v: PGLshort); extdecl;
  glSecondaryColor3ubEXT: procedure(r, g, b: GLubyte); extdecl;
  glSecondaryColor3ubvEXT: procedure(const v: PGLubyte); extdecl;
  glSecondaryColor3uiEXT: procedure(r, g, b: GLuint); extdecl;
  glSecondaryColor3uivEXT: procedure(const v: PGLuint); extdecl;
  glSecondaryColor3usEXT: procedure(r, g, b: GLushort); extdecl;
  glSecondaryColor3usvEXT: procedure(const v: PGLushort); extdecl;
  glSecondaryColorPointerEXT: procedure(size: GLint; atype: GLenum;
    stride: GLsizei; ptr: Pointer); extdecl;

//*** EXT_texture_perturb_normal
const
  GL_PERTURB_EXT		    = $85AE;
  GL_TEXTURE_NORMAL_EXT 	    = $85AF;

var
  glTextureNormalEXT: procedure(mode: GLenum); extdecl;

//*** EXT_multi_draw_arrays
var
  glMultiDrawArraysEXT: procedure(mode: GLenum; first: PGLint; count: PGLsizei;
    primcount: GLsizei); extdecl;
  glMultiDrawElementsEXT: procedure(mode: GLenum; const count: PGLsizei;
    atype: GLenum; const indices: PPointer; primcount: GLsizei); extdecl;

//*** EXT_fog_coord
const
  GL_FOG_COORDINATE_SOURCE_EXT	    = $8450;
  GL_FOG_COORDINATE_EXT 	    = $8451;
  GL_FRAGMENT_DEPTH_EXT 	    = $8452;
  GL_CURRENT_FOG_COORDINATE_EXT     = $8453;
  GL_FOG_COORDINATE_ARRAY_TYPE_EXT  = $8454;
  GL_FOG_COORDINATE_ARRAY_STRIDE_EXT = $8455;
  GL_FOG_COORDINATE_ARRAY_POINTER_EXT = $8456;
  GL_FOG_COORDINATE_ARRAY_EXT	    = $8457;

var
  glFogCoordfEXT: procedure(coord: GLfloat); extdecl;
  glFogCoordfvEXT: procedure(const coord: PGLfloat); extdecl;
  glFogCoorddEXT: procedure(coord: GLdouble); extdecl;
  glFogCoorddvEXT: procedure(const coord: PGLdouble); extdecl;
  glFogCoordPointerEXT: procedure(atype: GLenum; stride: GLsizei; const ptr: Pointer); extdecl;

//*** REND_screen_coordinates
const
  GL_SCREEN_COORDINATES_REND	    = $8490;
  GL_INVERTED_SCREEN_W_REND	    = $8491;

//*** EXT_coordinate_frame
const
  GL_TANGENT_ARRAY_EXT		    = $8439;
  GL_BINORMAL_ARRAY_EXT 	    = $843A;
  GL_CURRENT_TANGENT_EXT	    = $843B;
  GL_CURRENT_BINORMAL_EXT	    = $843C;
  GL_TANGENT_ARRAY_TYPE_EXT	    = $843E;
  GL_TANGENT_ARRAY_STRIDE_EXT	    = $843F;
  GL_BINORMAL_ARRAY_TYPE_EXT	    = $8440;
  GL_BINORMAL_ARRAY_STRIDE_EXT	    = $8441;
  GL_TANGENT_ARRAY_POINTER_EXT	    = $8442;
  GL_BINORMAL_ARRAY_POINTER_EXT     = $8443;
  GL_MAP1_TANGENT_EXT		    = $8444;
  GL_MAP2_TANGENT_EXT		    = $8445;
  GL_MAP1_BINORMAL_EXT		    = $8446;
  GL_MAP2_BINORMAL_EXT		    = $8447;

var
  glTangent3bEXT: procedure(tx, ty, tz: GLbyte); extdecl;
  glTangent3bvEXT: procedure(const v: PGLbyte); extdecl;
  glTangent3dEXT: procedure(tx, ty, tz: GLdouble); extdecl;
  glTangent3dvEXT: procedure(const v: PGLdouble); extdecl;
  glTangent3fEXT: procedure(tx, ty, tz: GLfloat); extdecl;
  glTangent3fvEXT: procedure(const v: PGLfloat); extdecl;
  glTangent3iEXT: procedure(tx, ty, tz: GLint); extdecl;
  glTangent3ivEXT: procedure(const v: PGLint); extdecl;
  glTangent3sEXT: procedure(tx, ty, tz: GLshort); extdecl;
  glTangent3svEXT: procedure(const v: PGLshort); extdecl;
  glBinormal3bEXT: procedure(bx, by, bz: GLbyte); extdecl;
  glBinormal3bvEXT: procedure(const v: PGLbyte); extdecl;
  glBinormal3dEXT: procedure(bx, by, bz: GLdouble); extdecl;
  glBinormal3dvEXT: procedure(const v: PGLdouble); extdecl;
  glBinormal3fEXT: procedure(bx, by, bz: GLfloat); extdecl;
  glBinormal3fvEXT: procedure(const v: PGLfloat); extdecl;
  glBinormal3iEXT: procedure(bx, by, bz: GLint); extdecl;
  glBinormal3ivEXT: procedure(const v: PGLint); extdecl;
  glBinormal3sEXT: procedure(bx, by, bz: GLshort); extdecl;
  glBinormal3svEXT: procedure(const v: PGLshort); extdecl;
  glTangentPointerEXT: procedure(atype: GLenum; stride: GLsizei; const ptr: Pointer); extdecl;
  glBinormalPointerEXT: procedure(atype: GLenum; stride: GLsizei; const ptr: Pointer); extdecl;

//*** EXT_texture_env_combine
const
  GL_COMBINE_EXT		    = $8570;
  GL_COMBINE_RGB_EXT		    = $8571;
  GL_COMBINE_ALPHA_EXT		    = $8572;
  GL_RGB_SCALE_EXT		    = $8573;
  GL_ADD_SIGNED_EXT		    = $8574;
  GL_INTERPOLATE_EXT		    = $8575;
  GL_CONSTANT_EXT		    = $8576;
  GL_PRIMARY_COLOR_EXT		    = $8577;
  GL_PREVIOUS_EXT		    = $8578;
  GL_SOURCE0_RGB_EXT		    = $8580;
  GL_SOURCE1_RGB_EXT		    = $8581;
  GL_SOURCE2_RGB_EXT		    = $8582;
  GL_SOURCE3_RGB_EXT		    = $8583;
  GL_SOURCE4_RGB_EXT		    = $8584;
  GL_SOURCE5_RGB_EXT		    = $8585;
  GL_SOURCE6_RGB_EXT		    = $8586;
  GL_SOURCE7_RGB_EXT		    = $8587;
  GL_SOURCE0_ALPHA_EXT		    = $8588;
  GL_SOURCE1_ALPHA_EXT		    = $8589;
  GL_SOURCE2_ALPHA_EXT		    = $858A;
  GL_SOURCE3_ALPHA_EXT		    = $858B;
  GL_SOURCE4_ALPHA_EXT		    = $858C;
  GL_SOURCE5_ALPHA_EXT		    = $858D;
  GL_SOURCE6_ALPHA_EXT		    = $858E;
  GL_SOURCE7_ALPHA_EXT		    = $858F;
  GL_OPERAND0_RGB_EXT		    = $8590;
  GL_OPERAND1_RGB_EXT		    = $8591;
  GL_OPERAND2_RGB_EXT		    = $8592;
  GL_OPERAND3_RGB_EXT		    = $8593;
  GL_OPERAND4_RGB_EXT		    = $8594;
  GL_OPERAND5_RGB_EXT		    = $8595;
  GL_OPERAND6_RGB_EXT		    = $8596;
  GL_OPERAND7_RGB_EXT		    = $8597;
  GL_OPERAND0_ALPHA_EXT 	    = $8598;
  GL_OPERAND1_ALPHA_EXT 	    = $8599;
  GL_OPERAND2_ALPHA_EXT 	    = $859A;
  GL_OPERAND3_ALPHA_EXT 	    = $859B;
  GL_OPERAND4_ALPHA_EXT 	    = $859C;
  GL_OPERAND5_ALPHA_EXT 	    = $859D;
  GL_OPERAND6_ALPHA_EXT 	    = $859E;
  GL_OPERAND7_ALPHA_EXT 	    = $859F;

//*** APPLE_specular_vector
const
  GL_LIGHT_MODEL_SPECULAR_VECTOR_APPLE = $85B0;

//*** APPLE_transform_hint
const
  GL_TRANSFORM_HINT_APPLE	    = $85B1;

//*** SGIX_fog_scale
const
  GL_FOG_SCALE_SGIX		    = $81FC;
  GL_FOG_SCALE_VALUE_SGIX	    = $81FD;

//*** SUNX_constant_data
const
  GL_UNPACK_CONSTANT_DATA_SUNX	    = $81D5;
  GL_TEXTURE_CONSTANT_DATA_SUNX     = $81D6;

var
  glFinishTextureSUNX: procedure; extdecl;

//*** SUN_global_alpha
const
  GL_GLOBAL_ALPHA_SUN		    = $81D9;
  GL_GLOBAL_ALPHA_FACTOR_SUN	    = $81DA;

var
  glGlobalAlphaFactorbSUN: procedure(factor: GLbyte); extdecl;
  glGlobalAlphaFactorsSUN: procedure(factor: GLshort); extdecl;
  glGlobalAlphaFactoriSUN: procedure(factor: GLint); extdecl;
  glGlobalAlphaFactorfSUN: procedure(factor: GLfloat); extdecl;
  glGlobalAlphaFactordSUN: procedure(factor: GLdouble); extdecl;
  glGlobalAlphaFactorubSUN: procedure(factor: GLubyte); extdecl;
  glGlobalAlphaFactorusSUN: procedure(factor: GLushort); extdecl;
  glGlobalAlphaFactoruiSUN: procedure(factor: GLuint); extdecl;

//*** SUN_triangle_list
const
  GL_RESTART_SUN		    = $01;
  GL_REPLACE_MIDDLE_SUN 	    = $02;
  GL_REPLACE_OLDEST_SUN 	    = $03;
  GL_TRIANGLE_LIST_SUN		    = $81D7;
  GL_REPLACEMENT_CODE_SUN	    = $81D8;
  GL_REPLACEMENT_CODE_ARRAY_SUN     = $85C0;
  GL_REPLACEMENT_CODE_ARRAY_TYPE_SUN = $85C1;
  GL_REPLACEMENT_CODE_ARRAY_STRIDE_SUN = $85C2;
  GL_REPLACEMENT_CODE_ARRAY_POINTER_SUN = $85C3;
  GL_R1UI_V3F_SUN		    = $85C4;
  GL_R1UI_C4UB_V3F_SUN		    = $85C5;
  GL_R1UI_C3F_V3F_SUN		    = $85C6;
  GL_R1UI_N3F_V3F_SUN		    = $85C7;
  GL_R1UI_C4F_N3F_V3F_SUN	    = $85C8;
  GL_R1UI_T2F_V3F_SUN		    = $85C9;
  GL_R1UI_T2F_N3F_V3F_SUN	    = $85CA;
  GL_R1UI_T2F_C4F_N3F_V3F_SUN	    = $85CB;

var
  glReplacementCodeuiSUN: procedure(code: GLuint); extdecl;
  glReplacementCodeusSUN: procedure(code: GLushort); extdecl;
  glReplacementCodeubSUN: procedure(code: GLubyte); extdecl;
  glReplacementCodeuivSUN: procedure(const code: PGLuint); extdecl;
  glReplacementCodeusvSUN: procedure(const code: PGLushort); extdecl;
  glReplacementCodeubvSUN: procedure(const code: PGLubyte); extdecl;
  glReplacementCodePointerSUN: procedure(atype: GLenum; stride: GLsizei; const ptr: Pointer); extdecl;

//*** SUN_vertex
var
  glColor4ubVertex2fSUN: procedure(r: GLubyte; g: GLubyte; b: GLubyte;
    a: GLubyte; x: GLfloat; y: GLfloat); extdecl;
  glColor4ubVertex2fvSUN: procedure(const c: PGLubyte; const v: PGLfloat); extdecl;
  glColor4ubVertex3fSUN: procedure(r: GLubyte; g: GLubyte; b: GLubyte;
    a: GLubyte; x: GLfloat; y: GLfloat; z: GLfloat); extdecl;
  glColor4ubVertex3fvSUN: procedure(const c: PGLubyte; const v: GLfloat); extdecl;
  glColor3fVertex3fSUN: procedure(r: GLfloat; g: GLfloat; b: GLfloat;
    x: GLfloat; y: GLfloat; z: GLfloat); extdecl;
  glColor3fVertex3fvSUN: procedure(const c: GLfloat; const v: GLfloat); extdecl;
  glNormal3fVertex3fSUN: procedure(nx: GLfloat; ny: GLfloat; nz: GLfloat;
    x: GLfloat; y: GLfloat; z: GLfloat); extdecl;
  glNormal3fVertex3fvSUN: procedure(const n: GLfloat; const v: GLfloat); extdecl;
  glColor4fNormal3fVertex3fSUN: procedure(r: GLfloat; g: GLfloat; b: GLfloat;
    a: GLfloat; nx: GLfloat; ny: GLfloat; nz: GLfloat; x: GLfloat; y: GLfloat;
      z:GLfloat); extdecl;
  glColor4fNormal3fVertex3fvSUN: procedure(const c: PGLfloat; const n: PGLfloat;
    const v: PGLfloat); extdecl;
  glTexCoord2fVertex3fSUN: procedure(s: GLfloat; t: GLfloat; x: GLfloat;
    y: GLfloat; z: GLfloat); extdecl;
  glTexCoord2fVertex3fvSUN: procedure(const t: PGLfloat; const v: PGLfloat); extdecl;
  glTexCoord4fVertex4fSUN: procedure(s: GLfloat; t: GLfloat; r: GLfloat;
    q: GLfloat; x: GLfloat; y: GLfloat; z: GLfloat; w: GLfloat); extdecl;
  glTexCoord4fVertex4fvSUN: procedure(const t: PGLfloat; const v: PGLfloat); extdecl;
  glTexCoord2fColor4ubVertex3fSUN: procedure(s: GLfloat; t: GLfloat; r: GLubyte;
    g: GLubyte; b: GLubyte; a: GLubyte; x: GLfloat; y: GLfloat; z: GLfloat); extdecl;
  glTexCoord2fColor4ubVertex3fvSUN: procedure(const t: PGLfloat;
    const c: PGLubyte; const v: PGLfloat); extdecl;
  glTexCoord2fColor3fVertex3fSUN: procedure(s: GLfloat; t: GLfloat; r: GLfloat;
    g: GLfloat; b: GLfloat; x: GLfloat; y: GLfloat; z: GLfloat); extdecl;
  glTexCoord2fColor3fVertex3fvSUN: procedure(const t: PGLfloat;
    const c: PGLfloat; const v: PGLfloat); extdecl;
  glTexCoord2fNormal3fVertex3fSUN: procedure(s: GLfloat; t: GLfloat;
    nx: GLfloat; ny: GLfloat; nz: GLfloat; x: GLfloat; y: GLfloat; z: GLfloat); extdecl;
  glTexCoord2fNormal3fVertex3fvSUN: procedure(const t: PGLfloat;
    const n: PGLfloat; const v: PGLfloat); extdecl;
  glTexCoord2fColor4fNormal3fVertex3fSUN: procedure(s: GLfloat; t: GLfloat;
    r: GLfloat; g: GLfloat; b: GLfloat; a: GLfloat; nx: GLfloat; ny: GLfloat;
      nz: GLfloat; x: GLfloat; y: GLfloat; z: GLfloat); extdecl;
  glTexCoord2fColor4fNormal3fVertex3fvSUN: procedure(const t: PGLfloat;
    const c: PGLfloat; const n: PGLfloat; const v: PGLfloat); extdecl;
  glTexCoord4fColor4fNormal3fVertex4fSUN: procedure(s: GLfloat; t: GLfloat;
    r: GLfloat; q: GLfloat; cr: GLfloat; cg: GLfloat; cb: GLfloat; ca: GLfloat;
    nx: GLfloat; ny: GLfloat; nz: GLfloat; x: GLfloat; y: GLfloat; z: GLfloat;
    w: GLfloat); extdecl;
  glTexCoord4fColor4fNormal3fVertex4fvSUN: procedure(const t: PGLfloat;
    const c: PGLfloat; const n: PGLfloat; const v: PGLfloat); extdecl;
  glReplacementCodeuiVertex3fSUN: procedure(rc: GLenum; x: GLfloat; y: GLfloat;
    z: GLfloat); extdecl;
  glReplacementCodeuiVertex3fvSUN: procedure(const rc: PGLenum;
    const v: PGLfloat); extdecl;
  glReplacementCodeuiColor4ubVertex3fSUN: procedure(rc: GLenum; r: GLubyte;
    g: GLubyte; b: GLubyte; a: GLubyte; x: GLfloat; y: GLfloat; z: GLfloat); extdecl;
  glReplacementCodeuiColor4ubVertex3fvSUN: procedure(const rc: PGLenum;
    const c: PGLubyte; const v: PGLfloat); extdecl;
  glReplacementCodeuiColor3fVertex3fSUN: procedure(rc: GLenum; r: GLfloat;
    g: GLfloat; b: GLfloat; x: GLfloat; y: GLfloat; z: GLfloat); extdecl;
  glReplacementCodeuiColor3fVertex3fvSUN: procedure(const rc: PGLenum;
    const c: PGLfloat; const v: PGLfloat); extdecl;
  glReplacementCodeuiNormal3fVertex3fSUN: procedure(rc: GLenum; nx: GLfloat;
    ny: GLfloat; nz: GLfloat; x: GLfloat; y: GLfloat; z: GLfloat); extdecl;
  glReplacementCodeuiNormal3fVertex3fvSUN: procedure(const rc: PGLenum;
    const n: PGLfloat; const v: PGLfloat); extdecl;
  glReplacementCodeuiColor4fNormal3fVertex3fSUN: procedure(rc: GLenum;
    r: GLfloat; g: GLfloat; b: GLfloat; a: GLfloat; nx: GLfloat; ny: GLfloat;
    nz: GLfloat; x: GLfloat; y: GLfloat; z: GLfloat); extdecl;
  glReplacementCodeuiColor4fNormal3fVertex3fvSUN: procedure(const rc: PGLenum;
    const c: PGLfloat; const n: PGLfloat; const v: PGLfloat); extdecl;
  glReplacementCodeuiTexCoord2fVertex3fSUN: procedure(rc: GLenum; s: GLfloat;
    t: GLfloat; x: GLfloat; y: GLfloat; z: GLfloat); extdecl;
  glReplacementCodeuiTexCoord2fVertex3fvSUN: procedure(const rc: PGLenum;
    const t: PGLfloat; const v: PGLfloat); extdecl;
  glReplacementCodeuiTexCoord2fNormal3fVertex3fSUN: procedure(rc: GLenum;
    s: GLfloat; t: GLfloat; nx: GLfloat; ny: GLfloat; nz: GLfloat; x: GLfloat;
    y: GLfloat; z: GLfloat); extdecl;
  glReplacementCodeuiTexCoord2fNormal3fVertex3fvSUN: procedure(
    const rc: PGLenum; const t: PGLfloat; const n: PGLfloat; const v: PGLfloat); extdecl;
  glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fSUN: procedure(rc: GLenum;
    s: GLfloat; t: GLfloat; r: GLfloat; g: GLfloat; b: GLfloat; a: GLfloat;
    nx: GLfloat; ny: GLfloat; nz: GLfloat; x: GLfloat; y: GLfloat; z: GLfloat); extdecl;
  glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fvSUN: procedure(
    const rc: PGLenum; const t: PGLfloat; const c: PGLfloat; const n: PGLfloat;
    const v: PGLfloat); extdecl;

//*** EXT_blend_func_separate
const
  GL_BLEND_DST_RGB_EXT		    = $80C8;
  GL_BLEND_SRC_RGB_EXT		    = $80C9;
  GL_BLEND_DST_ALPHA_EXT	    = $80CA;
  GL_BLEND_SRC_ALPHA_EXT	    = $80CB;

var
  glBlendFuncSeparateEXT: procedure(sfactorRGB, dfactorRGB, sfactorAlpha, dfactorAlpha: GLenum); extdecl;

//*** INGR_color_clamp
const
  GL_RED_MIN_CLAMP_INGR 	    = $8560;
  GL_GREEN_MIN_CLAMP_INGR	    = $8561;
  GL_BLUE_MIN_CLAMP_INGR	    = $8562;
  GL_ALPHA_MIN_CLAMP_INGR	    = $8563;
  GL_RED_MAX_CLAMP_INGR 	    = $8564;
  GL_GREEN_MAX_CLAMP_INGR	    = $8565;
  GL_BLUE_MAX_CLAMP_INGR	    = $8566;
  GL_ALPHA_MAX_CLAMP_INGR	    = $8567;

//*** INGR_interlace_read
const
  GL_INTERLACE_READ_INGR	    = $8568;

//*** EXT_stencil_wrap
const
  GL_INCR_WRAP_EXT		    = $8507;
  GL_DECR_WRAP_EXT		    = $8508;

//*** EXT_422_pixels
const
  GL_422_EXT			    = $80CC;
  GL_422_REV_EXT		    = $80CD;
  GL_422_AVERAGE_EXT		    = $80CE;
  GL_422_REV_AVERAGE_EXT	    = $80CF;

//*** NV_texgen_reflection
const
  GL_NORMAL_MAP_NV		    = $8511;
  GL_REFLECTION_MAP_NV		    = $8512;

//*** EXT_texture_cube_map
const
  GL_NORMAL_MAP_EXT		    = $8511;
  GL_REFLECTION_MAP_EXT 	    = $8512;
  GL_TEXTURE_CUBE_MAP_EXT	    = $8513;
  GL_TEXTURE_BINDING_CUBE_MAP_EXT   = $8514;
  GL_TEXTURE_CUBE_MAP_POSITIVE_X_EXT = $8515;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_X_EXT = $8516;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Y_EXT = $8517;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_EXT = $8518;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Z_EXT = $8519;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_EXT = $851A;
  GL_PROXY_TEXTURE_CUBE_MAP_EXT     = $851B;
  GL_MAX_CUBE_MAP_TEXTURE_SIZE_EXT  = $851C;

//*** SUN_convolution_border_modes
const
  GL_WRAP_BORDER_SUN		    = $81D4;

//*** EXT_texture_env_add

//*** EXT_texture_lod_bias
const
  GL_MAX_TEXTURE_LOD_BIAS_EXT	    = $84FD;
  GL_TEXTURE_FILTER_CONTROL_EXT     = $8500;
  GL_TEXTURE_LOD_BIAS_EXT	    = $8501;

//*** EXT_texture_filter_anisotropic
const
  GL_TEXTURE_MAX_ANISOTROPY_EXT     = $84FE;
  GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT = $84FF;

//*** EXT_vertex_weighting
const
  GL_MODELVIEW0_STACK_DEPTH_EXT     = GL_MODELVIEW_STACK_DEPTH;
  GL_MODELVIEW1_STACK_DEPTH_EXT     = $8502;
  GL_MODELVIEW0_MATRIX_EXT	    = GL_MODELVIEW_MATRIX;
  GL_MODELVIEW_MATRIX1_EXT	    = $8506;
  GL_VERTEX_WEIGHTING_EXT	    = $8509;
  GL_MODELVIEW0_EXT		    = GL_MODELVIEW;
  GL_MODELVIEW1_EXT		    = $850A;
  GL_CURRENT_VERTEX_WEIGHT_EXT	    = $850B;
  GL_VERTEX_WEIGHT_ARRAY_EXT	    = $850C;
  GL_VERTEX_WEIGHT_ARRAY_SIZE_EXT   = $850D;
  GL_VERTEX_WEIGHT_ARRAY_TYPE_EXT   = $850E;
  GL_VERTEX_WEIGHT_ARRAY_STRIDE_EXT = $850F;
  GL_VERTEX_WEIGHT_ARRAY_POINTER_EXT = $8510;

var
  glVertexWeightfEXT: procedure(weight: GLfloat); extdecl;
  glVertexWeightfvEXT: procedure(const weight: PGLfloat); extdecl;
  glVertexWeightPointerEXT: procedure(size: GLsizei; atype: GLenum;
    stride: GLsizei; const ptr: Pointer); extdecl;

//*** NV_light_max_exponent
const
  GL_MAX_SHININESS_NV		    = $8504;
  GL_MAX_SPOT_EXPONENT_NV	    = $8505;

//*** NV_vertex_array_range
const
  GL_VERTEX_ARRAY_RANGE_NV	    = $851D;
  GL_VERTEX_ARRAY_RANGE_LENGTH_NV   = $851E;
  GL_VERTEX_ARRAY_RANGE_VALID_NV    = $851F;
  GL_MAX_VERTEX_ARRAY_RANGE_ELEMENT_NV = $8520;
  GL_VERTEX_ARRAY_RANGE_POINTER_NV  = $8521;

var
  glFlushVertexArrayRangeNV: procedure; extdecl;
  glVertexArrayRangeNV: procedure(size: GLsizei; const ptr: Pointer); extdecl;
  {$IFDEF Win32}
  wglAllocateMemoryNV: function(size: GLsizei; readFrequency, writeFrequency,
				priority: Single): Pointer; extdecl;
  wglFreeMemoryNV: procedure(ptr: Pointer); extdecl;
  {$ENDIF}

//*** NV_register_combiners
const
  GL_REGISTER_COMBINERS_NV	    = $8522;
  GL_VARIABLE_A_NV		    = $8523;
  GL_VARIABLE_B_NV		    = $8524;
  GL_VARIABLE_C_NV		    = $8525;
  GL_VARIABLE_D_NV		    = $8526;
  GL_VARIABLE_E_NV		    = $8527;
  GL_VARIABLE_F_NV		    = $8528;
  GL_VARIABLE_G_NV		    = $8529;
  GL_CONSTANT_COLOR0_NV 	    = $852A;
  GL_CONSTANT_COLOR1_NV 	    = $852B;
  GL_PRIMARY_COLOR_NV		    = $852C;
  GL_SECONDARY_COLOR_NV 	    = $852D;
  GL_SPARE0_NV			    = $852E;
  GL_SPARE1_NV			    = $852F;
  GL_DISCARD_NV 		    = $8530;
  GL_E_TIMES_F_NV		    = $8531;
  GL_SPARE0_PLUS_SECONDARY_COLOR_NV = $8532;
  GL_UNSIGNED_IDENTITY_NV	    = $8536;
  GL_UNSIGNED_INVERT_NV 	    = $8537;
  GL_EXPAND_NORMAL_NV		    = $8538;
  GL_EXPAND_NEGATE_NV		    = $8539;
  GL_HALF_BIAS_NORMAL_NV	    = $853A;
  GL_HALF_BIAS_NEGATE_NV	    = $853B;
  GL_SIGNED_IDENTITY_NV 	    = $853C;
  GL_SIGNED_NEGATE_NV		    = $853D;
  GL_SCALE_BY_TWO_NV		    = $853E;
  GL_SCALE_BY_FOUR_NV		    = $853F;
  GL_SCALE_BY_ONE_HALF_NV	    = $8540;
  GL_BIAS_BY_NEGATIVE_ONE_HALF_NV   = $8541;
  GL_COMBINER_INPUT_NV		    = $8542;
  GL_COMBINER_MAPPING_NV	    = $8543;
  GL_COMBINER_COMPONENT_USAGE_NV    = $8544;
  GL_COMBINER_AB_DOT_PRODUCT_NV     = $8545;
  GL_COMBINER_CD_DOT_PRODUCT_NV     = $8546;
  GL_COMBINER_MUX_SUM_NV	    = $8547;
  GL_COMBINER_SCALE_NV		    = $8548;
  GL_COMBINER_BIAS_NV		    = $8549;
  GL_COMBINER_AB_OUTPUT_NV	    = $854A;
  GL_COMBINER_CD_OUTPUT_NV	    = $854B;
  GL_COMBINER_SUM_OUTPUT_NV	    = $854C;
  GL_MAX_GENERAL_COMBINERS_NV	    = $854D;
  GL_NUM_GENERAL_COMBINERS_NV	    = $854E;
  GL_COLOR_SUM_CLAMP_NV 	    = $854F;
  GL_COMBINER0_NV		    = $8550;
  GL_COMBINER1_NV		    = $8551;
  GL_COMBINER2_NV		    = $8552;
  GL_COMBINER3_NV		    = $8553;
  GL_COMBINER4_NV		    = $8554;
  GL_COMBINER5_NV		    = $8555;
  GL_COMBINER6_NV		    = $8556;
  GL_COMBINER7_NV		    = $8557;

var
  glCombinerParameterfvNV: procedure(pname: GLenum; const params: PGLfloat); extdecl;
  glCombinerParameterfNV: procedure(pname: GLenum; param: GLfloat); extdecl;
  glCombinerParameterivNV: procedure(pname: GLenum; const params: PGLint); extdecl;
  glCombinerParameteriNV: procedure(pname: GLenum; param: GLint); extdecl;
  glCombinerInputNV: procedure(
    stage, portion, variable, input, mapping, componentUsage: GLenum); extdecl;
  glCombinerOutputNV: procedure(
    stage, portion, abOutput, cdOutput, sumOutput, scale, bias: GLenum;
    abDotProduct, cdDotProduct, muxSum: GLboolean); extdecl;
  glFinalCombinerInputNV: procedure(
    variable, input, mapping, componentUsage: GLenum); extdecl;
  glGetCombinerInputParameterfvNV: procedure(
    stage, portion, variable, pname: GLenum; params: PGLfloat); extdecl;
  glGetCombinerInputParameterivNV: procedure(
    stage, portion, variable, pname: GLenum; params: PGLint); extdecl;
  glGetCombinerOutputParameterfvNV: procedure(stage, portion, pname: GLenum;
    params: PGLfloat); extdecl;
  glGetCombinerOutputParameterivNV: procedure(stage, portion, pname: GLenum;
    params: PGLint); extdecl;
  glGetFinalCombinerInputParameterfvNV: procedure( variable, pname: GLenum;
    params: PGLfloat); extdecl;
  glGetFinalCombinerInputParameterivNV: procedure(variable, pname: GLenum;
    params: PGLint); extdecl;

//*** NV_fog_distance
const
  GL_FOG_DISTANCE_MODE_NV	    = $855A;
  GL_EYE_RADIAL_NV		    = $855B;
  GL_EYE_PLANE_ABSOLUTE_NV	    = $855C;

//*** NV_texgen_emboss
const
  GL_EMBOSS_LIGHT_NV		    = $855D;
  GL_EMBOSS_CONSTANT_NV 	    = $855E;
  GL_EMBOSS_MAP_NV		    = $855F;

//*** NV_blend_square

//*** NV_texture_env_combine4
const
  GL_COMBINE4_NV		    = $8503;
  GL_SOURCE3_RGB_NV		    = $8583;
  GL_SOURCE3_ALPHA_NV		    = $858B;
  GL_OPERAND3_RGB_NV		    = $8593;
  GL_OPERAND3_ALPHA_NV		    = $859B;

//*** MESA_resize_buffers
var
  glResizeBuffersMESA: procedure; extdecl;

//*** MESA_window_pos
var
  glWindowPos2dMESA: procedure(x, y: GLdouble); extdecl;
  glWindowPos2dvMESA: procedure(const v: PGLdouble); extdecl;
  glWindowPos2fMESA: procedure(x, y: GLfloat); extdecl;
  glWindowPos2fvMESA: procedure(const v: PGLfloat); extdecl;
  glWindowPos2iMESA: procedure(x, y: GLint); extdecl;
  glWindowPos2ivMESA: procedure(const v: PGLint); extdecl;
  glWindowPos2sMESA: procedure(x, y: GLshort); extdecl;
  glWindowPos2svMESA: procedure(const v: PGLshort); extdecl;
  glWindowPos3dMESA: procedure(x, y, z: GLdouble); extdecl;
  glWindowPos3dvMESA: procedure(const v: PGLdouble); extdecl;
  glWindowPos3fMESA: procedure(x, y, z: GLfloat); extdecl;
  glWindowPos3fvMESA: procedure(const v: PGLfloat); extdecl;
  glWindowPos3iMESA: procedure(x, y, z: GLint); extdecl;
  glWindowPos3ivMESA: procedure(const v: PGLint); extdecl;
  glWindowPos3sMESA: procedure(x, y, z: GLshort); extdecl;
  glWindowPos3svMESA: procedure(const v: PGLshort); extdecl;
  glWindowPos4dMESA: procedure(x, y, z, w: GLdouble); extdecl;
  glWindowPos4dvMESA: procedure(const v: PGLdouble); extdecl;
  glWindowPos4fMESA: procedure(x, y, z, w: GLfloat); extdecl;
  glWindowPos4fvMESA: procedure(const v: PGLfloat); extdecl;
  glWindowPos4iMESA: procedure(x, y, z, w: GLint); extdecl;
  glWindowPos4ivMESA: procedure(const v: PGLint); extdecl;
  glWindowPos4sMESA: procedure(x, y, z, w: GLshort); extdecl;
  glWindowPos4svMESA: procedure(const v: PGLshort); extdecl;

//*** EXT_texture_compression_s3tc
const
  GL_COMPRESSED_RGB_S3TC_DXT1_EXT   = $83F0;
  GL_COMPRESSED_RGBA_S3TC_DXT1_EXT  = $83F1;
  GL_COMPRESSED_RGBA_S3TC_DXT3_EXT  = $83F2;
  GL_COMPRESSED_RGBA_S3TC_DXT5_EXT  = $83F3;

//*** IBM_cull_vertex
const
  GL_CULL_VERTEX_IBM		    = 103050;

//*** IBM_multimode_draw_arrays
var
  glMultiModeDrawArraysIBM: procedure(mode: GLenum; const first: PGLint;
    const count: PGLsizei; primcount: GLsizei; modestride: GLint); extdecl;
  glMultiModeDrawElementsIBM: procedure(const mode: PGLenum;
    const count: PGLsizei; atype: GLenum; const indices: PPointer;
    primcount: GLsizei; modestride: GLint); extdecl;

//*** IBM_vertex_array_lists
const
  GL_VERTEX_ARRAY_LIST_IBM	    = 103070;
  GL_NORMAL_ARRAY_LIST_IBM	    = 103071;
  GL_COLOR_ARRAY_LIST_IBM	    = 103072;
  GL_INDEX_ARRAY_LIST_IBM	    = 103073;
  GL_TEXTURE_COORD_ARRAY_LIST_IBM   = 103074;
  GL_EDGE_FLAG_ARRAY_LIST_IBM	    = 103075;
  GL_FOG_COORDINATE_ARRAY_LIST_IBM  = 103076;
  GL_SECONDARY_COLOR_ARRAY_LIST_IBM = 103077;
  GL_VERTEX_ARRAY_LIST_STRIDE_IBM   = 103080;
  GL_NORMAL_ARRAY_LIST_STRIDE_IBM   = 103081;
  GL_COLOR_ARRAY_LIST_STRIDE_IBM    = 103082;
  GL_INDEX_ARRAY_LIST_STRIDE_IBM    = 103083;
  GL_TEXTURE_COORD_ARRAY_LIST_STRIDE_IBM = 103084;
  GL_EDGE_FLAG_ARRAY_LIST_STRIDE_IBM = 103085;
  GL_FOG_COORDINATE_ARRAY_LIST_STRIDE_IBM = 103086;
  GL_SECONDARY_COLOR_ARRAY_LIST_STRIDE_IBM = 103087;

type
  PPGLboolean = ^PGLboolean;
  
var
  glColorPointerListIBM: procedure(size: GLint; atype: GLenum; stride: GLint; const ptr: PPointer; ptrstride: GLint); extdecl;
  glSecondaryColorPointerListIBM: procedure(size: GLint; atype: GLenum; stride: GLint; const ptr: PPointer; ptrstride: GLint); extdecl;
  glEdgeFlagPointerListIBM: procedure(stride: GLint; const ptr: PPGLboolean; ptrstride: GLint); extdecl;
  glFogCoordPointerListIBM: procedure(atype: GLenum; stride: GLint; const ptr: PPointer; ptrstride: GLint); extdecl;
  glIndexPointerListIBM: procedure(atype: GLenum; stride: GLint; const ptr: PPointer; ptrstride: GLint); extdecl;
  glNormalPointerListIBM: procedure(atype: GLenum; stride: GLint; const ptr: PPointer; ptrstride: GLint); extdecl;
  glTexCoordPointerListIBM: procedure(size: GLint; atype: GLenum; stride: GLint; const ptr: PPointer; ptrstride: GLint); extdecl;
  glVertexPointerListIBM: procedure(size: GLint; atype: GLenum; stride: GLint; const ptr: PPointer; ptrstride: GLint); extdecl;

//*** SGIX_subsample
const
  GL_PACK_SUBSAMPLE_RATE_SGIX	    = $85A0;
  GL_UNPACK_SUBSAMPLE_RATE_SGIX     = $85A1;
  GL_PIXEL_SUBSAMPLE_4444_SGIX	    = $85A2;
  GL_PIXEL_SUBSAMPLE_2424_SGIX	    = $85A3;
  GL_PIXEL_SUBSAMPLE_4242_SGIX	    = $85A4;

//*** SGIX_ycrcb_subsample

//*** SGIX_ycrcba
const
  GL_YCRCB_SGIX 		    = $8318;
  GL_YCRCBA_SGIX		    = $8319;

//*** SGI_depth_pass_instrument
const
  GL_DEPTH_PASS_INSTRUMENT_SGIX     = $8310;
  GL_DEPTH_PASS_INSTRUMENT_COUNTERS_SGIX = $8311;
  GL_DEPTH_PASS_INSTRUMENT_MAX_SGIX = $8312;

//*** 3DFX_texture_compression_FXT1
const
  GL_COMPRESSED_RGB_FXT1_3DFX	    = $86B0;
  GL_COMPRESSED_RGBA_FXT1_3DFX	    = $86B1;

//*** 3DFX_multisample
const
  GL_MULTISAMPLE_3DFX		    = $86B2;
  GL_SAMPLE_BUFFERS_3DFX	    = $86B3;
  GL_SAMPLES_3DFX		    = $86B4;
  GL_MULTISAMPLE_BIT_3DFX	    = $20000000;

//*** 3DFX_tbuffer
var
  glTbufferMask3DFX: procedure(mask: GLuint); extdecl;

//*** EXT_multisample
const
  GL_MULTISAMPLE_EXT		    = $809D;
  GL_SAMPLE_ALPHA_TO_MASK_EXT	    = $809E;
  GL_SAMPLE_ALPHA_TO_ONE_EXT	    = $809F;
  GL_SAMPLE_MASK_EXT		    = $80A0;
  GL_1PASS_EXT			    = $80A1;
  GL_2PASS_0_EXT		    = $80A2;
  GL_2PASS_1_EXT		    = $80A3;
  GL_4PASS_0_EXT		    = $80A4;
  GL_4PASS_1_EXT		    = $80A5;
  GL_4PASS_2_EXT		    = $80A6;
  GL_4PASS_3_EXT		    = $80A7;
  GL_SAMPLE_BUFFERS_EXT 	    = $80A8;
  GL_SAMPLES_EXT		    = $80A9;
  GL_SAMPLE_MASK_VALUE_EXT	    = $80AA;
  GL_SAMPLE_MASK_INVERT_EXT	    = $80AB;
  GL_SAMPLE_PATTERN_EXT 	    = $80AC;

var
  glSampleMaskEXT: procedure(value: GLclampf; invert: GLboolean); extdecl;
  glSamplePatternEXT: procedure(pattern: GLenum); extdecl;

//*** SGIX_vertex_preclip
const
  GL_VERTEX_PRECLIP_SGIX	    = $83EE;
  GL_VERTEX_PRECLIP_HINT_SGIX	    = $83EF;

//*** SGIX_convolution_accuracy
const
  GL_CONVOLUTION_HINT_SGIX	    = $8316;

//*** SGIX_resample
const
  GL_PACK_RESAMPLE_SGIX 	    = $842C;
  GL_UNPACK_RESAMPLE_SGIX	    = $842D;
  GL_RESAMPLE_REPLICATE_SGIX	    = $842E;
  GL_RESAMPLE_ZERO_FILL_SGIX	    = $842F;
  GL_RESAMPLE_DECIMATE_SGIX	    = $8430;

//*** SGIS_point_line_texgen
const
  GL_EYE_DISTANCE_TO_POINT_SGIS     = $81F0;
  GL_OBJECT_DISTANCE_TO_POINT_SGIS  = $81F1;
  GL_EYE_DISTANCE_TO_LINE_SGIS	    = $81F2;
  GL_OBJECT_DISTANCE_TO_LINE_SGIS   = $81F3;
  GL_EYE_POINT_SGIS		    = $81F4;
  GL_OBJECT_POINT_SGIS		    = $81F5;
  GL_EYE_LINE_SGIS		    = $81F6;
  GL_OBJECT_LINE_SGIS		    = $81F7;

//*** SGIS_texture_color_mask
const
  GL_TEXTURE_COLOR_WRITEMASK_SGIS   = $81EF;

var
  glTextureColorMaskSGIS: procedure(r, g, b, a: GLboolean); extdecl;

//*** ARB_texture_border_clamp
const
  GL_CLAMP_TO_BORDER_ARB	    = $812D;

//*** NV_vertex_program
const
  GL_VERTEX_PROGRAM_NV		    = $8620;
  GL_VERTEX_STATE_PROGRAM_NV	    = $8621;
  GL_ATTRIB_ARRAY_SIZE_NV	    = $8623;
  GL_ATTRIB_ARRAY_STRIDE_NV	    = $8624;
  GL_ATTRIB_ARRAY_TYPE_NV	    = $8625;
  GL_CURRENT_ATTRIB_NV		    = $8626;
  GL_PROGRAM_LENGTH_NV		    = $8627;
  GL_PROGRAM_STRING_NV		    = $8628;
  GL_MODELVIEW_PROJECTION_NV	    = $8629;
  GL_IDENTITY_NV		    = $862A;
  GL_INVERSE_NV 		    = $862B;
  GL_TRANSPOSE_NV		    = $862C;
  GL_INVERSE_TRANSPOSE_NV	    = $862D;
  GL_MAX_TRACK_MATRIX_STACK_DEPTH_NV = $862E;
  GL_MAX_TRACK_MATRICES_NV	    = $862F;
  GL_MATRIX0_NV 		    = $8630;
  GL_MATRIX1_NV 		    = $8631;
  GL_MATRIX2_NV 		    = $8632;
  GL_MATRIX3_NV 		    = $8633;
  GL_MATRIX4_NV 		    = $8634;
  GL_MATRIX5_NV 		    = $8635;
  GL_MATRIX6_NV 		    = $8636;
  GL_MATRIX7_NV 		    = $8637;
  GL_CURRENT_MATRIX_STACK_DEPTH_NV  = $8640;
  GL_CURRENT_MATRIX_NV		    = $8641;
  GL_VERTEX_PROGRAM_POINT_SIZE_NV   = $8642;
  GL_VERTEX_PROGRAM_TWO_SIDE_NV     = $8643;
  GL_PROGRAM_PARAMETER_NV	    = $8644;
  GL_ATTRIB_ARRAY_POINTER_NV	    = $8645;
  GL_PROGRAM_TARGET_NV		    = $8646;
  GL_PROGRAM_RESIDENT_NV	    = $8647;
  GL_TRACK_MATRIX_NV		    = $8648;
  GL_TRACK_MATRIX_TRANSFORM_NV	    = $8649;
  GL_VERTEX_PROGRAM_BINDING_NV	    = $864A;
  GL_PROGRAM_ERROR_POSITION_NV	    = $864B;
  GL_VERTEX_ATTRIB_ARRAY0_NV	    = $8650;
  GL_VERTEX_ATTRIB_ARRAY1_NV	    = $8651;
  GL_VERTEX_ATTRIB_ARRAY2_NV	    = $8652;
  GL_VERTEX_ATTRIB_ARRAY3_NV	    = $8653;
  GL_VERTEX_ATTRIB_ARRAY4_NV	    = $8654;
  GL_VERTEX_ATTRIB_ARRAY5_NV	    = $8655;
  GL_VERTEX_ATTRIB_ARRAY6_NV	    = $8656;
  GL_VERTEX_ATTRIB_ARRAY7_NV	    = $8657;
  GL_VERTEX_ATTRIB_ARRAY8_NV	    = $8658;
  GL_VERTEX_ATTRIB_ARRAY9_NV	    = $8659;
  GL_VERTEX_ATTRIB_ARRAY10_NV	    = $865A;
  GL_VERTEX_ATTRIB_ARRAY11_NV	    = $865B;
  GL_VERTEX_ATTRIB_ARRAY12_NV	    = $865C;
  GL_VERTEX_ATTRIB_ARRAY13_NV	    = $865D;
  GL_VERTEX_ATTRIB_ARRAY14_NV	    = $865E;
  GL_VERTEX_ATTRIB_ARRAY15_NV	    = $865F;
  GL_MAP1_VERTEX_ATTRIB0_4_NV	    = $8660;
  GL_MAP1_VERTEX_ATTRIB1_4_NV	    = $8661;
  GL_MAP1_VERTEX_ATTRIB2_4_NV	    = $8662;
  GL_MAP1_VERTEX_ATTRIB3_4_NV	    = $8663;
  GL_MAP1_VERTEX_ATTRIB4_4_NV	    = $8664;
  GL_MAP1_VERTEX_ATTRIB5_4_NV	    = $8665;
  GL_MAP1_VERTEX_ATTRIB6_4_NV	    = $8666;
  GL_MAP1_VERTEX_ATTRIB7_4_NV	    = $8667;
  GL_MAP1_VERTEX_ATTRIB8_4_NV	    = $8668;
  GL_MAP1_VERTEX_ATTRIB9_4_NV	    = $8669;
  GL_MAP1_VERTEX_ATTRIB10_4_NV	    = $866A;
  GL_MAP1_VERTEX_ATTRIB11_4_NV	    = $866B;
  GL_MAP1_VERTEX_ATTRIB12_4_NV	    = $866C;
  GL_MAP1_VERTEX_ATTRIB13_4_NV	    = $866D;
  GL_MAP1_VERTEX_ATTRIB14_4_NV	    = $866E;
  GL_MAP1_VERTEX_ATTRIB15_4_NV	    = $866F;
  GL_MAP2_VERTEX_ATTRIB0_4_NV	    = $8670;
  GL_MAP2_VERTEX_ATTRIB1_4_NV	    = $8671;
  GL_MAP2_VERTEX_ATTRIB2_4_NV	    = $8672;
  GL_MAP2_VERTEX_ATTRIB3_4_NV	    = $8673;
  GL_MAP2_VERTEX_ATTRIB4_4_NV	    = $8674;
  GL_MAP2_VERTEX_ATTRIB5_4_NV	    = $8675;
  GL_MAP2_VERTEX_ATTRIB6_4_NV	    = $8676;
  GL_MAP2_VERTEX_ATTRIB7_4_NV	    = $8677;
  GL_MAP2_VERTEX_ATTRIB8_4_NV	    = $8678;
  GL_MAP2_VERTEX_ATTRIB9_4_NV	    = $8679;
  GL_MAP2_VERTEX_ATTRIB10_4_NV	    = $867A;
  GL_MAP2_VERTEX_ATTRIB11_4_NV	    = $867B;
  GL_MAP2_VERTEX_ATTRIB12_4_NV	    = $867C;
  GL_MAP2_VERTEX_ATTRIB13_4_NV	    = $867D;
  GL_MAP2_VERTEX_ATTRIB14_4_NV	    = $867E;
  GL_MAP2_VERTEX_ATTRIB15_4_NV	    = $867F;

var
  glAreProgramsResidentNV: function(n: GLsizei; const programs: PGLuint;
    residences: PGLboolean): GLboolean; extdecl;
  glBindProgramNV: procedure(target: GLenum; id: GLuint); extdecl;
  glDeleteProgramsNV: procedure(n: GLsizei; const programs: PGLuint); extdecl;
  glExecuteProgramNV: procedure(target: GLenum; id: GLuint; const params: PGLfloat); extdecl;
  glGenProgramsNV: procedure(n: GLsizei; programs: PGLuint); extdecl;
  glGetProgramParameterdvNV: procedure(target: GLenum; index: GLuint; pname: GLenum; params: PGLdouble); extdecl;
  glGetProgramParameterfvNV: procedure(target: GLenum; index: GLuint; pname: GLenum; params: PGLfloat); extdecl;
  glGetProgramivNV: procedure(id: GLuint; pname: GLenum; params: PGLint); extdecl;
  glGetProgramStringNV: procedure(id: GLuint; pname: GLenum; aprogram: PGLubyte); extdecl;
  glGetTrackMatrixivNV: procedure(target: GLenum; address: GLuint; pname: GLenum; params: PGLint); extdecl;
  glGetVertexAttribdvNV: procedure(index: GLuint; pname: GLenum; params: PGLdouble); extdecl;
  glGetVertexAttribfvNV: procedure(index: GLuint; pname: GLenum; params: PGLfloat); extdecl;
  glGetVertexAttribivNV: procedure(index: GLuint; pname: GLenum; params: PGLint); extdecl;
  glGetVertexAttribPointervNV: procedure(index: GLuint; pname: GLenum; ptr: PPointer); extdecl;
  glIsProgramNV: function(id: GLuint): GLboolean; extdecl;
  glLoadProgramNV: procedure(target: GLenum; id: GLuint; len: GLsizei; const aprogram: PGLubyte); extdecl;
  glProgramParameter4dNV: procedure(target: GLenum; index: GLuint; x, y, z, w: GLdouble); extdecl;
  glProgramParameter4dvNV: procedure(target: GLenum; index: GLuint; const v: PGLdouble); extdecl;
  glProgramParameter4fNV: procedure(target: GLenum; index: GLuint; x, y, z, w: GLfloat); extdecl;
  glProgramParameter4fvNV: procedure(target: GLenum; index: GLuint; const v: PGLfloat); extdecl;
  glProgramParameters4dvNV: procedure(target: GLenum; index: GLuint; count: GLsizei; const v: PGLdouble); extdecl;
  glProgramParameters4fvNV: procedure(target: GLenum; index: GLuint; count: GLsizei; const v: PGLfloat); extdecl;
  glRequestResidentProgramsNV: procedure(n: GLsizei; const programs: PGLuint); extdecl;
  glTrackMatrixNV: procedure(target: GLenum; address: GLuint; matrix, transform: GLenum); extdecl;
  glVertexAttribPointerNV: procedure(index: GLuint; fsize: GLint; atype: GLenum; stride: GLsizei; const ptr: Pointer); extdecl;
  glVertexAttrib1dNV: procedure(index: GLuint; x: GLdouble); extdecl;
  glVertexAttrib1dvNV: procedure(index: GLuint; const v: PGLdouble); extdecl;
  glVertexAttrib1fNV: procedure(index: GLuint; x: GLfloat); extdecl;
  glVertexAttrib1fvNV: procedure(index: GLuint; const v: PGLfloat); extdecl;
  glVertexAttrib1sNV: procedure(index: GLuint; x: GLshort); extdecl;
  glVertexAttrib1svNV: procedure(index: GLuint; const v: PGLshort); extdecl;
  glVertexAttrib2dNV: procedure(index: GLuint; x, y: GLdouble); extdecl;
  glVertexAttrib2dvNV: procedure(index: GLuint; const v: PGLdouble); extdecl;
  glVertexAttrib2fNV: procedure(index: GLuint; x, y: GLfloat); extdecl;
  glVertexAttrib2fvNV: procedure(index: GLuint; const v: PGLfloat); extdecl;
  glVertexAttrib2sNV: procedure(index: GLuint; x, y: GLshort); extdecl;
  glVertexAttrib2svNV: procedure(index: GLuint; const v: PGLshort); extdecl;
  glVertexAttrib3dNV: procedure(index: GLuint; x, y, z: GLdouble); extdecl;
  glVertexAttrib3dvNV: procedure(index: GLuint; const v: PGLdouble); extdecl;
  glVertexAttrib3fNV: procedure(index: GLuint; x, y, z: GLfloat); extdecl;
  glVertexAttrib3fvNV: procedure(index: GLuint; const v: PGLfloat); extdecl;
  glVertexAttrib3sNV: procedure(index: GLuint; x, y, z: GLshort); extdecl;
  glVertexAttrib3svNV: procedure(index: GLuint; const v: PGLshort); extdecl;
  glVertexAttrib4dNV: procedure(index: GLuint; x, y, z, w: GLdouble); extdecl;
  glVertexAttrib4dvNV: procedure(index: GLuint; const v: PGLdouble); extdecl;
  glVertexAttrib4fNV: procedure(index: GLuint; x, y, z, w: GLfloat); extdecl;
  glVertexAttrib4fvNV: procedure(index: GLuint; const v: PGLfloat); extdecl;
  glVertexAttrib4sNV: procedure(index: GLuint; x, y, z, w: GLshort); extdecl;
  glVertexAttrib4svNV: procedure(index: GLuint; const v: PGLshort); extdecl;
  glVertexAttrib4ubvNV: procedure(index: GLuint; const v: PGLubyte); extdecl;
  glVertexAttribs1dvNV: procedure(index: GLuint; count: GLsizei; const v: PGLdouble); extdecl;
  glVertexAttribs1fvNV: procedure(index: GLuint; count: GLsizei; const v: PGLfloat); extdecl;
  glVertexAttribs1svNV: procedure(index: GLuint; count: GLsizei; const v: PGLshort); extdecl;
  glVertexAttribs2dvNV: procedure(index: GLuint; count: GLsizei; const v: PGLdouble); extdecl;
  glVertexAttribs2fvNV: procedure(index: GLuint; count: GLsizei; const v: PGLfloat); extdecl;
  glVertexAttribs2svNV: procedure(index: GLuint; count: GLsizei; const v: PGLshort); extdecl;
  glVertexAttribs3dvNV: procedure(index: GLuint; count: GLsizei; const v: PGLdouble); extdecl;
  glVertexAttribs3fvNV: procedure(index: GLuint; count: GLsizei; const v: PGLfloat); extdecl;
  glVertexAttribs3svNV: procedure(index: GLuint; count: GLsizei; const v: PGLshort); extdecl;
  glVertexAttribs4dvNV: procedure(index: GLuint; count: GLsizei; const v: PGLdouble); extdecl;
  glVertexAttribs4fvNV: procedure(index: GLuint; count: GLsizei; const v: PGLfloat); extdecl;
  glVertexAttribs4svNV: procedure(index: GLuint; count: GLsizei; const v: PGLshort); extdecl;
  glVertexAttribs4ubvNV: procedure(index: GLuint; count: GLsizei; const v: PGLubyte); extdecl;

//*** NV_evaluators
const
  GL_EVAL_2D_NV 		    = $86C0;
  GL_EVAL_TRIANGULAR_2D_NV	    = $86C1;
  GL_MAP_TESSELLATION_NV	    = $86C2;
  GL_MAP_ATTRIB_U_ORDER_NV	    = $86C3;
  GL_MAP_ATTRIB_V_ORDER_NV	    = $86C4;
  GL_EVAL_FRACTIONAL_TESSELLATION_NV = $86C5;
  GL_EVAL_VERTEX_ATTRIB0_NV	    = $86C6;
  GL_EVAL_VERTEX_ATTRIB1_NV	    = $86C7;
  GL_EVAL_VERTEX_ATTRIB2_NV	    = $86C8;
  GL_EVAL_VERTEX_ATTRIB3_NV	    = $86C9;
  GL_EVAL_VERTEX_ATTRIB4_NV	    = $86CA;
  GL_EVAL_VERTEX_ATTRIB5_NV	    = $86CB;
  GL_EVAL_VERTEX_ATTRIB6_NV	    = $86CC;
  GL_EVAL_VERTEX_ATTRIB7_NV	    = $86CD;
  GL_EVAL_VERTEX_ATTRIB8_NV	    = $86CE;
  GL_EVAL_VERTEX_ATTRIB9_NV	    = $86CF;
  GL_EVAL_VERTEX_ATTRIB10_NV	    = $86D0;
  GL_EVAL_VERTEX_ATTRIB11_NV	    = $86D1;
  GL_EVAL_VERTEX_ATTRIB12_NV	    = $86D2;
  GL_EVAL_VERTEX_ATTRIB13_NV	    = $86D3;
  GL_EVAL_VERTEX_ATTRIB14_NV	    = $86D4;
  GL_EVAL_VERTEX_ATTRIB15_NV	    = $86D5;
  GL_MAX_MAP_TESSELLATION_NV	    = $86D6;
  GL_MAX_RATIONAL_EVAL_ORDER_NV     = $86D7;

var
  glMapControlPointsNV: procedure(target: GLenum; index: GLuint; atype: GLenum;
    ustride, vstride: GLsizei; uorder, vorder: GLint; apacked: GLboolean;
    const points: Pointer); extdecl;
  glMapParameterivNV: procedure(target, pname: GLenum; const params: PGLint); extdecl;
  glMapParameterfvNV: procedure(target, pname: GLenum; const params: PGLfloat); extdecl;
  glGetMapControlPointsNV: procedure(target: GLenum; index: GLuint;
    atype: GLenum; ustride, vstride: GLsizei; apacked: GLboolean;
    points: Pointer); extdecl;
  glGetMapParameterivNV: procedure(target, pname: GLenum; params: PGLint); extdecl;
  glGetMapParameterfvNV: procedure(target, pname: GLenum; params: PGLfloat); extdecl;
  glGetMapAttribParameterivNV: procedure(target: GLenum; index: GLuint;
    pname: GLenum; params: PGLint); extdecl;
  glGetMapAttribParameterfvNV: procedure(target: GLenum; index: GLuint;
    pname: GLenum; params: PGLfloat); extdecl;
  glEvalMapsNV: procedure(target, mode: GLenum); extdecl;

//*** NV_texture_rectangle
const
  GL_TEXTURE_RECTANGLE_NV	    = $84F5;
  GL_TEXTURE_BINDING_RECTANGLE_NV   = $84F6;
  GL_PROXY_TEXTURE_RECTANGLE_NV     = $84F7;
  GL_MAX_RECTANGLE_TEXTURE_SIZE_NV  = $84F8;

//*** NV_texture_shader
const
  GL_RGBA_UNSIGNED_DOT_PRODUCT_MAPPING_NV = $86D9;
  GL_UNSIGNED_INT_S8_S8_8_8_NV	    = $86DA;
  GL_UNSIGNED_INT_S8_S8_8_8_REV_NV  = $86DB;
  GL_DSDT_MAG_INTENSITY_NV	    = $86DC;
  GL_SHADER_CONSISTENT_NV	    = $86DD;
  GL_TEXTURE_SHADER_NV		    = $86DE;
  GL_SHADER_OPERATION_NV	    = $86DF;
  GL_CULL_MODES_NV		    = $86E0;
  GL_OFFSET_TEXTURE_2D_MATRIX_NV    = $86E1;
  GL_OFFSET_TEXTURE_2D_SCALE_NV     = $86E2;
  GL_OFFSET_TEXTURE_2D_BIAS_NV	    = $86E3;
  GL_PREVIOUS_TEXTURE_INPUT_NV	    = $86E4;
  GL_CONST_EYE_NV		    = $86E5;
  GL_PASS_THROUGH_NV		    = $86E6;
  GL_CULL_FRAGMENT_NV		    = $86E7;
  GL_OFFSET_TEXTURE_2D_NV	    = $86E8;
  GL_DEPENDENT_AR_TEXTURE_2D_NV     = $86E9;
  GL_DEPENDENT_GB_TEXTURE_2D_NV     = $86EA;
  GL_DOT_PRODUCT_NV		    = $86EC;
  GL_DOT_PRODUCT_DEPTH_REPLACE_NV   = $86ED;
  GL_DOT_PRODUCT_TEXTURE_2D_NV	    = $86EE;
  GL_DOT_PRODUCT_TEXTURE_3D_NV	    = $86EF;
  GL_DOT_PRODUCT_TEXTURE_CUBE_MAP_NV = $86F0;
  GL_DOT_PRODUCT_DIFFUSE_CUBE_MAP_NV = $86F1;
  GL_DOT_PRODUCT_REFLECT_CUBE_MAP_NV = $86F2;
  GL_DOT_PRODUCT_CONST_EYE_REFLECT_CUBE_MAP_NV = $86F3;
  GL_HILO_NV			    = $86F4;
  GL_DSDT_NV			    = $86F5;
  GL_DSDT_MAG_NV		    = $86F6;
  GL_DSDT_MAG_VIB_NV		    = $86F7;
  GL_HILO16_NV			    = $86F8;
  GL_SIGNED_HILO_NV		    = $86F9;
  GL_SIGNED_HILO16_NV		    = $86FA;
  GL_SIGNED_RGBA_NV		    = $86FB;
  GL_SIGNED_RGBA8_NV		    = $86FC;
  GL_SIGNED_RGB_NV		    = $86FE;
  GL_SIGNED_RGB8_NV		    = $86FF;
  GL_SIGNED_LUMINANCE_NV	    = $8701;
  GL_SIGNED_LUMINANCE8_NV	    = $8702;
  GL_SIGNED_LUMINANCE_ALPHA_NV	    = $8703;
  GL_SIGNED_LUMINANCE8_ALPHA8_NV    = $8704;
  GL_SIGNED_ALPHA_NV		    = $8705;
  GL_SIGNED_ALPHA8_NV		    = $8706;
  GL_SIGNED_INTENSITY_NV	    = $8707;
  GL_SIGNED_INTENSITY8_NV	    = $8708;
  GL_DSDT8_NV			    = $8709;
  GL_DSDT8_MAG8_NV		    = $870A;
  GL_DSDT8_MAG8_INTENSITY8_NV	    = $870B;
  GL_SIGNED_RGB_UNSIGNED_ALPHA_NV   = $870C;
  GL_SIGNED_RGB8_UNSIGNED_ALPHA8_NV = $870D;
  GL_HI_SCALE_NV		    = $870E;
  GL_LO_SCALE_NV		    = $870F;
  GL_DS_SCALE_NV		    = $8710;
  GL_DT_SCALE_NV		    = $8711;
  GL_MAGNITUDE_SCALE_NV 	    = $8712;
  GL_VIBRANCE_SCALE_NV		    = $8713;
  GL_HI_BIAS_NV 		    = $8714;
  GL_LO_BIAS_NV 		    = $8715;
  GL_DS_BIAS_NV 		    = $8716;
  GL_DT_BIAS_NV 		    = $8717;
  GL_MAGNITUDE_BIAS_NV		    = $8718;
  GL_VIBRANCE_BIAS_NV		    = $8719;
  GL_TEXTURE_BORDER_VALUES_NV	    = $871A;
  GL_TEXTURE_HI_SIZE_NV 	    = $871B;
  GL_TEXTURE_LO_SIZE_NV 	    = $871C;
  GL_TEXTURE_DS_SIZE_NV 	    = $871D;
  GL_TEXTURE_DT_SIZE_NV 	    = $871E;
  GL_TEXTURE_MAG_SIZE_NV	    = $871F;

//*** NV_register_combiners2
const
  GL_PER_STAGE_CONSTANTS_NV	    = $8535;

var
  glCombinerStageParameterfvNV: procedure(stage, pname: GLenum;
    const params: PGLfloat); extdecl;
  glGetCombinerStageParameterfvNV: procedure(stage, pname: GLenum;
    params: PGLfloat); extdecl;

//*** NV_packed_depth_stencil
const
  GL_DEPTH_STENCIL_NV		    = $84F9;
  GL_UNSIGNED_INT_24_8_NV	    = $84FA;

//*** NV_fence
const
  GL_ALL_COMPLETED_NV		    = $84F2;
  GL_FENCE_STATUS_NV		    = $84F3;
  GL_FENCE_CONDITION_NV 	    = $84F4;

var
  glGenFencesNV: procedure(n: GLsizei; fences: PGLuint); extdecl;
  glDeleteFencesNV: procedure(n: GLsizei; const fences: PGLuint); extdecl;
  glSetFenceNV: procedure(fence: GLuint; condition: GLenum); extdecl;
  glTestFenceNV: function(fence: GLuint): GLboolean; extdecl;
  glFinishFenceNV: procedure(fence: GLuint); extdecl;
  glIsFenceNV: function(fence: GLuint): GLboolean; extdecl;
  glGetFenceivNV: procedure(fence: GLuint; pname: GLenum; params: PGLint); extdecl;

//*** SGIX_texture_coordinate_clamp
const
  GL_TEXTURE_MAX_CLAMP_S_SGIX       = $8369;
  GL_TEXTURE_MAX_CLAMP_T_SGIX       = $836A;
  GL_TEXTURE_MAX_CLAMP_R_SGIX       = $836B;

//*** OML_interlace
const
  GL_INTERLACE_OML                  = $8980;
  GL_INTERLACE_READ_OML             = $8981;

//*** OML_subsample
const
  GL_FORMAT_SUBSAMPLE_24_24_OML     = $8982;
  GL_FORMAT_SUBSAMPLE_244_244_OML   = $8983;

//*** OML_resample
const
  GL_PACK_RESAMPLE_OML              = $8984;
  GL_UNPACK_RESAMPLE_OML            = $8985;
  GL_RESAMPLE_REPLICATE_OML         = $8986;
  GL_RESAMPLE_ZERO_FILL_OML         = $8987;
  GL_RESAMPLE_AVERAGE_OML           = $8988;
  GL_RESAMPLE_DECIMATE_OML          = $8989;

//*** NV_COPY_DEPTH_TO_COLOR
const
  GL_DEPTH_STENCIL_TO_RGBA_NV       = $886E;
  GL_DEPTH_STENCIL_TO_BGRA_NV       = $886F;

//*** ATI_envmap_bumpmap
const
  GL_BUMP_ROT_MATRIX_ATI            = $8775;
  GL_BUMP_ROT_MATRIX_SIZE_ATI       = $8776;
  GL_BUMP_NUM_TEX_UNITS_ATI         = $8777;
  GL_BUMP_TEX_UNITS_ATI             = $8778;
  GL_DUDV_ATI                       = $8779;
  GL_DU8DV8_ATI                     = $877A;
  GL_BUMP_ENVMAP_ATI                = $877B;
  GL_BUMP_TARGET_ATI                = $877C;

var
  glTexBumpParameterivATI: procedure(pname: GLenum; param: PGLint); extdecl;
  glTexBumpParameterfvATI: procedure(pname: GLenum; param: PGLfloat); extdecl;
  glGetTexBumpParameterivATI: procedure(pname: GLenum; param: PGLint); extdecl;
  glGetTexBumpParameterfvATI: procedure(pname: GLenum; param: PGLfloat); extdecl;

//*** ATI_fragment_shader
const
  GL_FRAGMENT_SHADER_ATI            = $8920;
  GL_REG_0_ATI                      = $8921;
  GL_REG_1_ATI                      = $8922;
  GL_REG_2_ATI                      = $8923;
  GL_REG_3_ATI                      = $8924;
  GL_REG_4_ATI                      = $8925;
  GL_REG_5_ATI                      = $8926;
  GL_REG_6_ATI                      = $8927;
  GL_REG_7_ATI                      = $8928;
  GL_REG_8_ATI                      = $8929;
  GL_REG_9_ATI                      = $892A;
  GL_REG_10_ATI                     = $892B;
  GL_REG_11_ATI                     = $892C;
  GL_REG_12_ATI                     = $892D;
  GL_REG_13_ATI                     = $892E;
  GL_REG_14_ATI                     = $892F;
  GL_REG_15_ATI                     = $8930;
  GL_REG_16_ATI                     = $8931;
  GL_REG_17_ATI                     = $8932;
  GL_REG_18_ATI                     = $8933;
  GL_REG_19_ATI                     = $8934;
  GL_REG_20_ATI                     = $8935;
  GL_REG_21_ATI                     = $8936;
  GL_REG_22_ATI                     = $8937;
  GL_REG_23_ATI                     = $8938;
  GL_REG_24_ATI                     = $8939;
  GL_REG_25_ATI                     = $893A;
  GL_REG_26_ATI                     = $893B;
  GL_REG_27_ATI                     = $893C;
  GL_REG_28_ATI                     = $893D;
  GL_REG_29_ATI                     = $893E;
  GL_REG_30_ATI                     = $893F;
  GL_REG_31_ATI                     = $8940;
  GL_CON_0_ATI                      = $8941;
  GL_CON_1_ATI                      = $8942;
  GL_CON_2_ATI                      = $8943;
  GL_CON_3_ATI                      = $8944;
  GL_CON_4_ATI                      = $8945;
  GL_CON_5_ATI                      = $8946;
  GL_CON_6_ATI                      = $8947;
  GL_CON_7_ATI                      = $8948;
  GL_CON_8_ATI                      = $8949;
  GL_CON_9_ATI                      = $894A;
  GL_CON_10_ATI                     = $894B;
  GL_CON_11_ATI                     = $894C;
  GL_CON_12_ATI                     = $894D;
  GL_CON_13_ATI                     = $894E;
  GL_CON_14_ATI                     = $894F;
  GL_CON_15_ATI                     = $8950;
  GL_CON_16_ATI                     = $8951;
  GL_CON_17_ATI                     = $8952;
  GL_CON_18_ATI                     = $8953;
  GL_CON_19_ATI                     = $8954;
  GL_CON_20_ATI                     = $8955;
  GL_CON_21_ATI                     = $8956;
  GL_CON_22_ATI                     = $8957;
  GL_CON_23_ATI                     = $8958;
  GL_CON_24_ATI                     = $8959;
  GL_CON_25_ATI                     = $895A;
  GL_CON_26_ATI                     = $895B;
  GL_CON_27_ATI                     = $895C;
  GL_CON_28_ATI                     = $895D;
  GL_CON_29_ATI                     = $895E;
  GL_CON_30_ATI                     = $895F;
  GL_CON_31_ATI                     = $8960;
  GL_MOV_ATI                        = $8961;
  GL_ADD_ATI                        = $8963;
  GL_MUL_ATI                        = $8964;
  GL_SUB_ATI                        = $8965;
  GL_DOT3_ATI                       = $8966;
  GL_DOT4_ATI                       = $8967;
  GL_MAD_ATI                        = $8968;
  GL_LERP_ATI                       = $8969;
  GL_CND_ATI                        = $896A;
  GL_CND0_ATI                       = $896B;
  GL_DOT2_ADD_ATI                   = $896C;
  GL_SECONDARY_INTERPOLATOR_ATI     = $896D;
  GL_NUM_FRAGMENT_REGISTERS_ATI     = $896E;
  GL_NUM_FRAGMENT_CONSTANTS_ATI     = $896F;
  GL_NUM_PASSES_ATI                 = $8970;
  GL_NUM_INSTRUCTIONS_PER_PASS_ATI  = $8971;
  GL_NUM_INSTRUCTIONS_TOTAL_ATI     = $8972;
  GL_NUM_INPUT_INTERPOLATOR_COMPONENTS_ATI = $8973;
  GL_NUM_LOOPBACK_COMPONENTS_ATI    = $8974;
  GL_COLOR_ALPHA_PAIRING_ATI        = $8975;
  GL_SWIZZLE_STR_ATI                = $8976;
  GL_SWIZZLE_STQ_ATI                = $8977;
  GL_SWIZZLE_STR_DR_ATI             = $8978;
  GL_SWIZZLE_STQ_DQ_ATI             = $8979;
  GL_SWIZZLE_STRQ_ATI               = $897A;
  GL_SWIZZLE_STRQ_DQ_ATI            = $897B;
  GL_RED_BIT_ATI                    = $00000001;
  GL_GREEN_BIT_ATI                  = $00000002;
  GL_BLUE_BIT_ATI                   = $00000004;
  GL_2X_BIT_ATI                     = $00000001;
  GL_4X_BIT_ATI                     = $00000002;
  GL_8X_BIT_ATI                     = $00000004;
  GL_HALF_BIT_ATI                   = $00000008;
  GL_QUARTER_BIT_ATI                = $00000010;
  GL_EIGHTH_BIT_ATI                 = $00000020;
  GL_SATURATE_BIT_ATI               = $00000040;
  GL_COMP_BIT_ATI                   = $00000002;
  GL_NEGATE_BIT_ATI                 = $00000004;
  GL_BIAS_BIT_ATI                   = $00000008;

var
  glGenFragmentShadersATI: function(range: GLuint): GLuint; extdecl;
  glBindFragmentShaderATI: procedure(id: GLuint); extdecl;
  glDeleteFragmentShaderATI: procedure(id: GLuint); extdecl;
  glBeginFragmentShaderATI: procedure; extdecl;
  glEndFragmentShaderATI: procedure; extdecl;
  glPassTexCoordATI: procedure(dst, coord: GLuint; swizzle: GLenum);
  glSampleMapATI: procedure(dst, interp: GLuint; swizzle: GLenum); extdecl;
  glColorFragmentOp1ATI: procedure(op: GLenum; dst, dstMask, dstMod, arg1,
      arg1Rep, arg1Mod: GLuint); extdecl;
  glColorFragmentOp2ATI: procedure(op: GLenum; dst, dstMask, dstMod, arg1,
      arg1Rep, arg1Mod, arg2, arg2Rep, arg2Mod: GLuint); extdecl;
  glColorFragmentOp3ATI: procedure(op: GLenum; dst, dstMask, dstMod, arg1,
      arg1Rep, arg1Mod, arg2, arg2Rep, arg2Mod, arg3, arg3Rep, arg3Mod: GLuint); extdecl;
  glAlphaFragmentOp1ATI: procedure(op: GLenum; dst, dstMod, arg1, arg1Rep,
      arg1Mod: GLuint); extdecl;
  glAlphaFragmentOp2ATI: procedure(op: GLenum; dst, dstMod, arg1, arg1Rep,
      arg1Mod, arg2, arg2Rep, arg2Mod: GLuint); extdecl;
  glAlphaFragmentOp3ATI: procedure(op: GLenum; dst, dstMod, arg1, arg1Rep,
      arg1Mod, arg2, arg2Rep, arg2Mod, arg3, arg3Rep, arg3Mod: GLuint); extdecl;
  glSetFragmentShaderConstantATI: procedure(dst: GLuint; const value: PGLfloat); extdecl;

//*** ATI_pn_triangles
const
  GL_PN_TRIANGLES_ATI                         = $87F0;
  GL_MAX_PN_TRIANGLES_TESSELATION_LEVEL_ATI   = $87F1;
  GL_PN_TRIANGLES_POINT_MODE_ATI              = $87F2;
  GL_PN_TRIANGLES_NORMAL_MODE_ATI             = $87F3;
  GL_PN_TRIANGLES_TESSELATION_LEVEL_ATI       = $87F4;
  GL_PN_TRIANGLES_POINT_MODE_LINEAR_ATI       = $87F5;
  GL_PN_TRIANGLES_POINT_MODE_CUBIC_ATI        = $87F6;
  GL_PN_TRIANGLES_NORMAL_MODE_LINEAR_ATI      = $87F7;
  GL_PN_TRIANGLES_NORMAL_MODE_QUADRATIC_ATI   = $87F8;

var
  glPNTrianglesiATI: procedure(pname: GLenum; param: GLint); extdecl;
  glPNTrianglesfATI: procedure(pname: GLenum; param: GLfloat) extdecl;

//*** ATI_vertex_array_object
const
  GL_STATIC_ATI                     = $8760;
  GL_DYNAMIC_ATI                    = $8761;
  GL_PRESERVE_ATI                   = $8762;
  GL_DISCARD_ATI                    = $8763;
  GL_OBJECT_BUFFER_SIZE_ATI         = $8764;
  GL_OBJECT_BUFFER_USAGE_ATI        = $8765;
  GL_ARRAY_OBJECT_BUFFER_ATI        = $8766;
  GL_ARRAY_OBJECT_OFFSET_ATI        = $8767;

var
  glNewObjectBufferATI: function(size: GLsizei; const ptr: Pointer;
      usage: GLenum): GLuint; extdecl;
  glIsObjectBufferATI: function(buffer: GLuint): GLboolean; extdecl;
  glUpdateObjectBufferATI: procedure(buffer, offset: GLuint; size: GLsizei;
       const ptr: Pointer; preserve: GLenum); extdecl;
  glGetObjectBufferfvATI: procedure(buffer: GLuint; pname: GLenum;
      params: PGLfloat); extdecl;
  glGetObjectBufferivATI: procedure(buffer: GLuint; pname: GLenum;
      params: PGLint); extdecl;
  glFreeObjectBufferATI: procedure(buffer: GLuint); extdecl;
  glArrayObjectATI: procedure(aarray: GLenum; size: GLint; atype: GLenum;
      stride: GLsizei; buffer, offset: GLuint); extdecl;
  glGetArrayObjectfvATI: procedure(aarray, pname: GLenum; params: PGLfloat); extdecl;
  glGetArrayObjectivATI: procedure(aarray, pname: GLenum; params: PGLint); extdecl;
  glVariantArrayObjectATI: procedure(id: GLuint; atype: GLenum; stride: GLsizei;
      buffer, offset: GLuint); extdecl;
  glGetVariantArrayObjectfvATI: procedure(id: GLuint; pname: GLenum;
      params: PGLfloat); extdecl;
  glGetVariantArrayObjectivATI: procedure(id: GLuint; pname: GLenum;
      params: PGLint); extdecl;

//*** EXT_vertex_shader
const
  GL_VERTEX_SHADER_EXT              = $8780;
  GL_VARIANT_VALUE_EXT              = $87e4;
  GL_VARIANT_DATATYPE_EXT           = $87e5;
  GL_VARIANT_ARRAY_STRIDE_EXT       = $87e6;
  GL_VARIANT_ARRAY_TYPE_EXT         = $87e7;
  GL_VARIANT_ARRAY_EXT              = $87e8;
  GL_VARIANT_ARRAY_POINTER_EXT      = $87e9;
  GL_INVARIANT_VALUE_EXT            = $87ea;
  GL_INVARIANT_DATATYPE_EXT         = $87eb;
  GL_LOCAL_CONSTANT_VALUE_EXT       = $87ec;
  GL_LOCAL_CONSTANT_DATATYPE_EXT    = $87ed;
  GL_OP_INDEX_EXT                   = $8782;
  GL_OP_NEGATE_EXT                  = $8783;
  GL_OP_DOT3_EXT                    = $8784;
  GL_OP_DOT4_EXT                    = $8785;
  GL_OP_MUL_EXT                     = $8786;
  GL_OP_ADD_EXT                     = $8787;
  GL_OP_MADD_EXT                    = $8788;
  GL_OP_FRAC_EXT                    = $8789;
  GL_OP_MAX_EXT                     = $878a;
  GL_OP_MIN_EXT                     = $878b;
  GL_OP_SET_GE_EXT                  = $878c;
  GL_OP_SET_LT_EXT                  = $878d;
  GL_OP_CLAMP_EXT                   = $878e;
  GL_OP_FLOOR_EXT                   = $878f;
  GL_OP_ROUND_EXT                   = $8790;
  GL_OP_EXP_BASE_2_EXT              = $8791;
  GL_OP_LOG_BASE_2_EXT              = $8792;
  GL_OP_POWER_EXT                   = $8793;
  GL_OP_RECIP_EXT                   = $8794;
  GL_OP_RECIP_SQRT_EXT              = $8795;
  GL_OP_SUB_EXT                     = $8796;
  GL_OP_CROSS_PRODUCT_EXT           = $8797;
  GL_OP_MULTIPLY_MATRIX_EXT         = $8798;
  GL_OP_MOV_EXT                     = $8799;
  GL_OUTPUT_VERTEX_EXT              = $879a;
  GL_OUTPUT_COLOR0_EXT              = $879b;
  GL_OUTPUT_COLOR1_EXT              = $879c;
  GL_OUTPUT_TEXTURE_COORD0_EXT      = $879d;
  GL_OUTPUT_TEXTURE_COORD1_EXT      = $879e;
  GL_OUTPUT_TEXTURE_COORD2_EXT      = $879f;
  GL_OUTPUT_TEXTURE_COORD3_EXT      = $87a0;
  GL_OUTPUT_TEXTURE_COORD4_EXT      = $87a1;
  GL_OUTPUT_TEXTURE_COORD5_EXT      = $87a2;
  GL_OUTPUT_TEXTURE_COORD6_EXT      = $87a3;
  GL_OUTPUT_TEXTURE_COORD7_EXT      = $87a4;
  GL_OUTPUT_TEXTURE_COORD8_EXT      = $87a5;
  GL_OUTPUT_TEXTURE_COORD9_EXT      = $87a6;
  GL_OUTPUT_TEXTURE_COORD10_EXT     = $87a7;
  GL_OUTPUT_TEXTURE_COORD11_EXT     = $87a8;
  GL_OUTPUT_TEXTURE_COORD12_EXT     = $87a9;
  GL_OUTPUT_TEXTURE_COORD13_EXT     = $87aa;
  GL_OUTPUT_TEXTURE_COORD14_EXT     = $87ab;
  GL_OUTPUT_TEXTURE_COORD15_EXT     = $87ac;
  GL_OUTPUT_TEXTURE_COORD16_EXT     = $87ad;
  GL_OUTPUT_TEXTURE_COORD17_EXT     = $87ae;
  GL_OUTPUT_TEXTURE_COORD18_EXT     = $87af;
  GL_OUTPUT_TEXTURE_COORD19_EXT     = $87b0;
  GL_OUTPUT_TEXTURE_COORD20_EXT     = $87b1;
  GL_OUTPUT_TEXTURE_COORD21_EXT     = $87b2;
  GL_OUTPUT_TEXTURE_COORD22_EXT     = $87b3;
  GL_OUTPUT_TEXTURE_COORD23_EXT     = $87b4;
  GL_OUTPUT_TEXTURE_COORD24_EXT     = $87b5;
  GL_OUTPUT_TEXTURE_COORD25_EXT     = $87b6;
  GL_OUTPUT_TEXTURE_COORD26_EXT     = $87b7;
  GL_OUTPUT_TEXTURE_COORD27_EXT     = $87b8;
  GL_OUTPUT_TEXTURE_COORD28_EXT     = $87b9;
  GL_OUTPUT_TEXTURE_COORD29_EXT     = $87ba;
  GL_OUTPUT_TEXTURE_COORD30_EXT     = $87bb;
  GL_OUTPUT_TEXTURE_COORD31_EXT     = $87bc;
  GL_OUTPUT_FOG_EXT                 = $87bd;
  GL_SCALAR_EXT                     = $87be;
  GL_VECTOR_EXT                     = $87bf;
  GL_MATRIX_EXT                     = $87c0;
  GL_VARIANT_EXT                    = $87c1;
  GL_INVARIANT_EXT                  = $87c2;
  GL_LOCAL_CONSTANT_EXT             = $87c3;
  GL_LOCAL_EXT                      = $87c4;
  GL_MAX_VERTEX_SHADER_INSTRUCTIONS_EXT = $87c5;
  GL_MAX_VERTEX_SHADER_VARIANTS_EXT = $87c6;
  GL_MAX_VERTEX_SHADER_INVARIANTS_EXT = $87c7;
  GL_MAX_VERTEX_SHADER_LOCAL_CONSTANTS_EXT = $87c8;
  GL_MAX_VERTEX_SHADER_LOCALS_EXT   = $87c9;
  GL_MAX_OPTIMIZED_VERTEX_SHADER_INSTRUCTIONS_EXT = $87ca;
  GL_MAX_OPTIMIZED_VERTEX_SHADER_VARIANTS_EXT = $87cb;
  GL_MAX_OPTIMIZED_VERTEX_SHADER_LOCAL_CONSTANTS_EXT = $87cc;
  GL_MAX_OPTIMIZED_VERTEX_SHADER_INVARIANTS_EXT = $87cd;
  GL_MAX_OPTIMIZED_VERTEX_SHADER_LOCALS_EXT = $87ce;
  GL_VERTEX_SHADER_INSTRUCTIONS_EXT = $87cf;
  GL_VERTEX_SHADER_VARIANTS_EXT     = $87d0;
  GL_VERTEX_SHADER_INVARIANTS_EXT   = $87d1;
  GL_VERTEX_SHADER_LOCAL_CONSTANTS_EXT = $87d2;
  GL_VERTEX_SHADER_LOCALS_EXT       = $87d3;
  GL_VERTEX_SHADER_BINDING_EXT      = $8781;
  GL_VERTEX_SHADER_OPTIMIZED_EXT    = $87d4;
  GL_X_EXT                          = $87d5;
  GL_Y_EXT                          = $87d6;
  GL_Z_EXT                          = $87d7;
  GL_W_EXT                          = $87d8;
  GL_NEGEXTVE_X_EXT                 = $87d9;
  GL_NEGEXTVE_Y_EXT                 = $87da;
  GL_NEGEXTVE_Z_EXT                 = $87db;
  GL_NEGEXTVE_W_EXT                 = $87dc;
  GL_NEGEXTVE_ONE_EXT               = $87df;
  GL_NORMALIZED_RANGE_EXT           = $87e0;
  GL_FULL_RANGE_EXT                 = $87e1;
  GL_CURRENT_VERTEX_EXT             = $87e2;
  GL_MVP_MATRIX_EXT                 = $87e3;

var
  glBeginVertexShaderEXT: procedure; extdecl;
  glEndVertexShaderEXT: procedure; extdecl;
  glBindVertexShaderEXT: procedure(id: GLuint); extdecl;
  glGenVertexShadersEXT: function(range: GLuint): GLuint; extdecl;
  glDeleteVertexShaderEXT: procedure(id: GLuint); extdecl;
  glShaderOp1EXT: procedure(op: GLenum; res, arg1: GLuint); extdecl;
  glShaderOp2EXT: procedure(op: GLenum; res, arg1, arg2: GLuint); extdecl;
  glShaderOp3EXT: procedure(op: GLenum; res, arg1, arg2, arg3: GLuint); extdecl;
  glSwizzleEXT: procedure(res, ain: GLuint; outX, outY, outZ, outW: GLenum); extdecl;
  glWriteMaskEXT: procedure(res, ain: GLuint; outX, outY, outZ, outW: GLenum); extdecl;
  glInsertComponentEXT: procedure(res, src, num: GLuint); extdecl;
  glExtractComponentEXT: procedure(res, src, num: GLuint); extdecl;
  glGenSymbolsEXT: function(datatype, storagetype, range: GLenum;
      components: GLuint): GLuint; extdecl;
  glSetInvariantEXT: procedure(id: GLuint; atype: GLenum; addr: Pointer); extdecl;
  glSetLocalConstantEXT: procedure(id: GLuint; atype: GLenum; addr: Pointer); extdecl;
  glVariantbvEXT: procedure(id: GLuint; addr: PGLbyte); extdecl;
  glVariantsvEXT: procedure(id: GLuint; addr: PGLshort); extdecl;
  glVariantivEXT: procedure(id: GLuint; addr: PGLint); extdecl;
  glVariantfvEXT: procedure(id: GLuint; addr: PGLfloat); extdecl;
  glVariantdvEXT: procedure(id: GLuint; addr: PGLdouble); extdecl;
  glVariantubvEXT: procedure(id: GLuint; addr: PGLubyte); extdecl;
  glVariantusvEXT: procedure(id: GLuint; addr: PGLushort); extdecl;
  glVariantuivEXT: procedure(id: GLuint; addr: PGLuint); extdecl;
  glVariantPointerEXT: procedure(id: GLuint; atype: GLenum; stride: GLuint;
      addr: Pointer); extdecl;
  glEnableVariantClientStateEXT: procedure(id: GLuint); extdecl;
  glDisableVariantClientStateEXT: procedure(id: GLuint); extdecl;
  glBindLightParameterEXT: function(light, value: GLenum): GLuint; extdecl;
  glBindMaterialParameterEXT: function(face, value: GLenum): GLuint; extdecl;
  glBindTexGenParameterEXT: function(aunit, coord, value: GLenum): GLuint; extdecl;
  glBindTextureUnitParameterEXT: function(aunit, value: GLenum): GLuint; extdecl;
  glBindParameterEXT: function(value: GLenum): GLuint; extdecl;
  glIsVariantEnabledEXT: function(id: GLuint; cap: GLenum): GLboolean; extdecl;
  glGetVariantBooleanvEXT: procedure(id: GLuint; value: GLenum;
      data: PGLboolean); extdecl;
  glGetVariantIntegervEXT: procedure(id: GLuint; value: GLenum; data: PGLint); extdecl;
  glGetVariantFloatvEXT: procedure(id: GLuint; value: GLenum; data: PGLfloat); extdecl;
  glGetVariantPointervEXT: procedure(id: GLuint; value: GLenum; data: PPointer); extdecl;
  glGetInvariantBooleanvEXT: procedure(id: GLuint; value: GLenum;
      data: PGLboolean); extdecl;
  glGetInvariantIntegervEXT: procedure(id: GLuint; value: GLenum; data: PGLint); extdecl;
  glGetInvariantFloatvEXT: procedure(id: GLuint; value: GLenum; data: PGLfloat); extdecl;
  glGetLocalConstantBooleanvEXT: procedure(id: GLuint; value: GLenum;
      data: PGLboolean); extdecl;
  glGetLocalConstantIntegervEXT: procedure(id: GLuint; value: GLenum;
      data: PGLint); extdecl;
  glGetLocalConstantFloatvEXT: procedure(id: GLuint; value: GLenum;
      data: PGLfloat); extdecl;

//*** ATI_vertex_streams
const
  GL_MAX_VERTEX_STREAMS_ATI         = $876B;
  GL_VERTEX_STREAM0_ATI             = $876C;
  GL_VERTEX_STREAM1_ATI             = $876D;
  GL_VERTEX_STREAM2_ATI             = $876E;
  GL_VERTEX_STREAM3_ATI             = $876F;
  GL_VERTEX_STREAM4_ATI             = $8770;
  GL_VERTEX_STREAM5_ATI             = $8771;
  GL_VERTEX_STREAM6_ATI             = $8772;
  GL_VERTEX_STREAM7_ATI             = $8773;
  GL_VERTEX_SOURCE_ATI              = $8774;

var
  glVertexStream1s: procedure(stream: GLenum; coords: GLshort); extdecl;
  glVertexStream1i: procedure(stream: GLenum; coords: GLint); extdecl;
  glVertexStream1f: procedure(stream: GLenum; coords: GLfloat); extdecl;
  glVertexStream1d: procedure(stream: GLenum; coords: GLdouble); extdecl;
  glVertexStream2s: procedure(stream: GLenum; a, b: GLshort); extdecl;
  glVertexStream2i: procedure(stream: GLenum; a, b: GLint); extdecl;
  glVertexStream2f: procedure(stream: GLenum; a, b: GLfloat); extdecl;
  glVertexStream2d: procedure(stream: GLenum; a, b: GLdouble); extdecl;
  glVertexStream3s: procedure(stream: GLenum; a, b, c: GLshort); extdecl;
  glVertexStream3i: procedure(stream: GLenum; a, b, c: GLint); extdecl;
  glVertexStream3f: procedure(stream: GLenum; a, b, c: GLfloat); extdecl;
  glVertexStream3d: procedure(stream: GLenum; a, b, c: GLdouble); extdecl;
  glVertexStream4s: procedure(stream: GLenum; a, b, c, d: GLshort); extdecl;
  glVertexStream4i: procedure(stream: GLenum; a, b, c, d: GLint); extdecl;
  glVertexStream4f: procedure(stream: GLenum; a, b, c, d: GLfloat); extdecl;
  glVertexStream4d: procedure(stream: GLenum; a, b, c, d: GLdouble); extdecl;

  glVertexStream1sv: procedure(stream: GLenum; coords: PGLshort); extdecl;
  glVertexStream1iv: procedure(stream: GLenum; coords: PGLint); extdecl;
  glVertexStream1fv: procedure(stream: GLenum; coords: PGLfloat); extdecl;
  glVertexStream1dv: procedure(stream: GLenum; coords: PGLdouble); extdecl;
  glVertexStream2sv: procedure(stream: GLenum; coords: PGLshort); extdecl;
  glVertexStream2iv: procedure(stream: GLenum; coords: PGLint); extdecl;
  glVertexStream2fv: procedure(stream: GLenum; coords: PGLfloat); extdecl;
  glVertexStream2dv: procedure(stream: GLenum; coords: PGLdouble); extdecl;
  glVertexStream3sv: procedure(stream: GLenum; coords: PGLshort); extdecl;
  glVertexStream3iv: procedure(stream: GLenum; coords: PGLint); extdecl;
  glVertexStream3fv: procedure(stream: GLenum; coords: PGLfloat); extdecl;
  glVertexStream3dv: procedure(stream: GLenum; coords: PGLdouble); extdecl;
  glVertexStream4sv: procedure(stream: GLenum; coords: PGLshort); extdecl;
  glVertexStream4iv: procedure(stream: GLenum; coords: PGLint); extdecl;
  glVertexStream4fv: procedure(stream: GLenum; coords: PGLfloat); extdecl;
  glVertexStream4dv: procedure(stream: GLenum; coords: PGLdouble); extdecl;

  glNormalStream3b: procedure(stream: GLenum; x, y, z: GLbyte); extdecl;
  glNormalStream3s: procedure(stream: GLenum; x, y, z: GLshort); extdecl;
  glNormalStream3i: procedure(stream: GLenum; x, y, z: GLint); extdecl;
  glNormalStream3f: procedure(stream: GLenum; x, y, z: GLfloat); extdecl;
  glNormalStream3d: procedure(stream: GLenum; x, y, z: GLdouble); extdecl;

  glNormalStream3bv: procedure(stream: GLenum; coords: PGLbyte); extdecl;
  glNormalStream3sv: procedure(stream: GLenum; coords: PGLshort); extdecl;
  glNormalStream3iv: procedure(stream: GLenum; coords: PGLint); extdecl;
  glNormalStream3fv: procedure(stream: GLenum; coords: PGLfloat); extdecl;
  glNormalStream3dv: procedure(stream: GLenum; coords: PGLdouble); extdecl;

  glClientActiveVertexStream: procedure(stream: GLenum); extdecl;

  glVertexBlendEnvi: procedure(pname: GLenum; param: GLint); extdecl;
  glVertexBlendEnvf: procedure(pname: GLenum; param: GLfloat); extdecl;

//*** NV_depth_clamp
const
  GL_DEPTH_CLAMP_NV                 = $864F;

//*** NV_multisample_filter_hint
const
  GL_MULTISAMPLE_FILTER_HINT_NV     = $8534;

//*** NV_occlusion_query
const
//  GL_OCCLUSION_TEST_HP              = $8165;
//  GL_OCCLUSION_TEST_RESULT_HP       = $8166;
  GL_PIXEL_COUNTER_BITS_NV          = $8864;
  GL_CURRENT_OCCLUSION_QUERY_ID_NV  = $8865;
  GL_PIXEL_COUNT_NV                 = $8866;
  GL_PIXEL_COUNT_AVAILABLE_NV       = $8867;

var
  glGenOcclusionQueriesNV: procedure(n: GLsizei; ids: PGLuint); extdecl;
  glDeleteOcclusionQueriesNV: procedure(n: GLsizei; const ids: PGLuint); extdecl;
  glIsOcclusionQueryNV: function(id: GLuint): GLboolean; extdecl;
  glBeginOcclusionQueryNV: procedure(id: GLuint); extdecl;
  glEndOcclusionQueryNV: procedure; extdecl;
  glGetOcclusionQueryivNV: procedure(id: GLuint; pname: GLenum; params: PGLint); extdecl;
  glGetOcclusionQueryuivNV: procedure(id: GLuint; pname: GLenum; params: PGLuint); extdecl;

//*** NV_point_sprite
const
  GL_POINT_SPRITE_NV                = $8861;
  GL_COORD_REPLACE_NV               = $8862;
//  GL_FALSE
//  GL_TRUE
  GL_POINT_SPRITE_R_MODE_NV         = $8863;
//  GL_ZERO
//  GL_S
//  GL_R

var
  glPointParameteriNV: procedure(pname: GLenum; param: GLint); extdecl;
  glPointParameterivNV: procedure(pname: GLenum; const params: PGLint); extdecl;

//*** NV_texture_shader3
const
  GL_OFFSET_PROJECTIVE_TEXTURE_2D_NV = $8850;
  GL_OFFSET_PROJECTIVE_TEXTURE_2D_SCALE_NV = $8851;
  GL_OFFSET_PROJECTIVE_TEXTURE_RECTANGLE_NV = $8852;
  GL_OFFSET_PROJECTIVE_TEXTURE_RECTANGLE_SCALE_NV = $8853;
  GL_OFFSET_HILO_TEXTURE_2D_NV      = $8854;
  GL_OFFSET_HILO_TEXTURE_RECTANGLE_NV = $8855;
  GL_OFFSET_HILO_PROJECTIVE_TEXTURE_2D_NV = $8856;
  GL_OFFSET_HILO_PROJECTIVE_TEXTURE_RECTANGLE_NV = $8857;
  GL_DEPENDENT_HILO_TEXTURE_2D_NV   = $8858;
  GL_DEPENDENT_RGB_TEXTURE_3D_NV    = $8859;
  GL_DEPENDENT_RGB_TEXTURE_CUBE_MAP_NV = $885A;
  GL_DOT_PRODUCT_PASS_THROUGH_NV    = $885B;
  GL_DOT_PRODUCT_TEXTURE_1D_NV      = $885C;
  GL_DOT_PRODUCT_AFFINE_DEPTH_REPLACE_NV = $885D;
  GL_HILO8_NV                       = $885E;
  GL_SIGNED_HILO8_NV                = $885F;
  GL_FORCE_BLUE_TO_ONE_NV           = $8860;

//*** ARB_vertex_blend
const
  GL_MAX_VERTEX_UNITS_ARB           = $86A4;
  GL_ACTIVE_VERTEX_UNITS_ARB        = $86A5;
  GL_WEIGHT_SUM_UNITY_ARB           = $86A6;
  GL_VERTEX_BLEND_ARB               = $86A7;
  GL_MODELVIEW0_ARB                 = $1700;
  GL_MODELVIEW1_ARB                 = $850a;
  GL_MODELVIEW2_ARB                 = $8722;
  GL_MODELVIEW3_ARB                 = $8723;
  GL_MODELVIEW4_ARB                 = $8724;
  GL_MODELVIEW5_ARB                 = $8725;
  GL_MODELVIEW6_ARB                 = $8726;
  GL_MODELVIEW7_ARB                 = $8727;
  GL_MODELVIEW8_ARB                 = $8728;
  GL_MODELVIEW9_ARB                 = $8729;
  GL_MODELVIEW10_ARB                = $872A;
  GL_MODELVIEW11_ARB                = $872B;
  GL_MODELVIEW12_ARB                = $872C;
  GL_MODELVIEW13_ARB                = $872D;
  GL_MODELVIEW14_ARB                = $872E;
  GL_MODELVIEW15_ARB                = $872F;
  GL_MODELVIEW16_ARB                = $8730;
  GL_MODELVIEW17_ARB                = $8731;
  GL_MODELVIEW18_ARB                = $8732;
  GL_MODELVIEW19_ARB                = $8733;
  GL_MODELVIEW20_ARB                = $8734;
  GL_MODELVIEW21_ARB                = $8735;
  GL_MODELVIEW22_ARB                = $8736;
  GL_MODELVIEW23_ARB                = $8737;
  GL_MODELVIEW24_ARB                = $8738;
  GL_MODELVIEW25_ARB                = $8739;
  GL_MODELVIEW26_ARB                = $873A;
  GL_MODELVIEW27_ARB                = $873B;
  GL_MODELVIEW28_ARB                = $873C;
  GL_MODELVIEW29_ARB                = $873D;
  GL_MODELVIEW30_ARB                = $873E;
  GL_MODELVIEW31_ARB                = $873F;
  GL_CURRENT_WEIGHT_ARB             = $86A8;
  GL_WEIGHT_ARRAY_TYPE_ARB          = $86A9;
  GL_WEIGHT_ARRAY_STRIDE_ARB        = $86AA;
  GL_WEIGHT_ARRAY_SIZE_ARB          = $86AB;
  GL_WEIGHT_ARRAY_POINTER_ARB       = $86AC;
  GL_WEIGHT_ARRAY_ARB               = $86AD;

var
  glWeightbvARB: procedure(size: GLint; weights: GLbyte); extdecl;
  glWeightsvARB: procedure(size: GLint; weights: GLshort); extdecl;
  glWeightivARB: procedure(size: GLint; weights: GLint); extdecl;
  glWeightfvARB: procedure(size: GLint; weights: GLfloat); extdecl;
  glWeightdvARB: procedure(size: GLint; weights: GLdouble); extdecl;
  glWeightubvARB: procedure(size: GLint; weights: GLubyte); extdecl;
  glWeightusvARB: procedure(size: GLint; weights: GLushort); extdecl;
  glWeightuivARB: procedure(size: GLint; weights: GLuint); extdecl;
  glWeightPointerARB: procedure(size: GLint; atype: GLenum; stride: GLsizei;
                                ptr: Pointer); extdecl;
  glVertexBlendARB: procedure(count: GLint); extdecl;

//*** ARB_matrix_palette
const
  GL_MATRIX_PALETTE_ARB             = $8840;
  GL_MAX_MATRIX_PALETTE_STACK_DEPTH_ARB = $8841;
  GL_MAX_PALETTE_MATRICES_ARB       = $8842;
  GL_CURRENT_PALETTE_MATRIX_ARB     = $8843;
  GL_MATRIX_INDEX_ARRAY_ARB         = $8844;
  GL_CURRENT_MATRIX_INDEX_ARB       = $8845;
  GL_MATRIX_INDEX_ARRAY_SIZE_ARB    = $8846;
  GL_MATRIX_INDEX_ARRAY_TYPE_ARB    = $8847;
  GL_MATRIX_INDEX_ARRAY_STRIDE_ARB  = $8848;
  GL_MATRIX_INDEX_ARRAY_POINTER_ARB = $8849;

var
  glCurrentPaletteMatrixARB: procedure(index: GLint); extdecl;
  glMatrixIndexubvARB: procedure(size: GLint; indices: PGLubyte); extdecl;
  glMatrixIndexusvARB: procedure(size: GLint; indices: PGLushort); extdecl;
  glMatrixIndexuivARB: procedure(size: GLint; indices: PGLuint); extdecl;
  glMatrixIndexPointerARB: procedure(size: Integer; atype: GLenum;
                                     stride: GLsizei; ptr: Pointer); extdecl;

//*** ARB_texture_mirrored_repeat
const
  GL_MIRRORED_REPEAT_ARB            = $8370;

//*** ARB_depth_texture
const
  GL_DEPTH_COMPONENT16_ARB          = $81A5;
  GL_DEPTH_COMPONENT24_ARB          = $81A6;
  GL_DEPTH_COMPONENT32_ARB          = $81A7;
  GL_TEXTURE_DEPTH_SIZE_ARB         = $884A;
  GL_DEPTH_TEXTURE_MODE_ARB         = $884B;

//*** ARB_shadow
const
  GL_TEXTURE_COMPARE_MODE_ARB       = $884C;
  GL_TEXTURE_COMPARE_FUNC_ARB       = $884D;
  GL_COMPARE_R_TO_TEXTURE_ARB       = $884E;

//*** ARB_shadow_ambient
const
  GL_TEXTURE_COMPARE_FAIL_VALUE_ARB = $80BF;

//*** ARB_window_pos
var
  glWindowPos2dARB: procedure(x, y: GLdouble); extdecl;
  glWindowPos2fARB: procedure(x, y: GLfloat); extdecl;
  glWindowPos2iARB: procedure(x, y: GLint); extdecl;
  glWindowPos2sARB: procedure(x, y: GLshort); extdecl;

  glWindowPos2dvARB: procedure(const p: PGLdouble); extdecl;
  glWindowPos2fvARB: procedure(const p: PGLfloat); extdecl;
  glWindowPos2ivARB: procedure(const p: PGLint); extdecl;
  glWindowPos2svARB: procedure(const p: PGLshort); extdecl;

  glWindowPos3dARB: procedure(x, y, z: GLdouble); extdecl;
  glWindowPos3fARB: procedure(x, y, z: GLfloat); extdecl;
  glWindowPos3iARB: procedure(x, y, z: GLint); extdecl;
  glWindowPos3sARB: procedure(x, y, z: GLshort); extdecl;

  glWindowPos3dvARB: procedure(const p: PGLdouble); extdecl;
  glWindowPos3fvARB: procedure(const p: PGLfloat); extdecl;
  glWindowPos3ivARB: procedure(const p: PGLint); extdecl;
  glWindowPos3svARB: procedure(const p: PGLshort); extdecl;

//*** ARB_vertex_program
var
  glVertexAttrib1sARB: procedure(index: GLuint; x: GLshort); extdecl;
  glVertexAttrib1fARB: procedure(index: GLuint; x: GLfloat); extdecl;
  glVertexAttrib1dARB: procedure(index: GLuint; x: GLdouble); extdecl;
  glVertexAttrib2sARB: procedure(index: GLuint; x, y: GLshort); extdecl;
  glVertexAttrib2fARB: procedure(index: GLuint; x, y: GLfloat); extdecl;
  glVertexAttrib2dARB: procedure(index: GLuint; x, y: GLdouble); extdecl;
  glVertexAttrib3sARB: procedure(index: GLuint; x, y, z: GLshort); extdecl;
  glVertexAttrib3fARB: procedure(index: GLuint; x, y, z: GLfloat); extdecl;
  glVertexAttrib3dARB: procedure(index: GLuint; x, y, z: GLdouble); extdecl;
  glVertexAttrib4sARB: procedure(index: GLuint; x, y, z, w: GLshort); extdecl;
  glVertexAttrib4fARB: procedure(index: GLuint; x, y, z, w: GLfloat); extdecl;
  glVertexAttrib4dARB: procedure(index: GLuint; x, y, z, w: GLdouble); extdecl;
  glVertexAttrib4NubARB: procedure(index: GLuint; x, y, z, w: GLubyte); extdecl;

  glVertexAttrib1svARB: procedure(index: GLuint; const v: PGLshort); extdecl;
  glVertexAttrib1fvARB: procedure(index: GLuint; const v: PGLfloat); extdecl;
  glVertexAttrib1dvARB: procedure(index: GLuint; const v: PGLdouble); extdecl;
  glVertexAttrib2svARB: procedure(index: GLuint; const v: PGLshort); extdecl;
  glVertexAttrib2fvARB: procedure(index: GLuint; const v: PGLfloat); extdecl;
  glVertexAttrib2dvARB: procedure(index: GLuint; const v: PGLdouble); extdecl;
  glVertexAttrib3svARB: procedure(index: GLuint; const v: PGLshort); extdecl;
  glVertexAttrib3fvARB: procedure(index: GLuint; const v: PGLfloat); extdecl;
  glVertexAttrib3dvARB: procedure(index: GLuint; const v: PGLdouble); extdecl;
  glVertexAttrib4bvARB: procedure(index: GLuint; const v: PGLbyte); extdecl;
  glVertexAttrib4svARB: procedure(index: GLuint; const v: PGLshort); extdecl;
  glVertexAttrib4ivARB: procedure(index: GLuint; const v: PGLint); extdecl;
  glVertexAttrib4ubvARB: procedure(index: GLuint; const v: PGLubyte); extdecl;
  glVertexAttrib4usvARB: procedure(index: GLuint; const v: PGLushort); extdecl;
  glVertexAttrib4uivARB: procedure(index: GLuint; const v: PGLuint); extdecl;
  glVertexAttrib4fvARB: procedure(index: GLuint; const v: PGLfloat); extdecl;
  glVertexAttrib4dvARB: procedure(index: GLuint; const v: PGLdouble); extdecl;
  glVertexAttrib4NbvARB: procedure(index: GLuint; const v: PGLbyte); extdecl;
  glVertexAttrib4NsvARB: procedure(index: GLuint; const v: PGLshort); extdecl;
  glVertexAttrib4NivARB: procedure(index: GLuint; const v: PGLint); extdecl;
  glVertexAttrib4NubvARB: procedure(index: GLuint; const v: PGLubyte); extdecl;
  glVertexAttrib4NusvARB: procedure(index: GLuint; const v: PGLushort); extdecl;
  glVertexAttrib4NuivARB: procedure(index: GLuint; const v: PGLuint); extdecl;

  glVertexAttribPointerARB: procedure(index: GLuint; size: Integer; atype: GLenum; 
                                      normalized: GLboolean; stride: GLsizei;
                                      const ptr: Pointer); extdecl;

  glEnableVertexAttribArrayARB: procedure(index: GLuint); extdecl;
  glDisableVertexAttribArrayARB: procedure(index: GLuint); extdecl;

  glProgramStringARB: procedure(target, format: GLenum; len: GLsizei;
                                const str: Pointer); extdecl;

  glBindProgramARB: procedure(target: GLenum; prog: GLuint); extdecl;

  glDeleteProgramsARB: procedure(n: GLsizei; const programs: PGLuint); extdecl;

  glGenProgramsARB: procedure(n: GLsizei; programs: PGLuint); extdecl;

  glProgramEnvParameter4dARB: procedure(target: GLenum; index: GLuint;
                                        x, y, z, w: GLdouble); extdecl;
  glProgramEnvParameter4dvARB: procedure(target: GLenum; index: GLuint;
                                         const params: PGLdouble); extdecl;
  glProgramEnvParameter4fARB: procedure(target: GLenum; index: GLuint;
                                        x, y, z, w: GLfloat); extdecl;
  glProgramEnvParameter4fvARB: procedure(target: GLenum; index: GLuint;
                                         const params: PGLfloat); extdecl;

  glProgramLocalParameter4dARB: procedure(target: GLenum; index: GLuint;
                                          x, y, z, w: GLdouble); extdecl;
  glProgramLocalParameter4dvARB: procedure(target: GLenum; index: GLuint;
                                           const params: PGLdouble); extdecl;
  glProgramLocalParameter4fARB: procedure(target: GLenum; index: GLuint;
                                          x, y, z, w: GLfloat); extdecl;
  glProgramLocalParameter4fvARB: procedure(target: GLenum; index: GLuint;
                                           const params: PGLfloat); extdecl;

  glGetProgramEnvParameterdvARB: procedure(target: GLenum; index: GLuint;
                                           params: PGLdouble); extdecl;
  glGetProgramEnvParameterfvARB: procedure(target: GLenum; index: GLuint; 
                                           params: PGLfloat); extdecl;

  glGetProgramLocalParameterdvARB: procedure(target: GLenum; index: GLuint;
                                             params: PGLdouble); extdecl;
  glGetProgramLocalParameterfvARB: procedure(target: GLenum; index: GLuint;
                                             params: PGLdouble); extdecl;

  glGetProgramivARB: procedure(target, pname: GLenum; params: PGLint); extdecl;

  glGetProgramStringARB: procedure(target, pname: GLenum; str: Pointer); extdecl;

  glGetVertexAttribdvARB: procedure(index: GLuint; pname: GLenum; params: PGLdouble); extdecl;
  glGetVertexAttribfvARB: procedure(index: GLuint; pname: GLenum; params: PGLfloat); extdecl;
  glGetVertexAttribivARB: procedure(index: GLuint; pname: GLenum; params: PGLint); extdecl;

  glGetVertexAttribPointervARB: procedure(index: GLuint; pname: GLenum; ptr: PPointer); extdecl;

  glIsProgramARB: function(prog: GLuint): GLboolean; extdecl;

const
  GL_VERTEX_PROGRAM_ARB             = $8620;
  GL_VERTEX_PROGRAM_POINT_SIZE_ARB  = $8642;
  GL_VERTEX_PROGRAM_TWO_SIDE_ARB    = $8643;
  GL_COLOR_SUM_ARB                  = $8458;
  GL_PROGRAM_FORMAT_ASCII_ARB       = $8875;
  GL_VERTEX_ATTRIB_ARRAY_ENABLED_ARB = $8622;
  GL_VERTEX_ATTRIB_ARRAY_SIZE_ARB   = $8623;
  GL_VERTEX_ATTRIB_ARRAY_STRIDE_ARB = $8624;
  GL_VERTEX_ATTRIB_ARRAY_TYPE_ARB   = $8625;
  GL_VERTEX_ATTRIB_ARRAY_NORMALIZED_ARB = $886A;
  GL_CURRENT_VERTEX_ATTRIB_ARB      = $8626;
  GL_VERTEX_ATTRIB_ARRAY_POINTER_ARB = $8645;
  GL_PROGRAM_LENGTH_ARB             = $8627;
  GL_PROGRAM_FORMAT_ARB             = $8876;
  GL_PROGRAM_BINDING_ARB            = $8677;
  GL_PROGRAM_INSTRUCTIONS_ARB       = $88A0;
  GL_MAX_PROGRAM_INSTRUCTIONS_ARB   = $88A1;
  GL_PROGRAM_NATIVE_INSTRUCTIONS_ARB = $88A2;
  GL_MAX_PROGRAM_NATIVE_INSTRUCTIONS_ARB = $88A3;
  GL_PROGRAM_TEMPORARIES_ARB        = $88A4;
  GL_MAX_PROGRAM_TEMPORARIES_ARB    = $88A5;
  GL_PROGRAM_NATIVE_TEMPORARIES_ARB = $88A6;
  GL_MAX_PROGRAM_NATIVE_TEMPORARIES_ARB = $88A7;
  GL_PROGRAM_PARAMETERS_ARB         = $88A8;
  GL_MAX_PROGRAM_PARAMETERS_ARB     = $88A9;
  GL_PROGRAM_NATIVE_PARAMETERS_ARB  = $88AA;
  GL_MAX_PROGRAM_NATIVE_PARAMETERS_ARB = $88AB;
  GL_PROGRAM_ATTRIBS_ARB            = $88AC;
  GL_MAX_PROGRAM_ATTRIBS_ARB        = $88AD;
  GL_PROGRAM_NATIVE_ATTRIBS_ARB     = $88AE;
  GL_MAX_PROGRAM_NATIVE_ATTRIBS_ARB = $88AF;
  GL_PROGRAM_ADDRESS_REGISTERS_ARB  = $88B0;
  GL_MAX_PROGRAM_ADDRESS_REGISTERS_ARB = $88B1;
  GL_PROGRAM_NATIVE_ADDRESS_REGISTERS_ARB = $88B2;
  GL_MAX_PROGRAM_NATIVE_ADDRESS_REGISTERS_ARB = $88B3;
  GL_MAX_PROGRAM_LOCAL_PARAMETERS_ARB = $88B4;
  GL_MAX_PROGRAM_ENV_PARAMETERS_ARB = $88B5;
  GL_PROGRAM_UNDER_NATIVE_LIMITS_ARB = $88B6;
  GL_PROGRAM_STRING_ARB             = $8628;
  GL_PROGRAM_ERROR_POSITION_ARB     = $864B;
  GL_CURRENT_MATRIX_ARB             = $8641;
  GL_TRANSPOSE_CURRENT_MATRIX_ARB   = $88B7;
  GL_CURRENT_MATRIX_STACK_DEPTH_ARB = $8640;
  GL_MAX_VERTEX_ATTRIBS_ARB         = $8869;
  GL_MAX_PROGRAM_MATRICES_ARB       = $862F;
  GL_MAX_PROGRAM_MATRIX_STACK_DEPTH_ARB = $862E;
  GL_PROGRAM_ERROR_STRING_ARB       = $8874;
  GL_MATRIX0_ARB                    = $88C0;
  GL_MATRIX1_ARB                    = $88C1;
  GL_MATRIX2_ARB                    = $88C2;
  GL_MATRIX3_ARB                    = $88C3;
  GL_MATRIX4_ARB                    = $88C4;
  GL_MATRIX5_ARB                    = $88C5;
  GL_MATRIX6_ARB                    = $88C6;
  GL_MATRIX7_ARB                    = $88C7;
  GL_MATRIX8_ARB                    = $88C8;
  GL_MATRIX9_ARB                    = $88C9;
  GL_MATRIX10_ARB                   = $88CA;
  GL_MATRIX11_ARB                   = $88CB;
  GL_MATRIX12_ARB                   = $88CC;
  GL_MATRIX13_ARB                   = $88CD;
  GL_MATRIX14_ARB                   = $88CE;
  GL_MATRIX15_ARB                   = $88CF;
  GL_MATRIX16_ARB                   = $88D0;
  GL_MATRIX17_ARB                   = $88D1;
  GL_MATRIX18_ARB                   = $88D2;
  GL_MATRIX19_ARB                   = $88D3;
  GL_MATRIX20_ARB                   = $88D4;
  GL_MATRIX21_ARB                   = $88D5;
  GL_MATRIX22_ARB                   = $88D6;
  GL_MATRIX23_ARB                   = $88D7;
  GL_MATRIX24_ARB                   = $88D8;
  GL_MATRIX25_ARB                   = $88D9;
  GL_MATRIX26_ARB                   = $88DA;
  GL_MATRIX27_ARB                   = $88DB;
  GL_MATRIX28_ARB                   = $88DC;
  GL_MATRIX29_ARB                   = $88DD;
  GL_MATRIX30_ARB                   = $88DE;
  GL_MATRIX31_ARB                   = $88DF;

//*** EXT_stencil_two_side
var
  glActiveStencilFaceEXT: procedure(face: GLenum); extdecl;

const
  GL_STENCIL_TEST_TWO_SIDE_EXT      = $8910;
  GL_ACTIVE_STENCIL_FACE_EXT        = $8911;

//*** NV_element_array
var
  glElementPointerNV: procedure(atype: GLenum; const pointer: Pointer); extdecl;
  glDrawElementArrayNV: procedure(mode: GLenum; first: GLint; count: GLsizei); extdecl;
  glDrawRangeElementArrayNV: procedure(mode: GLenum; start, aend: GLuint;
                                       first: GLint; count: GLsizei); extdecl;
  glMultiDrawElementArrayNV: procedure(mode: GLenum; const first: PGLint;
                                       const count: PGLsizei;
                                       primcount: GLsizei); extdecl;
  glMultiDrawRangeElementArrayNV: procedure(mode: GLenum; start, aend: GLuint;
                                            const first: PGLint;
                                            const count: PGLsizei;
                                            primcount: GLsizei); extdecl;

const
  GL_ELEMENT_ARRAY_TYPE_NV          = $8769;
  GL_ELEMENT_ARRAY_POINTER_NV       = $876A;

//*** NV_float_buffer
const
  GL_FLOAT_R_NV                     = $8880;
  GL_FLOAT_RG_NV                    = $8881;
  GL_FLOAT_RGB_NV                   = $8882;
  GL_FLOAT_RGBA_NV                  = $8883;
  GL_FLOAT_R16_NV                   = $8884;
  GL_FLOAT_R32_NV                   = $8885;
  GL_FLOAT_RG16_NV                  = $8886;
  GL_FLOAT_RG32_NV                  = $8887;
  GL_FLOAT_RGB16_NV                 = $8888;
  GL_FLOAT_RGB32_NV                 = $8889;
  GL_FLOAT_RGBA16_NV                = $888A;
  GL_FLOAT_RGBA32_NV                = $888B;
  GL_TEXTURE_FLOAT_COMPONENTS_NV    = $888C;
  GL_FLOAT_CLEAR_COLOR_VALUE_NV     = $888D;
  GL_FLOAT_RGBA_MODE_NV             = $888E;
  {$IFDEF Win32}
  WGL_FLOAT_COMPONENTS_NV           = $20B0;
  WGL_BIND_TO_TEXTURE_RECTANGLE_FLOAT_R_NV = $20B1;
  WGL_BIND_TO_TEXTURE_RECTANGLE_FLOAT_RG_NV = $20B2;
  WGL_BIND_TO_TEXTURE_RECTANGLE_FLOAT_RGB_NV = $20B3;
  WGL_BIND_TO_TEXTURE_RECTANGLE_FLOAT_RGBA_NV = $20B4;
  WGL_TEXTURE_FLOAT_R_NV            = $20B5;
  WGL_TEXTURE_FLOAT_RG_NV           = $20B6;
  WGL_TEXTURE_FLOAT_RGB_NV          = $20B7;
  WGL_TEXTURE_FLOAT_RGBA_NV         = $20B8;
  {$ENDIF}

//*** NV_fragment_program
var
  glProgramNamedParameter4fNV: procedure(id: GLuint; len: GLsizei;
        const name: PGLubyte; x, y, z, w: GLfloat); extdecl;
  glProgramNamedParameter4dNV: procedure(id: GLuint; len: GLsizei;
        const name: PGLubyte; x, y, z, w: GLdouble); extdecl;
  glProgramNamedParameter4fvNV: procedure(id: GLuint; len: GLsizei;
        const name: PGLubyte; const v: PGLfloat); extdecl;
  glProgramNamedParameter4dvNV: procedure(id: GLuint; len: GLsizei;
        const name: PGLubyte; const v: PGLdouble); extdecl;
  glGetProgramNamedParameterfvNV: procedure(id: GLuint; len: GLsizei;
        const name: PGLubyte; params: PGLfloat); extdecl;
  glGetProgramNamedParameterdvNV: procedure(id: GLuint; len: GLsizei;
        const name: PGLubyte; params: PGLdouble); extdecl;
  // glProgramLocalParameter4dARB
  // glProgramLocalParameter4dvARB
  // glProgramLocalParameter4fARB
  // glProgramLocalParameter4fvARB
  // glGetProgramLocalParameterdvARB
  // glGetProgramLocalParameterfvARB

const
  GL_FRAGMENT_PROGRAM_NV            = $8870;
  GL_MAX_TEXTURE_COORDS_NV          = $8871;
  GL_MAX_TEXTURE_IMAGE_UNITS_NV     = $8872;
  GL_FRAGMENT_PROGRAM_BINDING_NV    = $8873;
  GL_MAX_FRAGMENT_PROGRAM_LOCAL_PARAMETERS_NV = $8868;
  GL_PROGRAM_ERROR_STRING_NV        = $8874;

//*** NV_primitive_restart
var
  glPrimitiveRestartNV: procedure; extdecl;
  glPrimitiveRestartIndexNV: procedure(index: GLuint); extdecl;

const
  GL_PRIMITIVE_RESTART_NV           = $8558;
  GL_PRIMITIVE_RESTART_INDEX_NV     = $8559;

{******************************************************************************}

{$IFDEF Win32}
//*** WGL_ARB_buffer_region
const
  WGL_FRONT_COLOR_BUFFER_BIT_ARB    = $00000001;
  WGL_BACK_COLOR_BUFFER_BIT_ARB     = $00000002;
  WGL_DEPTH_BUFFER_BIT_ARB	    = $00000004;
  WGL_STENCIL_BUFFER_BIT_ARB        = $00000008;

var
  wglCreateBufferRegionARB: function(dc: HDC; iLayerPlane: Integer;
    uType: UINT): THandle; extdecl;
  wglDeleteBufferRegionARB: procedure(hRegion: THandle); extdecl;
  wglSaveBufferRegionARB: function(hRegion: THandle;
    x, y, width, height: Integer): BOOL; extdecl;
  wglRestoreBufferRegionARB: function(hRegion: THandle;
    x, y, width, height, xSrc, ySrc: Integer): BOOL; extdecl;

//*** WGL_ARB_extensions_string
var
  wglGetExtensionsStringARB: function(dc: HDC): PChar; extdecl;

//*** WGL_ARB_pixel_format
const
  WGL_NUMBER_PIXEL_FORMATS_ARB      = $2000;
  WGL_DRAW_TO_WINDOW_ARB            = $2001;
  WGL_DRAW_TO_BITMAP_ARB            = $2002;
  WGL_ACCELERATION_ARB              = $2003;
  WGL_NEED_PALETTE_ARB              = $2004;
  WGL_NEED_SYSTEM_PALETTE_ARB       = $2005;
  WGL_SWAP_LAYER_BUFFERS_ARB        = $2006;
  WGL_SWAP_METHOD_ARB	            = $2007;
  WGL_NUMBER_OVERLAYS_ARB           = $2008;
  WGL_NUMBER_UNDERLAYS_ARB          = $2009;
  WGL_TRANSPARENT_ARB	            = $200A;
  WGL_TRANSPARENT_RED_VALUE_ARB     = $2037;
  WGL_TRANSPARENT_GREEN_VALUE_ARB   = $2038;
  WGL_TRANSPARENT_BLUE_VALUE_ARB    = $2039;
  WGL_TRANSPARENT_ALPHA_VALUE_ARB   = $203A;
  WGL_TRANSPARENT_INDEX_VALUE_ARB   = $203B;
  WGL_SHARE_DEPTH_ARB               = $200C;
  WGL_SHARE_STENCIL_ARB             = $200D;
  WGL_SHARE_ACCUM_ARB		    = $200E;
  WGL_SUPPORT_GDI_ARB		    = $200F;
  WGL_SUPPORT_OPENGL_ARB	    = $2010;
  WGL_DOUBLE_BUFFER_ARB 	    = $2011;
  WGL_STEREO_ARB		    = $2012;
  WGL_PIXEL_TYPE_ARB		    = $2013;
  WGL_COLOR_BITS_ARB		    = $2014;
  WGL_RED_BITS_ARB		    = $2015;
  WGL_RED_SHIFT_ARB		    = $2016;
  WGL_GREEN_BITS_ARB		    = $2017;
  WGL_GREEN_SHIFT_ARB		    = $2018;
  WGL_BLUE_BITS_ARB		    = $2019;
  WGL_BLUE_SHIFT_ARB		    = $201A;
  WGL_ALPHA_BITS_ARB		    = $201B;
  WGL_ALPHA_SHIFT_ARB		    = $201C;
  WGL_ACCUM_BITS_ARB		    = $201D;
  WGL_ACCUM_RED_BITS_ARB	    = $201E;
  WGL_ACCUM_GREEN_BITS_ARB	    = $201F;
  WGL_ACCUM_BLUE_BITS_ARB	    = $2020;
  WGL_ACCUM_ALPHA_BITS_ARB	    = $2021;
  WGL_DEPTH_BITS_ARB		    = $2022;
  WGL_STENCIL_BITS_ARB		    = $2023;
  WGL_AUX_BUFFERS_ARB		    = $2024;

  WGL_NO_ACCELERATION_ARB           = $2025;
  WGL_GENERIC_ACCELERATION_ARB      = $2026;
  WGL_FULL_ACCELERATION_ARB         = $2027;

  WGL_SWAP_EXCHANGE_ARB             = $2028;
  WGL_SWAP_COPY_ARB                 = $2029;
  WGL_SWAP_UNDEFINED_ARB            = $202A;

  WGL_TYPE_RGBA_ARB                 = $202B;
  WGL_TYPE_COLORINDEX_ARB           = $202C;

var
  wglGetPixelFormatAttribivARB: function(dc: HDC;
    iPixelFormat, iLayerPlane: Integer; nAttributes: UINT;
      const piAttributes, piValues: PInteger): BOOL; extdecl;
  wglGetPixelFormatAttribfvARB: function(dc: HDC;
    iPixelFormat, iLayerPlane: Integer; nAttributes: UINT;
      const piAttributes, piValues: PInteger; pfValues: PSingle): BOOL; extdecl;
  wglChoosePixelFormatARB: function(dc: HDC; const piAttribIList: PInteger;
    const pfAttribFList: PSingle; nMaxFormats: UINT; piFormats: PInteger;
    nNumFormats: PUINT): BOOL; extdecl;

//*** WGL_ARB_make_current_read
const
  GL_ERROR_INVALID_PIXEL_TYPE_ARB   = $2043;
  GL_ERROR_INCOMPATIBLE_DEVICE_CONTEXTS_ARB = $2054;

var
  wglMakeContextCurrentARB: function(hDrawDC, hReadDC: HDC; glrc: HGLRC): BOOL; extdecl;
  wglGetCurrentReadDCARB: function: HDC; extdecl;

//*** WGL_ARB_pbuffer
const
  WGL_DRAW_TO_PBUFFER_ARB	    = $202D;
  WGL_MAX_PBUFFER_PIXELS_ARB	    = $202E;
  WGL_MAX_PBUFFER_WIDTH_ARB	    = $202F;
  WGL_MAX_PBUFFER_HEIGHT_ARB	    = $2030;
  WGL_PBUFFER_LARGEST_ARB	    = $2033;
  WGL_PBUFFER_WIDTH_ARB 	    = $2034;
  WGL_PBUFFER_HEIGHT_ARB	    = $2035;
  WGL_PBUFFER_LOST_ARB		    = $2036;

type
  HPBUFFERARB = THandle;

var
  wglCreatePbufferARB: function(DC: HDC;
    iPixelFormat, iWidth, iHeight: Integer;
    const piAttribList: PInteger): HPBUFFERARB; extdecl;
  wglGetPbufferDCARB: function(hPbuffer: HPBUFFERARB): HDC; extdecl;
  wglReleasePbufferDCARB: function(hPbuffer: HPBUFFERARB; DC: HDC): Integer; extdecl;
  wglDestroyPbufferARB: function(hPbuffer: HPBUFFERARB): BOOL; extdecl;
  wglQueryPbufferARB: function(hPbuffer: HPBUFFERARB; iAttribute: Integer;
    piValue: PInteger): BOOL; extdecl;

//*** WGL_EXT_swap_control
var
  wglSwapIntervalEXT: function(interval: GLint): Integer; extdecl;

//*** WGL_ARB_multisample
const
  WGL_SAMPLE_BUFFERS_ARB            = $2041;
  WGL_SAMPLES_ARB                   = $2042;

//*** WGL_OML_sync_control
type
  PINT64 = ^INT64;

var
  wglGetSyncValuesOML: function(hdc: HDC; ust, msc, sbc: PINT64): BOOL; extdecl;
  wglGetMscRateOML: function(hdc: HDC;
      numerator, denominator: PInteger): BOOL; extdecl;
  wglSwapBuffersMscOML: function(hdc: HDC;
      target_msc, divisor, remainder: INT64): INT64; extdecl;
  wglSwapLayerBuffersMscOML: function(hdc: HDC; fuPlanes: Integer;
      target_msc, divisor, remainder: INT64): INT64; extdecl;
  wglWaitForMscOML: function(hdc: HDC; target_msc, divisor, remainder: INT64;
                             ust, msc, sbc: PINT64): BOOL; extdecl;
  wglWaitForSbcOML: function(hdc: HDC; target_sbc: INT64;
      ust, msc, sbc: PINT64): BOOL; extdecl;

//*** WGL_ARB_render_texture
const
  WGL_BIND_TO_TEXTURE_RGB_ARB       = $2070;
  WGL_BIND_TO_TEXTURE_RGBA_ARB      = $2071;
  WGL_TEXTURE_FORMAT_ARB            = $2072;
  WGL_TEXTURE_TARGET_ARB            = $2073;
  WGL_MIPMAP_TEXTURE_ARB            = $2074;
  WGL_TEXTURE_RGB_ARB               = $2075;
  WGL_TEXTURE_RGBA_ARB              = $2076;
  WGL_NO_TEXTURE_ARB                = $2077;
  WGL_TEXTURE_CUBE_MAP_ARB          = $2078;
  WGL_TEXTURE_1D_ARB                = $2079;
  WGL_TEXTURE_2D_ARB                = $207A;
//  WGL_NO_TEXTURE_ARB                = $2077;
  WGL_MIPMAP_LEVEL_ARB              = $207B;
  WGL_CUBE_MAP_FACE_ARB             = $207C;
  WGL_TEXTURE_CUBE_MAP_POSITIVE_X_ARB = $207D;
  WGL_TEXTURE_CUBE_MAP_NEGATIVE_X_ARB = $207E;
  WGL_TEXTURE_CUBE_MAP_POSITIVE_Y_ARB = $207F;
  WGL_TEXTURE_CUBE_MAP_NEGATIVE_Y_ARB = $2080;
  WGL_TEXTURE_CUBE_MAP_POSITIVE_Z_ARB = $2081;
  WGL_TEXTURE_CUBE_MAP_NEGATIVE_Z_ARB = $2082;
  WGL_FRONT_LEFT_ARB                = $2083;
  WGL_FRONT_RIGHT_ARB               = $2084;
  WGL_BACK_LEFT_ARB                 = $2085;
  WGL_BACK_RIGHT_ARB                = $2086;
  WGL_AUX0_ARB                      = $2087;
  WGL_AUX1_ARB                      = $2088;
  WGL_AUX2_ARB                      = $2089;
  WGL_AUX3_ARB                      = $208A;
  WGL_AUX4_ARB                      = $208B;
  WGL_AUX5_ARB                      = $208C;
  WGL_AUX6_ARB                      = $208D;
  WGL_AUX7_ARB                      = $208E;
  WGL_AUX8_ARB                      = $208F;
  WGL_AUX9_ARB                      = $2090;

var
  wglBindTexImageARB: function(hPbuffer: HPBUFFERARB; iBuffer: Integer): BOOL; extdecl;
  wglReleaseTexImageARB: function(hPbuffer: HPBUFFERARB; iBuffer: Integer): BOOL; extdecl;
  wglSetPbufferAttribARB: function(hPbuffer: HPBUFFERARB; const piAttribList: PInteger): BOOL; extdecl;

//*** WGL_NV_render_depth_texture
const
  WGL_BIND_TO_TEXTURE_DEPTH_NV      = $20A3;
  WGL_BIND_TO_TEXTURE_RECTANGLE_DEPTH_NV = $20A4;
  WGL_DEPTH_TEXTURE_FORMAT_NV       = $20A5;
  WGL_TEXTURE_DEPTH_COMPONENT_NV    = $20A6;
//  WGL_NO_TEXTURE_ARB                = $2077;
  WGL_DEPTH_COMPONENT_NV            = $20A7;

//*** WGL_NV_render_texture_rectangle
const
  WGL_BIND_TO_TEXTURE_RECTANGLE_RGB_NV = $20A0;
  WGL_BIND_TO_TEXTURE_RECTANGLE_RGBA_NV = $20A1;
  WGL_TEXTURE_RECTANGLE_NV          = $20A2;

{$ENDIF Windows}

{******************************************************************************}

implementation

{******************************************************************************}

{$IFNDEF Windows}
function wglGetProcAddress(proc: PChar): Pointer;
begin
  Result := GetProcAddress(LibGL, proc);
end;
{$ENDIF}

{ Check if an OpenGL extension is supported by the ICD. Translated from a C
  routine by Mark Kilgard of NVIDIA. }

function glext_ExtensionSupported(const extension: String): Boolean;
begin
  Result := glext_ExtensionSupported(extension, '');
end;

function glext_ExtensionSupported(const extension: String; const searchIn: String): Boolean;
var
  extensions: PChar;
  start: PChar;
  where, terminator: PChar;
begin

  // Extension names should not have spaces.
  if (Pos(' ', extension) <> 0) or (extension = '') then
  begin
    Result := FALSE;
    Exit;
  end;

  if searchIn = '' then extensions := PChar(glGetString(GL_EXTENSIONS))
  else extensions := PChar(searchIn);
  { It takes a bit of care to be fool-proof about parsing the OpenGL extensions
    string. Don't be fooled by sub-strings, etc. }
  start := extensions;
  while TRUE do
  begin
    where := StrPos(start, PChar(extension));
    if where = nil then Break;
    terminator := Pointer(Integer(where) + Length(extension));
    if (where = start) or (PChar(Integer(where) - 1)^ = ' ') then
    begin
      if (terminator^ = ' ') or (terminator^ = #0) then
      begin
	Result := TRUE;
	Exit;
      end;
    end;
    start := terminator;
  end;
  Result := FALSE;

end;

end.


{
  $Log:
}

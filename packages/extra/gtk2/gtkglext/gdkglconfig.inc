{ Original file is /usr/include/gtkglext-1.0/gdk/gdkglconfig.h }

{$ifdef read_interface_types}

  { PGdkGLConfig already defined in gdkgltypes.inc }

  TGdkGLConfig = record
    parent_instance: TGObject;
    layer_plane: gint;
    n_aux_buffers: gint;
    n_sample_buffers: gint;
    flag0 : word;
  end;

  PGdkGLConfigClass = ^TGdkGLConfigClass;
  TGdkGLConfigClass = record
    parent_class: TGObjectClass;
  end;

{$endif read_interface_types}

{$ifdef read_interface_rest}
const
  { Constants for operating on TGdkGLConfig.flag0 }
  bm_TGdkGLConfig_is_rgba            = 1 shl 0;
  bp_TGdkGLConfig_is_rgba            =       0;
  bm_TGdkGLConfig_is_double_buffered = 1 shl 1;
  bp_TGdkGLConfig_is_double_buffered =       1;
  bm_TGdkGLConfig_as_single_mode     = 1 shl 2;
  bp_TGdkGLConfig_as_single_mode     =       2;
  bm_TGdkGLConfig_is_stereo          = 1 shl 3;
  bp_TGdkGLConfig_is_stereo          =       3;
  bm_TGdkGLConfig_has_alpha          = 1 shl 4;
  bp_TGdkGLConfig_has_alpha          =       4;
  bm_TGdkGLConfig_has_depth_buffer   = 1 shl 5;
  bp_TGdkGLConfig_has_depth_buffer   =       5;
  bm_TGdkGLConfig_has_stencil_buffer = 1 shl 6;
  bp_TGdkGLConfig_has_stencil_buffer =       6;
  bm_TGdkGLConfig_has_accum_buffer   = 1 shl 7;
  bp_TGdkGLConfig_has_accum_buffer   =       7;

{*
 * Display mode bit masks.
 *}
const
  GDK_GL_MODE_RGB         = 0;
  GDK_GL_MODE_RGBA        = 0;       {* same as RGB *}
  GDK_GL_MODE_INDEX       = 1 shl 0;
  GDK_GL_MODE_SINGLE      = 0;
  GDK_GL_MODE_DOUBLE      = 1 shl 1;
  GDK_GL_MODE_STEREO      = 1 shl 2;
  GDK_GL_MODE_ALPHA       = 1 shl 3;
  GDK_GL_MODE_DEPTH       = 1 shl 4;
  GDK_GL_MODE_STENCIL     = 1 shl 5;
  GDK_GL_MODE_ACCUM       = 1 shl 6;
  GDK_GL_MODE_MULTISAMPLE = 1 shl 7;   {* not supported yet *}

type
  TGdkGLConfigMode = Longint;
  PGdkGLConfigMode = ^TGdkGLConfigMode;

function GDK_TYPE_GL_CONFIG: GType;
function GDK_GL_CONFIG(anObject: Pointer): PGdkGLConfig;
function GDK_GL_CONFIG_CLASS(klass: Pointer): PGdkGLConfigClass;
function GDK_IS_GL_CONFIG(anObject: Pointer): boolean;
function GDK_IS_GL_CONFIG_CLASS(klass: Pointer): boolean;
function GDK_GL_CONFIG_GET_CLASS(obj: Pointer): PGdkGLConfigClass;

function gdk_gl_config_get_type: GType; cdecl; external GdkGLExtLib;

{$ifndef GDK_MULTIHEAD_SAFE}
function gdk_gl_config_new(
  attrib_list: PInteger): PGdkGLConfig;
  cdecl; external GdkGLExtLib;
{$endif GDK_MULTIHEAD_SAFE}

{$ifdef GDKGLEXT_MULTIHEAD_SUPPORT}
function gdk_gl_config_new_for_screen(
  screen: PGdkScreen;
  attrib_list: PInteger): PGdkGLConfig;
  cdecl; external GdkGLExtLib;
{$endif GDKGLEXT_MULTIHEAD_SUPPORT}

{$ifndef GDK_MULTIHEAD_SAFE}
function gdk_gl_config_new_by_mode(
  mode: TGdkGLConfigMode): PGdkGLConfig;
  cdecl; external GdkGLExtLib;
{$endif GDK_MULTIHEAD_SAFE}

{$ifdef GDKGLEXT_MULTIHEAD_SUPPORT}
function gdk_gl_config_new_by_mode_for_screen(
  screen: PGdkScreen;
  mode: TGdkGLConfigMode): PGdkGLConfig;
  cdecl; external GdkGLExtLib;
{$endif GDKGLEXT_MULTIHEAD_SUPPORT}

function gdk_gl_config_get_screen(glconfig: PGdkGLConfig): PGdkScreen;
  cdecl; external GdkGLExtLib;

function gdk_gl_config_get_attrib(glconfig: PGdkGLConfig;
  attribute: Integer;
  value: PInteger): gboolean;
  cdecl; external GdkGLExtLib;

function gdk_gl_config_get_colormap(glconfig: PGdkGLConfig): PGdkColormap;
  cdecl; external GdkGLExtLib;

function gdk_gl_config_get_visual(glconfig: PGdkGLConfig): PGdkVisual;
  cdecl; external GdkGLExtLib;

function gdk_gl_config_get_depth              (glconfig: PGdkGLConfig): gint;
  cdecl; external GdkGLExtLib;

function gdk_gl_config_get_layer_plane        (glconfig: PGdkGLConfig): gint;
  cdecl; external GdkGLExtLib;

function gdk_gl_config_get_n_aux_buffers      (glconfig: PGdkGLConfig): gint;
  cdecl; external GdkGLExtLib;

function gdk_gl_config_get_n_sample_buffers   (glconfig: PGdkGLConfig): gint;
  cdecl; external GdkGLExtLib;

function gdk_gl_config_is_rgba                (glconfig: PGdkGLConfig): gboolean;
  cdecl; external GdkGLExtLib;

function gdk_gl_config_is_double_buffered     (glconfig: PGdkGLConfig): gboolean;
  cdecl; external GdkGLExtLib;

function gdk_gl_config_is_stereo              (glconfig: PGdkGLConfig): gboolean;
  cdecl; external GdkGLExtLib;

function gdk_gl_config_has_alpha              (glconfig: PGdkGLConfig): gboolean;
  cdecl; external GdkGLExtLib;

function gdk_gl_config_has_depth_buffer       (glconfig: PGdkGLConfig): gboolean;
  cdecl; external GdkGLExtLib;

function gdk_gl_config_has_stencil_buffer     (glconfig: PGdkGLConfig): gboolean;
  cdecl; external GdkGLExtLib;

function gdk_gl_config_has_accum_buffer       (glconfig: PGdkGLConfig): gboolean;
  cdecl; external GdkGLExtLib;

{$endif read_interface_rest}

{$ifdef read_implementation}

function GDK_TYPE_GL_CONFIG: GType;
begin
 GDK_TYPE_GL_CONFIG := gdk_gl_config_get_type;
end;

function GDK_GL_CONFIG(anObject: Pointer): PGdkGLConfig;
begin
 GDK_GL_CONFIG := PGdkGLConfig(
   G_TYPE_CHECK_INSTANCE_CAST(anObject, GDK_TYPE_GL_CONFIG));
end;

function GDK_GL_CONFIG_CLASS(klass: Pointer): PGdkGLConfigClass;
begin
 GDK_GL_CONFIG_CLASS := PGdkGLConfigClass(
   G_TYPE_CHECK_CLASS_CAST(klass, GDK_TYPE_GL_CONFIG));
end;

function GDK_IS_GL_CONFIG(anObject: Pointer): boolean;
begin
 GDK_IS_GL_CONFIG := G_TYPE_CHECK_INSTANCE_TYPE(anObject, GDK_TYPE_GL_CONFIG);
end;

function GDK_IS_GL_CONFIG_CLASS(klass: Pointer): boolean;
begin
 GDK_IS_GL_CONFIG_CLASS := G_TYPE_CHECK_CLASS_TYPE(klass, GDK_TYPE_GL_CONFIG);
end;

function GDK_GL_CONFIG_GET_CLASS(obj: Pointer): PGdkGLConfigClass;
begin
 GDK_GL_CONFIG_GET_CLASS := PGdkGLConfigClass(
   G_TYPE_INSTANCE_GET_CLASS(obj, GDK_TYPE_GL_CONFIG));
end;

{$endif read_implementation}

{ Original file is /usr/include/gtkglext-1.0/gdk/gdkglenumtypes.h }

{ Translation to Pascal:
  used regexps
    GType \(.*\) (void);
  to
    function \1: GType; cdecl; external GdkGLExtLib;

    #define \(.*\) (\(.*\)())
  to
    function \1: GType; 
      cdecl; external GdkGLExtLib name '\2';
}

{$ifdef read_interface_rest}

{* enumerations from "gdkgltokens.h" *}

function gdk_gl_config_attrib_get_type: GType; cdecl; external GdkGLExtLib;
function GDK_TYPE_GL_CONFIG_ATTRIB: GType; 
  cdecl; external GdkGLExtLib name 'gdk_gl_config_attrib_get_type';

function gdk_gl_config_caveat_get_type: GType; cdecl; external GdkGLExtLib;
function GDK_TYPE_GL_CONFIG_CAVEAT: GType; 
  cdecl; external GdkGLExtLib name 'gdk_gl_config_caveat_get_type';

function gdk_gl_visual_type_get_type: GType; cdecl; external GdkGLExtLib;
function GDK_TYPE_GL_VISUAL_TYPE: GType; 
  cdecl; external GdkGLExtLib name 'gdk_gl_visual_type_get_type';

function gdk_gl_transparent_type_get_type: GType; cdecl; external GdkGLExtLib;
function GDK_TYPE_GL_TRANSPARENT_TYPE: GType; 
  cdecl; external GdkGLExtLib name 'gdk_gl_transparent_type_get_type';

function gdk_gl_drawable_type_mask_get_type: GType; cdecl; external GdkGLExtLib;
function GDK_TYPE_GL_DRAWABLE_TYPE_MASK: GType; 
  cdecl; external GdkGLExtLib name 'gdk_gl_drawable_type_mask_get_type';

function gdk_gl_render_type_mask_get_type: GType; cdecl; external GdkGLExtLib;
function GDK_TYPE_GL_RENDER_TYPE_MASK: GType; 
  cdecl; external GdkGLExtLib name 'gdk_gl_render_type_mask_get_type';

function gdk_gl_buffer_mask_get_type: GType; cdecl; external GdkGLExtLib;
function GDK_TYPE_GL_BUFFER_MASK: GType; 
  cdecl; external GdkGLExtLib name 'gdk_gl_buffer_mask_get_type';

function gdk_gl_config_error_get_type: GType; cdecl; external GdkGLExtLib;
function GDK_TYPE_GL_CONFIG_ERROR: GType; 
  cdecl; external GdkGLExtLib name 'gdk_gl_config_error_get_type';

function gdk_gl_render_type_get_type: GType; cdecl; external GdkGLExtLib;
function GDK_TYPE_GL_RENDER_TYPE: GType; 
  cdecl; external GdkGLExtLib name 'gdk_gl_render_type_get_type';

function gdk_gl_drawable_attrib_get_type: GType; cdecl; external GdkGLExtLib;
function GDK_TYPE_GL_DRAWABLE_ATTRIB: GType; 
  cdecl; external GdkGLExtLib name 'gdk_gl_drawable_attrib_get_type';

function gdk_gl_pbuffer_attrib_get_type: GType; cdecl; external GdkGLExtLib;
function GDK_TYPE_GL_PBUFFER_ATTRIB: GType; 
  cdecl; external GdkGLExtLib name 'gdk_gl_pbuffer_attrib_get_type';

function gdk_gl_event_mask_get_type: GType; cdecl; external GdkGLExtLib;
function GDK_TYPE_GL_EVENT_MASK: GType; 
  cdecl; external GdkGLExtLib name 'gdk_gl_event_mask_get_type';

function gdk_gl_event_type_get_type: GType; cdecl; external GdkGLExtLib;
function GDK_TYPE_GL_EVENT_TYPE: GType; 
  cdecl; external GdkGLExtLib name 'gdk_gl_event_type_get_type';

function gdk_gl_drawable_type_get_type: GType; cdecl; external GdkGLExtLib;
function GDK_TYPE_GL_DRAWABLE_TYPE: GType; 
  cdecl; external GdkGLExtLib name 'gdk_gl_drawable_type_get_type';

{* enumerations from "gdkglconfig.h" *}

function gdk_gl_config_mode_get_type: GType; cdecl; external GdkGLExtLib;
function GDK_TYPE_GL_CONFIG_MODE: GType; 
  cdecl; external GdkGLExtLib name 'gdk_gl_config_mode_get_type';

{$endif read_interface_rest}

{$ifdef read_implementation}

{$endif read_implementation}

{ Original file is /usr/include/gtkglext-1.0/gdk/gdkglquery.h }

{$ifdef read_interface_rest}

{$ifndef GDK_MULTIHEAD_SAFE}
function gdk_gl_query_extension: gboolean;
  cdecl; external GdkGLExtLib;
{$endif GDK_MULTIHEAD_SAFE}

{$ifdef GDKGLEXT_MULTIHEAD_SUPPORT}
function gdk_gl_query_extension_for_display(display: PGdkDisplay): gboolean;
  cdecl; external GdkGLExtLib;
{$endif GDKGLEXT_MULTIHEAD_SUPPORT}

{$ifndef GDK_MULTIHEAD_SAFE}
function  gdk_gl_query_version(
  var major, minor: LongInt): gboolean;
  cdecl; external GdkGLExtLib;
{$endif GDK_MULTIHEAD_SAFE}

{$ifdef GDKGLEXT_MULTIHEAD_SUPPORT}
function gdk_gl_query_version_for_display(
  display: PGdkDisplay;
  var major, minor: LongInt): gboolean;
  cdecl; external GdkGLExtLib;
{$endif GDKGLEXT_MULTIHEAD_SUPPORT}

function gdk_gl_query_gl_extension(extension: PChar): gboolean;
  cdecl; external GdkGLExtLib;

function gdk_gl_get_proc_address(proc_name: PChar): TGdkGLProc;
  cdecl; external GdkGLExtLib;

{$endif read_interface_rest}

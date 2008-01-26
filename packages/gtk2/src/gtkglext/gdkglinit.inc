{ Original file is /usr/include/gtkglext-1.0/gdk/gdkglinit.h }

{$ifdef read_interface_rest}

{*
 * Initialization routines.
 *}

function gdk_gl_parse_args(argc: Plongint; argv: PPPChar): gboolean;
  cdecl; external GdkGLExtLib;
function gdk_gl_init_check(argc: Plongint; argv: PPPChar): gboolean;
  cdecl; external GdkGLExtLib;
procedure gdk_gl_init(argc: Plongint; argv: PPPChar);
  cdecl; external GdkGLExtLib;

{$endif read_interface_rest}

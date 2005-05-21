{ Original file is /usr/include/gtkglext-1.0/gdk/gdkgltypes.h }

{$ifdef read_interface_types}

{*
 * Forward declarations of commonly used types
 *}

  { Comments for Pascal translation: TGdkGLProc is a procedure address
    returned by gdk_gl_get_proc_address.

    Actually one cannot say is it procedure or a function,
    what parameters it takes and even it's calling convention
    (e.g. under Win32 procedures from OpenGL32.dll
    use stdcall calling convention, not cdecl).

    That's why I didn't translate it as "procedure; cdecl;"
    (this would be a strict translation of what is expressed in C header)
    but I translated it as a generic Pointer. }
  TGdkGLProc = Pointer;

  PGdkGLConfig   = ^TGdkGLConfig;
  PGdkGLContext  = ^TGdkGLContext;
  PGdkGLDrawable = ^TGdkGLDrawable;
  PGdkGLPixmap   = ^TGdkGLPixmap;
  PGdkGLWindow   = ^TGdkGLWindow;

{$endif read_interface_types}

{$ifdef read_implementation}
{$endif read_implementation}

{
   $Id$
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     PGtkPlug = ^TGtkPlug;
     TGtkPlug = record
          window : TGtkWindow;
          socket_window : PGdkWindow;
          same_app : gint;
       end;

     PGtkPlugClass = ^TGtkPlugClass;
     TGtkPlugClass = record
          parent_class : TGtkWindowClass;
       end;

type
  GTK_PLUG=PGtkPlug;
  GTK_PLUG_CLASS=PGtkPlugClass;

{$ifndef gtkwin}
function  gtk_plug_get_type:guint;cdecl;external gtkdll name 'gtk_plug_get_type';
procedure gtk_plug_construct(plug:PGtkPlug; socket_id:guint32);cdecl;external gtkdll name 'gtk_plug_construct';
function  gtk_plug_new(socket_id:guint32):PGtkWidget;cdecl;external gtkdll name 'gtk_plug_new';
{$endif}

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}
{$endif read_implementation}


{
  $Log$
  Revision 1.1  2000-07-13 06:34:05  michael
  + Initial import

  Revision 1.1  1999/11/24 23:36:36  peter
    * moved to packages dir

  Revision 1.5  1999/10/06 17:42:50  peter
    * external is now only in the interface
    * removed gtk 1.0 support

  Revision 1.4  1999/07/23 16:12:53  peter
    * use packrecords C

  Revision 1.3  1999/05/11 00:39:10  peter
    * win32 fixes

  Revision 1.2  1999/05/10 15:20:05  peter
    * cdecl fixes

  Revision 1.1  1999/05/10 09:14:00  peter
    + new gtk 1.2 files

}


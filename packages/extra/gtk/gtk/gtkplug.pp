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
  Revision 1.1  2002-01-29 17:55:12  peter
    * splitted to base and extra

  Revision 1.2  2000/07/13 11:33:23  michael
  + removed logs
 
}

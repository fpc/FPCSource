{
   $Id$
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

    type
       PGtkAspectFrame = ^TGtkAspectFrame;
       TGtkAspectFrame = record
            frame : TGtkFrame;
            xalign : gfloat;
            yalign : gfloat;
            ratio : gfloat;
            obey_child : gint;
            center_allocation : TGtkAllocation;
         end;

       PGtkAspectFrameClass = ^TGtkAspectFrameClass;
       TGtkAspectFrameClass = record
            parent_class : TGtkBinClass;
         end;

Type
  GTK_ASPECT_FRAME=PGtkAspectFrame;
  GTK_ASPECT_FRAME_CLASS=PGtkAspectFrameClass;

function  GTK_ASPECT_FRAME_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_aspect_frame_get_type';
function  GTK_IS_ASPECT_FRAME(obj:pointer):boolean;
function  GTK_IS_ASPECT_FRAME_CLASS(klass:pointer):boolean;

function  gtk_aspect_frame_get_type:TGtkType;cdecl;external gtkdll name 'gtk_aspect_frame_get_type';
function  gtk_aspect_frame_new (thelabel:Pgchar; xalign:gfloat; yalign:gfloat; ratio:gfloat; obey_child:gint):PGtkWidget;cdecl;external gtkdll name 'gtk_aspect_frame_new';
procedure gtk_aspect_frame_set(aspect_frame:PGtkAspectFrame; xalign:gfloat; yalign:gfloat; ratio:gfloat; obey_child:gint);cdecl;external gtkdll name 'gtk_aspect_frame_set';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_ASPECT_FRAME(obj:pointer):boolean;
begin
  GTK_IS_ASPECT_FRAME:=(obj<>nil) and GTK_IS_ASPECT_FRAME_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_ASPECT_FRAME_CLASS(klass:pointer):boolean;
begin
  GTK_IS_ASPECT_FRAME_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_ASPECT_FRAME_TYPE);
end;

{$endif read_implementation}


{
  $Log$
  Revision 1.1  2000-07-13 06:34:03  michael
  + Initial import

  Revision 1.1  1999/11/24 23:36:35  peter
    * moved to packages dir

  Revision 1.11  1999/10/21 08:42:01  florian
    * some changes to get it work with gtk 1.3 under Windows 98:
      - removed some trailing space after the import name
      - In gtkbindings.h is
        #define  gtk_binding_entry_add          gtk_binding_entry_clear
        so in the pascal headers the import name of gtk_bindings_entry_add should be
        gtk_binding_entry_clear!
      - removed the declaration of
        gtk_drag_source_unset in gtkdnd.pp it isn't in gtk-1.3.dll!
      - in gdk.pp glibdll must be set to gdk-1.3:
        const
           gdkdll='gdk-1.3';
           glibdll='gdk-1.3';
        else the whole gdk_* calls are imported from glib-1.3.dll which is wrong!

  Revision 1.10  1999/10/06 17:42:48  peter
    * external is now only in the interface
    * removed gtk 1.0 support

  Revision 1.9  1999/07/23 16:11:55  peter
    * use packrecords C

  Revision 1.8  1999/05/11 00:38:08  peter
    * win32 fixes

  Revision 1.7  1999/05/10 15:18:55  peter
    * cdecl fixes

  Revision 1.6  1999/05/07 17:40:11  peter
    * more updates

  Revision 1.5  1999/05/07 10:40:27  peter
    * first things for 1.2

  Revision 1.4  1998/11/09 10:09:33  peter
    + C type casts are now correctly handled

  Revision 1.3  1998/10/21 20:22:08  peter
    * cdecl, packrecord fixes (from the gtk.tar.gz)
    * win32 support
    * gtk.pp,gdk.pp for an all in one unit

}


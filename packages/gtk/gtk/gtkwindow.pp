{
   $Id$
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

    type

       PGtkWindow = ^TGtkWindow;
       TGtkWindow = record
            bin : TGtkBin;
            title : Pgchar;
            wmclass_name : Pgchar;
            wmclass_class : Pgchar;
            thetype : TGtkWindowType;
            focus_widget : PGtkWidget;
            default_widget : PGtkWidget;
            transient_parent : PGtkWindow;
            resize_count : gushort;
            flag0 : {$ifdef win32}longint{$else}word{$endif};
         end;

    const
       bm_modal = 1;
       bp_modal = 0;
       bm_allow_shrink = 2;
       bp_allow_shrink = 1;
       bm_allow_grow = 4;
       bp_allow_grow = 2;
       bm_auto_shrink = 8;
       bp_auto_shrink = 3;
       bm_handling_resize = 16;
       bp_handling_resize = 4;
       bm_position = 96;
       bp_position = 5;
       bm_use_uposition = 128;
       bp_use_uposition = 7;

function  modal(var a : TGtkWindow) : guint;
procedure set_modal(var a : TGtkWindow; __modal : guint);
function  allow_shrink(var a : TGtkWindow) : guint;
procedure set_allow_shrink(var a : TGtkWindow; __allow_shrink : guint);
function  allow_grow(var a : TGtkWindow) : guint;
procedure set_allow_grow(var a : TGtkWindow; __allow_grow : guint);
function  auto_shrink(var a : TGtkWindow) : guint;
procedure set_auto_shrink(var a : TGtkWindow; __auto_shrink : guint);
function  handling_resize(var a : TGtkWindow) : guint;cdecl;
procedure set_handling_resize(var a : TGtkWindow; __handling_resize : guint);cdecl;
function  position(var a : TGtkWindow) : guint;
procedure set_position(var a : TGtkWindow; __position : guint);
function  use_uposition(var a : TGtkWindow) : guint;
procedure set_use_uposition(var a : TGtkWindow; __use_uposition : guint);

    type
       PGtkWindowClass = ^TGtkWindowClass;
       TGtkWindowClass = record
            parent_class : TGtkBinClass;
            set_focus : procedure (window:PGtkWindow; focus:PGtkWidget);cdecl;
         end;

Type
  GTK_WINDOW = PGtkWindow;
  GTK_WINDOW_CLASS = PGtkWindowClass;

function  GTK_WINDOW_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_window_get_type';
function  GTK_IS_WINDOW(obj:pointer):boolean;
function  GTK_IS_WINDOW_CLASS(klass:pointer):boolean;

function  gtk_window_get_type:TGtkType;cdecl;external gtkdll name 'gtk_window_get_type';
function  gtk_window_new (thetype:TGtkWindowType):PGtkWidget;cdecl;external gtkdll name 'gtk_window_new';
procedure gtk_window_set_title(window:PGtkWindow; title:Pgchar);cdecl;external gtkdll name 'gtk_window_set_title';
procedure gtk_window_set_wmclass(window:PGtkWindow; wmclass_name:Pgchar; wmclass_class:Pgchar);cdecl;external gtkdll name 'gtk_window_set_wmclass';
procedure gtk_window_set_policy(window:PGtkWindow; allow_shrink:gint; allow_grow:gint; auto_shrink:gint);cdecl;external gtkdll name 'gtk_window_set_policy';
procedure gtk_window_add_accel_group(window:PGtkWindow; accel_group:PGtkAccelGroup);cdecl;external gtkdll name 'gtk_window_add_accel_group';
procedure gtk_window_set_position(window:PGtkWindow; position:TGtkWindowPosition);cdecl;external gtkdll name 'gtk_window_set_position';
function  gtk_window_activate_focus(window:PGtkWindow):gint;cdecl;external gtkdll name 'gtk_window_activate_focus';
function  gtk_window_activate_default(window:PGtkWindow):gint;cdecl;external gtkdll name 'gtk_window_activate_default';
procedure gtk_window_set_transient_for(window:PGtkWindow; parent:PGtkWindow);cdecl;external gtkdll name 'gtk_window_set_transient_for';
procedure gtk_window_set_geometry_hints(window:PGtkWindow; geometry_widget:PGtkWidget; geometry:PGdkGeometry; geom_mask:TGdkWindowHints);cdecl;external gtkdll name 'gtk_window_set_geometry_hints';
procedure gtk_window_set_default_size(window:PGtkWindow; width:gint; height:gint);cdecl;external gtkdll name 'gtk_window_set_default_size';
procedure gtk_window_set_modal(window:PGtkWindow; modal:gboolean);cdecl;external gtkdll name 'gtk_window_set_modal';
procedure gtk_window_set_focus(window:PGtkWindow; focus:PGtkWidget);cdecl;external gtkdll name 'gtk_window_set_focus';
procedure gtk_window_set_default(window:PGtkWindow; defaultw:PGtkWidget);cdecl;external gtkdll name 'gtk_window_set_default';
{$ifndef gtkwin}
procedure gtk_window_remove_embedded_xid(window:PGtkWindow; xid:guint);cdecl;external gtkdll name 'gtk_window_remove_embedded_xid';
procedure gtk_window_add_embedded_xid(window:PGtkWindow; xid:guint);cdecl;external gtkdll name 'gtk_window_add_embedded_xid';
{$endif}

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  modal(var a : TGtkWindow) : guint;
  begin
     modal:=(a.flag0 and bm_modal) shr bp_modal;
  end;

procedure set_modal(var a : TGtkWindow; __modal : guint);
  begin
     a.flag0:=a.flag0 or ((__modal shl bp_modal) and bm_modal);
  end;

function  allow_shrink(var a : TGtkWindow) : guint;
  begin
     allow_shrink:=(a.flag0 and bm_allow_shrink) shr bp_allow_shrink;
  end;

procedure set_allow_shrink(var a : TGtkWindow; __allow_shrink : guint);
  begin
     a.flag0:=a.flag0 or ((__allow_shrink shl bp_allow_shrink) and bm_allow_shrink);
  end;

function  allow_grow(var a : TGtkWindow) : guint;
  begin
     allow_grow:=(a.flag0 and bm_allow_grow) shr bp_allow_grow;
  end;

procedure set_allow_grow(var a : TGtkWindow; __allow_grow : guint);
  begin
     a.flag0:=a.flag0 or ((__allow_grow shl bp_allow_grow) and bm_allow_grow);
  end;

function  auto_shrink(var a : TGtkWindow) : guint;
  begin
     auto_shrink:=(a.flag0 and bm_auto_shrink) shr bp_auto_shrink;
  end;

procedure set_auto_shrink(var a : TGtkWindow; __auto_shrink : guint);
  begin
     a.flag0:=a.flag0 or ((__auto_shrink shl bp_auto_shrink) and bm_auto_shrink);
  end;

function  handling_resize(var a : TGtkWindow) : guint;cdecl;
  begin
     handling_resize:=(a.flag0 and bm_handling_resize) shr bp_handling_resize;
  end;

procedure set_handling_resize(var a : TGtkWindow; __handling_resize : guint);cdecl;
  begin
     a.flag0:=a.flag0 or ((__handling_resize shl bp_handling_resize) and bm_handling_resize);
  end;

function  position(var a : TGtkWindow) : guint;
  begin
     position:=(a.flag0 and bm_position) shr bp_position;
  end;

procedure set_position(var a : TGtkWindow; __position : guint);
  begin
     a.flag0:=a.flag0 or ((__position shl bp_position) and bm_position);
  end;

function  use_uposition(var a : TGtkWindow) : guint;
  begin
     use_uposition:=(a.flag0 and bm_use_uposition) shr bp_use_uposition;
  end;

procedure set_use_uposition(var a : TGtkWindow; __use_uposition : guint);
  begin
     a.flag0:=a.flag0 or ((__use_uposition shl bp_use_uposition) and bm_use_uposition);
  end;

function  GTK_IS_WINDOW(obj:pointer):boolean;
begin
  GTK_IS_WINDOW:=(obj<>nil) and GTK_IS_WINDOW_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_WINDOW_CLASS(klass:pointer):boolean;
begin
  GTK_IS_WINDOW_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_WINDOW_TYPE);
end;

{$endif read_implementation}

{
  $Log$
  Revision 1.1.2.3  2000-09-09 18:42:53  peter
    * gtk win32 fixes

  Revision 1.1.2.2  2000/09/06 21:13:35  peter
    * packrecords 4 for win32, packrecords c for linux

  Revision 1.1.2.1  2000/09/06 20:46:25  peter
    * fixed flag0 size for win32

  Revision 1.1  2000/07/13 06:34:08  michael
  + Initial import

  Revision 1.1  1999/11/24 23:36:37  peter
    * moved to packages dir

  Revision 1.12  1999/10/21 08:42:02  florian
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

  Revision 1.11  1999/10/06 17:42:51  peter
    * external is now only in the interface
    * removed gtk 1.0 support

  Revision 1.10  1999/07/23 16:13:30  peter
    * use packrecords C

  Revision 1.9  1999/05/11 00:39:51  peter
    * win32 fixes

  Revision 1.8  1999/05/10 15:20:51  peter
    * cdecl fixes

  Revision 1.7  1999/05/10 09:04:16  peter
    * gtk 1.2 port working

  Revision 1.6  1999/05/07 15:10:28  peter
    * more fixes

  Revision 1.5  1998/11/09 10:10:52  peter
    + C type casts are now correctly handled

  Revision 1.4  1998/10/21 22:25:24  peter
    * fixed some wrong cdecls

  Revision 1.3  1998/10/21 20:23:34  peter
    * cdecl, packrecord fixes (from the gtk.tar.gz)
    * win32 support
    * gtk.pp,gdk.pp for an all in one unit

}

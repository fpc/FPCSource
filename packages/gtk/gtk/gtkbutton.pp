{
   $Id$
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

    type
       PGtkButton = ^TGtkButton;
       TGtkButton = record
            bin : TGtkBin;
            child : PGtkWidget;
            flag0 : word;
         end;

    const
       bm_in_button = 1;
       bp_in_button = 0;
       bm_button_down = 2;
       bp_button_down = 1;
       bm_relief = 4;
       bp_relief = 2;
function  in_button(var a : TGtkButton) : guint;
procedure set_in_button(var a : TGtkButton; __in_button : guint);
function  button_down(var a : TGtkButton) : guint;
procedure set_button_down(var a : TGtkButton; __button_down : guint);
function  relief(var a : TGtkButton) : guint;
procedure set_relief(var a : TGtkButton; __button_down : guint);

    type
       PGtkButtonClass = ^TGtkButtonClass;
       TGtkButtonClass = record
            parent_class : TGtkBinClass;
            pressed : procedure (button:PGtkButton); cdecl;
            released : procedure (button:PGtkButton); cdecl;
            clicked : procedure (button:PGtkButton); cdecl;
            enter : procedure (button:PGtkButton); cdecl;
            leave : procedure (button:PGtkButton); cdecl;
         end;

Type
  GTK_BUTTON=PGtkButton;
  GTK_BUTTON_CLASS=PGtkButtonClass;

function  GTK_BUTTON_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_button_get_type';
function  GTK_IS_BUTTON(obj:pointer):boolean;
function  GTK_IS_BUTTON_CLASS(klass:pointer):boolean;

function  gtk_button_get_type:TGtkType;cdecl;external gtkdll name 'gtk_button_get_type';
function  gtk_button_new : PGtkWidget;cdecl;external gtkdll name 'gtk_button_new';
function  gtk_button_new_with_label (thelabel:Pgchar):PGtkWidget;cdecl;external gtkdll name 'gtk_button_new_with_label';
procedure gtk_button_pressed(button:PGtkButton);cdecl;external gtkdll name 'gtk_button_pressed';
procedure gtk_button_released(button:PGtkButton);cdecl;external gtkdll name 'gtk_button_released';
procedure gtk_button_clicked(button:PGtkButton);cdecl;external gtkdll name 'gtk_button_clicked';
procedure gtk_button_enter(button:PGtkButton);cdecl;external gtkdll name 'gtk_button_enter';
procedure gtk_button_leave(button:PGtkButton);cdecl;external gtkdll name 'gtk_button_leave';
procedure gtk_button_set_relief(button:PGtkButton; newstyle:TGtkReliefStyle);cdecl;external gtkdll name 'gtk_button_set_relief';
function  gtk_button_get_relief(button:PGtkButton):TGtkReliefStyle;cdecl;external gtkdll name 'gtk_button_get_relief';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  in_button(var a : TGtkButton) : guint;
      begin
         in_button:=(a.flag0 and bm_in_button) shr bp_in_button;
      end;

procedure set_in_button(var a : TGtkButton; __in_button : guint);
      begin
         a.flag0:=a.flag0 or ((__in_button shl bp_in_button) and bm_in_button);
      end;

function  button_down(var a : TGtkButton) : guint;
      begin
         button_down:=(a.flag0 and bm_button_down) shr bp_button_down;
      end;

procedure set_button_down(var a : TGtkButton; __button_down : guint);
      begin
         a.flag0:=a.flag0 or ((__button_down shl bp_button_down) and bm_button_down);
      end;

function  relief(var a : TGtkButton) : guint;
      begin
         relief:=(a.flag0 and bm_relief) shr bp_relief;
      end;

procedure set_relief(var a : TGtkButton; __button_down : guint);
      begin
         a.flag0:=a.flag0 or ((__button_down shl bp_relief) and bm_relief);
      end;

function  GTK_IS_BUTTON(obj:pointer):boolean;
begin
  GTK_IS_BUTTON:=(obj<>nil) and GTK_IS_BUTTON_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_BUTTON_CLASS(klass:pointer):boolean;
begin
  GTK_IS_BUTTON_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_BUTTON_TYPE);
end;

{$endif read_implementation}


{
  $Log$
  Revision 1.1  2000-07-13 06:34:03  michael
  + Initial import

  Revision 1.1  1999/11/24 23:36:35  peter
    * moved to packages dir

  Revision 1.13  1999/10/21 08:42:01  florian
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

  Revision 1.12  1999/10/06 17:42:48  peter
    * external is now only in the interface
    * removed gtk 1.0 support

  Revision 1.11  1999/07/23 16:12:01  peter
    * use packrecords C

  Revision 1.10  1999/05/11 00:38:13  peter
    * win32 fixes

  Revision 1.9  1999/05/10 15:19:00  peter
    * cdecl fixes

  Revision 1.8  1999/05/10 09:02:59  peter
    * gtk 1.2 port working

  Revision 1.7  1999/05/07 17:40:13  peter
    * more updates

  Revision 1.6  1999/05/07 10:40:31  peter
    * first things for 1.2

  Revision 1.5  1998/11/09 10:09:37  peter
    + C type casts are now correctly handled

  Revision 1.4  1998/10/21 22:25:17  peter
    * fixed some wrong cdecls

  Revision 1.3  1998/10/21 20:22:12  peter
    * cdecl, packrecord fixes (from the gtk.tar.gz)
    * win32 support
    * gtk.pp,gdk.pp for an all in one unit

}


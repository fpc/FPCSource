{
   $Id$
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

    type
       PGtkToggleButton = ^TGtkToggleButton;
       TGtkToggleButton = record
          button : TGtkButton;
          flag0 : longint;
          event_window : PGdkWindow;
        end;

  const
     bm_TGtkToggleButton_active = $1;
     bp_TGtkToggleButton_active = 0;
     bm_TGtkToggleButton_draw_indicator = $2;
     bp_TGtkToggleButton_draw_indicator = 1;
function  active(var a : TGtkToggleButton) : guint;
procedure set_active(var a : TGtkToggleButton; __active : guint);
function  draw_indicator(var a : TGtkToggleButton) : guint;
procedure set_draw_indicator(var a : TGtkToggleButton; __draw_indicator : guint);

    type
       PGtkToggleButtonClass = ^TGtkToggleButtonClass;
       TGtkToggleButtonClass = record
            parent_class : TGtkButtonClass;
            toggled : procedure (toggle_button:PGtkToggleButton); cdecl;
         end;

Type
  GTK_TOGGLE_BUTTON=PGtkToggleButton;
  GTK_TOGGLE_BUTTON_CLASS=PGtkToggleButtonClass;

function  GTK_TOGGLE_BUTTON_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_toggle_button_get_type';
function  GTK_IS_TOGGLE_BUTTON(obj:pointer):boolean;
function  GTK_IS_TOGGLE_BUTTON_CLASS(klass:pointer):boolean;

function  gtk_toggle_button_get_type:TGtkType;cdecl;external gtkdll name 'gtk_toggle_button_get_type';
function  gtk_toggle_button_new : PGtkWidget;cdecl;external gtkdll name 'gtk_toggle_button_new';
function  gtk_toggle_button_new_with_label (thelabel:Pgchar):PGtkWidget;cdecl;external gtkdll name 'gtk_toggle_button_new_with_label';
procedure gtk_toggle_button_set_mode(toggle_button:PGtkToggleButton; draw_indicator:gint);cdecl;external gtkdll name 'gtk_toggle_button_set_mode';
procedure gtk_toggle_button_set_active(toggle_button:PGtkToggleButton; is_active:gboolean);cdecl;external gtkdll name 'gtk_toggle_button_set_active';
function  gtk_toggle_button_get_active(toggle_button:PGtkToggleButton):gboolean;cdecl;external gtkdll name 'gtk_toggle_button_get_active';
procedure gtk_toggle_button_toggled(toggle_button:PGtkToggleButton);cdecl;external gtkdll name 'gtk_toggle_button_toggled';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  active(var a : TGtkToggleButton) : guint;
    begin
       active:=(a.flag0 and bm_TGtkToggleButton_active) shr bp_TGtkToggleButton_active;
    end;

procedure set_active(var a : TGtkToggleButton; __active : guint);
    begin
       a.flag0:=a.flag0 or ((__active shl bp_TGtkToggleButton_active) and bm_TGtkToggleButton_active);
    end;

function  draw_indicator(var a : TGtkToggleButton) : guint;
    begin
       draw_indicator:=(a.flag0 and bm_TGtkToggleButton_draw_indicator) shr bp_TGtkToggleButton_draw_indicator;
    end;

procedure set_draw_indicator(var a : TGtkToggleButton; __draw_indicator : guint);
    begin
       a.flag0:=a.flag0 or ((__draw_indicator shl bp_TGtkToggleButton_draw_indicator) and bm_TGtkToggleButton_draw_indicator);
    end;

function  GTK_IS_TOGGLE_BUTTON(obj:pointer):boolean;
begin
  GTK_IS_TOGGLE_BUTTON:=(obj<>nil) and GTK_IS_TOGGLE_BUTTON_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_TOGGLE_BUTTON_CLASS(klass:pointer):boolean;
begin
  GTK_IS_TOGGLE_BUTTON_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_TOGGLE_BUTTON_TYPE);
end;

{$endif read_implementation}


{
  $Log$
  Revision 1.1  2000-07-13 06:34:07  michael
  + Initial import

  Revision 1.1  1999/11/24 23:36:36  peter
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

  Revision 1.10  1999/10/06 17:42:50  peter
    * external is now only in the interface
    * removed gtk 1.0 support

  Revision 1.9  1999/07/23 16:13:16  peter
    * use packrecords C

  Revision 1.8  1999/05/11 00:39:35  peter
    * win32 fixes

  Revision 1.7  1999/05/10 19:18:34  peter
    * more fixes for the examples to work

  Revision 1.6  1999/05/10 15:20:34  peter
    * cdecl fixes

  Revision 1.5  1999/05/10 09:04:07  peter
    * gtk 1.2 port working

  Revision 1.4  1998/11/09 10:10:37  peter
    + C type casts are now correctly handled

  Revision 1.3  1998/10/21 20:23:19  peter
    * cdecl, packrecord fixes (from the gtk.tar.gz)
    * win32 support
    * gtk.pp,gdk.pp for an all in one unit

}


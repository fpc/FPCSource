{
   $Id$
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     PGtkCombo = ^TGtkCombo;
     TGtkCombo = record
          hbox : TGtkHBox;
          entry : PGtkWidget;
          button : PGtkWidget;
          popup : PGtkWidget;
          popwin : PGtkWidget;
          list : PGtkWidget;
          entry_change_id : guint;
          list_change_id : guint;
          flag0 : {$ifdef win32}longint{$else}word{$endif};
          current_button : guint16;
          activate_id : guint;
       end;

  const
     bm_value_in_list = 1;
     bp_value_in_list = 0;
     bm_ok_if_empty = 2;
     bp_ok_if_empty = 1;
     bm_case_sensitive = 4;
     bp_case_sensitive = 2;
     bm_use_arrows = 8;
     bp_use_arrows = 3;
     bm_use_arrows_always = 16;
     bp_use_arrows_always = 4;
function  value_in_list(var a : TGtkCombo) : guint;
procedure set_value_in_list(var a : TGtkCombo; __value_in_list : guint);
function  ok_if_empty(var a : TGtkCombo) : guint;
procedure set_ok_if_empty(var a : TGtkCombo; __ok_if_empty : guint);
function  case_sensitive(var a : TGtkCombo) : guint;
procedure set_case_sensitive(var a : TGtkCombo; __case_sensitive : guint);
function  use_arrows(var a : TGtkCombo) : guint;
procedure set_use_arrows(var a : TGtkCombo; __use_arrows : guint);
function  use_arrows_always(var a : TGtkCombo) : guint;
procedure set_use_arrows_always(var a : TGtkCombo; __use_arrows_always : guint);

  type
     PGtkComboClass = ^TGtkComboClass;
     TGtkComboClass = record
          parent_class : TGtkHBoxClass;
       end;

Type
  GTK_COMBO=PGtkCombo;
  GTK_COMBO_CLASS=PGtkComboClass;

function  GTK_COMBO_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_combo_get_type';
function  GTK_IS_COMBO(obj:pointer):boolean;
function  GTK_IS_COMBO_CLASS(klass:pointer):boolean;

function  gtk_combo_get_type:TGtkType;cdecl;external gtkdll name 'gtk_combo_get_type';
function  gtk_combo_new : PGtkWidget;cdecl;external gtkdll name 'gtk_combo_new';
procedure gtk_combo_set_value_in_list(combo:PGtkCombo; val:gint; ok_if_empty:gint);cdecl;external gtkdll name 'gtk_combo_set_value_in_list';
procedure gtk_combo_set_use_arrows(combo:PGtkCombo; val:gint);cdecl;external gtkdll name 'gtk_combo_set_use_arrows';
procedure gtk_combo_set_use_arrows_always(combo:PGtkCombo; val:gint);cdecl;external gtkdll name 'gtk_combo_set_use_arrows_always';
procedure gtk_combo_set_case_sensitive(combo:PGtkCombo; val:gint);cdecl;external gtkdll name 'gtk_combo_set_case_sensitive';
procedure gtk_combo_set_item_string(combo:PGtkCombo; item:PGtkItem; item_value:Pgchar);cdecl;external gtkdll name 'gtk_combo_set_item_string';
procedure gtk_combo_set_popdown_strings(combo:PGtkCombo; strings:PGList);cdecl;external gtkdll name 'gtk_combo_set_popdown_strings';
procedure gtk_combo_disable_activate(combo:PGtkCombo);cdecl;external gtkdll name 'gtk_combo_disable_activate';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  value_in_list(var a : TGtkCombo) : guint;
    begin
       value_in_list:=(a.flag0 and bm_value_in_list) shr bp_value_in_list;
    end;

procedure set_value_in_list(var a : TGtkCombo; __value_in_list : guint);
    begin
       a.flag0:=a.flag0 or ((__value_in_list shl bp_value_in_list) and bm_value_in_list);
    end;

function  ok_if_empty(var a : TGtkCombo) : guint;
    begin
       ok_if_empty:=(a.flag0 and bm_ok_if_empty) shr bp_ok_if_empty;
    end;

procedure set_ok_if_empty(var a : TGtkCombo; __ok_if_empty : guint);
    begin
       a.flag0:=a.flag0 or ((__ok_if_empty shl bp_ok_if_empty) and bm_ok_if_empty);
    end;

function  case_sensitive(var a : TGtkCombo) : guint;
    begin
       case_sensitive:=(a.flag0 and bm_case_sensitive) shr bp_case_sensitive;
    end;

procedure set_case_sensitive(var a : TGtkCombo; __case_sensitive : guint);
    begin
       a.flag0:=a.flag0 or ((__case_sensitive shl bp_case_sensitive) and bm_case_sensitive);
    end;

function  use_arrows(var a : TGtkCombo) : guint;
    begin
       use_arrows:=(a.flag0 and bm_use_arrows) shr bp_use_arrows;
    end;

procedure set_use_arrows(var a : TGtkCombo; __use_arrows : guint);
    begin
       a.flag0:=a.flag0 or ((__use_arrows shl bp_use_arrows) and bm_use_arrows);
    end;

function  use_arrows_always(var a : TGtkCombo) : guint;
    begin
       use_arrows_always:=(a.flag0 and bm_use_arrows_always) shr bp_use_arrows_always;
    end;

procedure set_use_arrows_always(var a : TGtkCombo; __use_arrows_always : guint);
    begin
       a.flag0:=a.flag0 or ((__use_arrows_always shl bp_use_arrows_always) and bm_use_arrows_always);
    end;

function  GTK_IS_COMBO(obj:pointer):boolean;
begin
  GTK_IS_COMBO:=(obj<>nil) and GTK_IS_COMBO_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_COMBO_CLASS(klass:pointer):boolean;
begin
  GTK_IS_COMBO_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_COMBO_TYPE);
end;

{$endif read_implementation}


{
  $Log$
  Revision 1.1.2.1  2000-09-09 18:42:52  peter
    * gtk win32 fixes

  Revision 1.1  2000/07/13 06:34:03  michael
  + Initial import

  Revision 1.1  1999/11/24 23:36:35  peter
    * moved to packages dir

  Revision 1.10  1999/10/21 08:42:01  florian
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

  Revision 1.9  1999/10/06 17:42:48  peter
    * external is now only in the interface
    * removed gtk 1.0 support

  Revision 1.8  1999/07/23 16:12:08  peter
    * use packrecords C

  Revision 1.7  1999/05/11 00:38:19  peter
    * win32 fixes

  Revision 1.6  1999/05/10 15:19:07  peter
    * cdecl fixes

  Revision 1.5  1999/05/07 17:40:17  peter
    * more updates

  Revision 1.4  1998/11/09 10:09:42  peter
    + C type casts are now correctly handled

  Revision 1.3  1998/10/21 20:22:17  peter
    * cdecl, packrecord fixes (from the gtk.tar.gz)
    * win32 support
    * gtk.pp,gdk.pp for an all in one unit

}


{
   $Id$
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type

     PGtkEditable = ^TGtkEditable;
     TGtkEditable = record
          widget : TGtkWidget;
          current_pos : guint;
          selection_start_pos : guint;
          selection_end_pos : guint;
          flag0 : {$ifdef win32}longint{$else}word{$endif};
          ic : PGdkIC;
          ic_attr : PGdkICAttr;
          clipboard_text : Pgchar;
       end;
     TGtkTextfunction = procedure (editable:PGtkEditable; time:guint32); cdecl; cdecl;

  const
     bm_TGtkEditable_has_selection = $1;
     bp_TGtkEditable_has_selection = 0;
     bm_TGtkEditable_editable = $2;
     bp_TGtkEditable_editable = 1;
     bm_TGtkEditable_visible = $4;
     bp_TGtkEditable_visible = 2;
function  has_selection(var a : TGtkEditable) : guint;
procedure set_has_selection(var a : TGtkEditable; __has_selection : guint);
function  editable(var a : TGtkEditable) : guint;
procedure set_editable(var a : TGtkEditable; __editable : guint);
function  visible(var a : TGtkEditable) : guint;
procedure set_visible(var a : TGtkEditable; __visible : guint);

  type
     PGtkEditableClass = ^TGtkEditableClass;
     TGtkEditableClass = record
          parent_class : TGtkWidgetClass;
          changed : procedure (editable:PGtkEditable);cdecl;
          insert_text : procedure (editable:PGtkEditable; thetext:Pgchar; length:gint; position:Pgint); cdecl;
          delete_text : procedure (editable:PGtkEditable; start_pos:gint; end_pos:gint); cdecl;
          activate : procedure (editable:PGtkEditable);cdecl;
          set_editable : procedure (editable:PGtkEditable; is_editable:gboolean);cdecl;
          move_cursor : procedure (editable:PGtkEditable; x:gint; y:gint);cdecl;
          move_word : procedure (editable:PGtkEditable; n:gint);cdecl;
          move_page : procedure (editable:PGtkEditable; x:gint; y:gint);cdecl;
          move_to_row : procedure (editable:PGtkEditable; row:gint);cdecl;
          move_to_column : procedure (editable:PGtkEditable; row:gint);cdecl;
          kill_char : procedure (editable:PGtkEditable; direction:gint);cdecl;
          kill_word : procedure (editable:PGtkEditable; direction:gint);cdecl;
          kill_line : procedure (editable:PGtkEditable; direction:gint);cdecl;
          cut_clipboard : procedure (editable:PGtkEditable);cdecl;
          copy_clipboard : procedure (editable:PGtkEditable);cdecl;
          paste_clipboard : procedure (editable:PGtkEditable);cdecl;
          update_text : procedure (editable:PGtkEditable; start_pos:gint; end_pos:gint); cdecl;
          get_chars : function (editable:PGtkEditable; start_pos:gint; end_pos:gint):Pgchar; cdecl;
          set_selection : procedure (editable:PGtkEditable; start_pos:gint; end_pos:gint); cdecl;
          set_position : procedure (editable:PGtkEditable; position:gint);cdecl;
       end;

Type
  GTK_EDITABLE=PGtkEditable;
  GTK_EDITABLE_CLASS=PGtkEditableClass;

function  GTK_EDITABLE_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_editable_get_type';
function  GTK_IS_EDITABLE(obj:pointer):boolean;
function  GTK_IS_EDITABLE_CLASS(klass:pointer):boolean;

function  gtk_editable_get_type:TGtkType;cdecl;external gtkdll name 'gtk_editable_get_type';
procedure gtk_editable_select_region(editable:PGtkEditable; start:gint; theend:gint);cdecl;external gtkdll name 'gtk_editable_select_region';
procedure gtk_editable_insert_text(editable:PGtkEditable; new_text:Pgchar; new_text_length:gint; position:Pgint);cdecl;external gtkdll name 'gtk_editable_insert_text';
procedure gtk_editable_delete_text(editable:PGtkEditable; start_pos:gint; end_pos:gint);cdecl;external gtkdll name 'gtk_editable_delete_text';
function  gtk_editable_get_chars (editable:PGtkEditable; start_pos:gint; end_pos:gint):Pgchar;cdecl;external gtkdll name 'gtk_editable_get_chars';
procedure gtk_editable_cut_clipboard(editable:PGtkEditable);cdecl;external gtkdll name 'gtk_editable_cut_clipboard';
procedure gtk_editable_copy_clipboard(editable:PGtkEditable);cdecl;external gtkdll name 'gtk_editable_copy_clipboard';
procedure gtk_editable_paste_clipboard(editable:PGtkEditable);cdecl;external gtkdll name 'gtk_editable_paste_clipboard';
procedure gtk_editable_claim_selection(editable:PGtkEditable; claim:gboolean; time:guint32);cdecl;external gtkdll name 'gtk_editable_claim_selection';
procedure gtk_editable_delete_selection(editable:PGtkEditable);cdecl;external gtkdll name 'gtk_editable_delete_selection';
procedure gtk_editable_changed(editable:PGtkEditable);cdecl;external gtkdll name 'gtk_editable_changed';
procedure gtk_editable_set_position(editable:PGtkEditable; position:gint);cdecl;external gtkdll name 'gtk_editable_set_position';
function  gtk_editable_get_position(editable:PGtkEditable):gint;cdecl;external gtkdll name 'gtk_editable_get_position';
procedure gtk_editable_set_editable(editable:PGtkEditable; is_editable:gboolean);cdecl;external gtkdll name 'gtk_editable_set_editable';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  has_selection(var a : TGtkEditable) : guint;
    begin
       has_selection:=(a.flag0 and bm_TGtkEditable_has_selection) shr bp_TGtkEditable_has_selection;
    end;

procedure set_has_selection(var a : TGtkEditable; __has_selection : guint);
    begin
       a.flag0:=a.flag0 or ((__has_selection shl bp_TGtkEditable_has_selection) and bm_TGtkEditable_has_selection);
    end;

function  editable(var a : TGtkEditable) : guint;
    begin
       editable:=(a.flag0 and bm_TGtkEditable_editable) shr bp_TGtkEditable_editable;
    end;

procedure set_editable(var a : TGtkEditable; __editable : guint);
    begin
       a.flag0:=a.flag0 or ((__editable shl bp_TGtkEditable_editable) and bm_TGtkEditable_editable);
    end;

function  visible(var a : TGtkEditable) : guint;
    begin
       visible:=(a.flag0 and bm_TGtkEditable_visible) shr bp_TGtkEditable_visible;
    end;

procedure set_visible(var a : TGtkEditable; __visible : guint);
    begin
       a.flag0:=a.flag0 or ((__visible shl bp_TGtkEditable_visible) and bm_TGtkEditable_visible);
    end;

function  GTK_IS_EDITABLE(obj:pointer):boolean;
begin
  GTK_IS_EDITABLE:=(obj<>nil) and GTK_IS_EDITABLE_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_EDITABLE_CLASS(klass:pointer):boolean;
begin
  GTK_IS_EDITABLE_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_EDITABLE_TYPE);
end;

{$endif read_implementation}


{
  $Log$
  Revision 1.1.2.1  2000-09-09 18:42:52  peter
    * gtk win32 fixes

  Revision 1.1  2000/07/13 06:34:04  michael
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

  Revision 1.8  1999/07/23 16:12:15  peter
    * use packrecords C

  Revision 1.7  1999/05/11 00:38:28  peter
    * win32 fixes

  Revision 1.6  1999/05/10 15:19:16  peter
    * cdecl fixes

  Revision 1.5  1999/05/10 09:03:07  peter
    * gtk 1.2 port working

  Revision 1.4  1998/11/09 10:09:48  peter
    + C type casts are now correctly handled

  Revision 1.3  1998/10/21 20:22:24  peter
    * cdecl, packrecord fixes (from the gtk.tar.gz)
    * win32 support
    * gtk.pp,gdk.pp for an all in one unit

}


{
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
     TGtkTextfunction = procedure (editable:PGtkEditable; time:guint32); cdecl;

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



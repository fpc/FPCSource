// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
{< public > }
{< private > }
   PGtkOldEditable = ^TGtkOldEditable;
   TGtkOldEditable = record
        widget : TGtkWidget;
        current_pos : guint;
        selection_start_pos : guint;
        selection_end_pos : guint;
        flag0 : word;
        clipboard_text : Pgchar;
     end;

   TGtkTextFunction = procedure (editable:PGtkOldEditable; time:guint32); cdecl;

{ Bindings actions  }
{ Virtual functions. get_chars is in paricular not a signal because
     it returns malloced memory. The others are not signals because
     they would not be particularly useful as such. (All changes to
     selection and position do not go through these functions)
    }
   PGtkOldEditableClass = ^TGtkOldEditableClass;
   TGtkOldEditableClass = record
        parent_class : TGtkWidgetClass;
        activate : procedure (editable:PGtkOldEditable); cdecl;
        set_editable : procedure (editable:PGtkOldEditable; is_editable:gboolean); cdecl;
        move_cursor : procedure (editable:PGtkOldEditable; x:gint; y:gint); cdecl;
        move_word : procedure (editable:PGtkOldEditable; n:gint); cdecl;
        move_page : procedure (editable:PGtkOldEditable; x:gint; y:gint); cdecl;
        move_to_row : procedure (editable:PGtkOldEditable; row:gint); cdecl;
        move_to_column : procedure (editable:PGtkOldEditable; row:gint); cdecl;
        kill_char : procedure (editable:PGtkOldEditable; direction:gint); cdecl;
        kill_word : procedure (editable:PGtkOldEditable; direction:gint); cdecl;
        kill_line : procedure (editable:PGtkOldEditable; direction:gint); cdecl;
        cut_clipboard : procedure (editable:PGtkOldEditable); cdecl;
        copy_clipboard : procedure (editable:PGtkOldEditable); cdecl;
        paste_clipboard : procedure (editable:PGtkOldEditable); cdecl;
        update_text : procedure (editable:PGtkOldEditable; start_pos:gint; end_pos:gint); cdecl;
        get_chars : function (editable:PGtkOldEditable; start_pos:gint; end_pos:gint):Pgchar; cdecl;
        set_selection : procedure (editable:PGtkOldEditable; start_pos:gint; end_pos:gint); cdecl;
        set_position : procedure (editable:PGtkOldEditable; position:gint); cdecl;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
   bm_TGtkOldEditable_has_selection = $1;
   bp_TGtkOldEditable_has_selection = 0;
   bm_TGtkOldEditable_editable = $2;
   bp_TGtkOldEditable_editable = 1;
   bm_TGtkOldEditable_visible = $4;
   bp_TGtkOldEditable_visible = 2;

function GTK_TYPE_OLD_EDITABLE : GType;
function GTK_OLD_EDITABLE(obj: pointer) : PGtkOldEditable;
function GTK_OLD_EDITABLE_CLASS(klass: pointer) : PGtkOldEditableClass;
function GTK_IS_OLD_EDITABLE(obj: pointer) : boolean;
function GTK_IS_OLD_EDITABLE_CLASS(klass: pointer) : boolean;
function GTK_OLD_EDITABLE_GET_CLASS(obj: pointer) : PGtkOldEditableClass;

function has_selection(var a : TGtkOldEditable) : guint;
procedure set_has_selection(var a : TGtkOldEditable; __has_selection : guint);
function editable(var a : TGtkOldEditable) : guint; overload;
procedure set_editable(var a : TGtkOldEditable; __editable : guint); overload;
function visible(var a : TGtkOldEditable) : guint; overload;
procedure set_visible(var a : TGtkOldEditable; __visible : guint); overload;

function gtk_old_editable_get_type:TGtkType; cdecl; external gtklib;
procedure gtk_old_editable_claim_selection(old_editable:PGtkOldEditable; claim:gboolean; time:guint32); cdecl; external gtklib;
procedure gtk_old_editable_changed(old_editable:PGtkOldEditable); cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_OLD_EDITABLE : GType;
begin
   GTK_TYPE_OLD_EDITABLE:=gtk_old_editable_get_type;
end;

function GTK_OLD_EDITABLE(obj: pointer) : PGtkOldEditable;
begin
   GTK_OLD_EDITABLE:=PGtkOldEditable(GTK_CHECK_CAST(obj,GTK_TYPE_OLD_EDITABLE));
end;

function GTK_OLD_EDITABLE_CLASS(klass: pointer) : PGtkOldEditableClass;
begin
   GTK_OLD_EDITABLE_CLASS:=PGtkOldEditableClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_OLD_EDITABLE));
end;

function GTK_IS_OLD_EDITABLE(obj: pointer) : boolean;
begin
   GTK_IS_OLD_EDITABLE:=GTK_CHECK_TYPE(obj,GTK_TYPE_OLD_EDITABLE);
end;

function GTK_IS_OLD_EDITABLE_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_OLD_EDITABLE_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_OLD_EDITABLE);
end;

function GTK_OLD_EDITABLE_GET_CLASS(obj: pointer) : PGtkOldEditableClass;
begin
   GTK_OLD_EDITABLE_GET_CLASS:=PGtkOldEditableClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_OLD_EDITABLE));
end;

function has_selection(var a : TGtkOldEditable) : guint;
begin
   has_selection:=(a.flag0 and bm_TGtkOldEditable_has_selection) shr bp_TGtkOldEditable_has_selection;
end;

procedure set_has_selection(var a : TGtkOldEditable; __has_selection : guint);
begin
   a.flag0:=a.flag0 or ((__has_selection shl bp_TGtkOldEditable_has_selection) and bm_TGtkOldEditable_has_selection);
end;

function editable(var a : TGtkOldEditable) : guint;
begin
   editable:=(a.flag0 and bm_TGtkOldEditable_editable) shr bp_TGtkOldEditable_editable;
end;

procedure set_editable(var a : TGtkOldEditable; __editable : guint);
begin
   a.flag0:=a.flag0 or ((__editable shl bp_TGtkOldEditable_editable) and bm_TGtkOldEditable_editable);
end;

function visible(var a : TGtkOldEditable) : guint;
begin
   visible:=(a.flag0 and bm_TGtkOldEditable_visible) shr bp_TGtkOldEditable_visible;
end;

procedure set_visible(var a : TGtkOldEditable; __visible : guint);
begin
   a.flag0:=a.flag0 or ((__visible shl bp_TGtkOldEditable_visible) and bm_TGtkOldEditable_visible);
end;

{$ENDIF read_implementation}
// included by gtk2.pas


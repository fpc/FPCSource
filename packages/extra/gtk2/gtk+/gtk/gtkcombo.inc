// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
{ you should access only the entry and list fields directly  }
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
        flag0 : word;
        current_button : guint16;
        activate_id : guint;
     end;

{ Padding for future expansion  }
   PGtkComboClass = ^TGtkComboClass;
   TGtkComboClass = record
        parent_class : TGtkHBoxClass;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
   bm_TGtkCombo_value_in_list = $1;
   bp_TGtkCombo_value_in_list = 0;
   bm_TGtkCombo_ok_if_empty = $2;
   bp_TGtkCombo_ok_if_empty = 1;
   bm_TGtkCombo_case_sensitive = $4;
   bp_TGtkCombo_case_sensitive = 2;
   bm_TGtkCombo_use_arrows = $8;
   bp_TGtkCombo_use_arrows = 3;
   bm_TGtkCombo_use_arrows_always = $10;
   bp_TGtkCombo_use_arrows_always = 4;


function GTK_TYPE_COMBO : GType;
function GTK_COMBO(obj: pointer) : PGtkCombo;
function GTK_COMBO_CLASS(klass: pointer) : PGtkComboClass;
function GTK_IS_COMBO(obj: pointer) : boolean;
function GTK_IS_COMBO_CLASS(klass: pointer) : boolean;
function GTK_COMBO_GET_CLASS(obj: pointer) : PGtkComboClass;


function value_in_list(var a : TGtkCombo) : guint;
procedure set_value_in_list(var a : TGtkCombo; __value_in_list : guint);
function ok_if_empty(var a : TGtkCombo) : guint;
procedure set_ok_if_empty(var a : TGtkCombo; __ok_if_empty : guint);
function case_sensitive(var a : TGtkCombo) : guint;
procedure set_case_sensitive(var a : TGtkCombo; __case_sensitive : guint);
function use_arrows(var a : TGtkCombo) : guint;
procedure set_use_arrows(var a : TGtkCombo; __use_arrows : guint);
function use_arrows_always(var a : TGtkCombo) : guint;
procedure set_use_arrows_always(var a : TGtkCombo; __use_arrows_always : guint);

function gtk_combo_get_type:TGtkType; cdecl; external gtklib;
function gtk_combo_new:PGtkWidget; cdecl; external gtklib;
{ the text in the entry must be or not be in the list  }
procedure gtk_combo_set_value_in_list(combo:PGtkCombo; val:gboolean; ok_if_empty:gboolean); cdecl; external gtklib;
{ set/unset arrows working for changing the value (can be annoying)  }
procedure gtk_combo_set_use_arrows(combo:PGtkCombo; val:gboolean); cdecl; external gtklib;
{ up/down arrows change value if current value not in list  }
procedure gtk_combo_set_use_arrows_always(combo:PGtkCombo; val:gboolean); cdecl; external gtklib;
{ perform case-sensitive compares  }
procedure gtk_combo_set_case_sensitive(combo:PGtkCombo; val:gboolean); cdecl; external gtklib;
{ call this function on an item if it isn't a label or you
   want it to have a different value to be displayed in the entry  }
procedure gtk_combo_set_item_string(combo:PGtkCombo; item:PGtkItem; item_value:Pgchar); cdecl; external gtklib;
{ simple interface  }
procedure gtk_combo_set_popdown_strings(combo:PGtkCombo; strings:PGList); cdecl; external gtklib;
procedure gtk_combo_disable_activate(combo:PGtkCombo); cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_COMBO : GType;
begin
   GTK_TYPE_COMBO:=gtk_combo_get_type;
end;

function GTK_COMBO(obj: pointer) : PGtkCombo;
begin
   GTK_COMBO:=PGtkCombo(GTK_CHECK_CAST(obj,GTK_TYPE_COMBO));
end;

function GTK_COMBO_CLASS(klass: pointer) : PGtkComboClass;
begin
   GTK_COMBO_CLASS:=PGtkComboClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_COMBO));
end;

function GTK_IS_COMBO(obj: pointer) : boolean;
begin
   GTK_IS_COMBO:=GTK_CHECK_TYPE(obj,GTK_TYPE_COMBO);
end;

function GTK_IS_COMBO_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_COMBO_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_COMBO);
end;

function GTK_COMBO_GET_CLASS(obj: pointer) : PGtkComboClass;
begin
   GTK_COMBO_GET_CLASS:=PGtkComboClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_COMBO));
end;

function value_in_list(var a : TGtkCombo) : guint;
begin
   value_in_list:=(a.flag0 and bm_TGtkCombo_value_in_list) shr bp_TGtkCombo_value_in_list;
end;

procedure set_value_in_list(var a : TGtkCombo; __value_in_list : guint);
begin
   a.flag0:=a.flag0 or ((__value_in_list shl bp_TGtkCombo_value_in_list) and bm_TGtkCombo_value_in_list);
end;

function ok_if_empty(var a : TGtkCombo) : guint;
begin
   ok_if_empty:=(a.flag0 and bm_TGtkCombo_ok_if_empty) shr bp_TGtkCombo_ok_if_empty;
end;

procedure set_ok_if_empty(var a : TGtkCombo; __ok_if_empty : guint);
begin
   a.flag0:=a.flag0 or ((__ok_if_empty shl bp_TGtkCombo_ok_if_empty) and bm_TGtkCombo_ok_if_empty);
end;

function case_sensitive(var a : TGtkCombo) : guint;
begin
   case_sensitive:=(a.flag0 and bm_TGtkCombo_case_sensitive) shr bp_TGtkCombo_case_sensitive;
end;

procedure set_case_sensitive(var a : TGtkCombo; __case_sensitive : guint);
begin
   a.flag0:=a.flag0 or ((__case_sensitive shl bp_TGtkCombo_case_sensitive) and bm_TGtkCombo_case_sensitive);
end;

function use_arrows(var a : TGtkCombo) : guint;
begin
   use_arrows:=(a.flag0 and bm_TGtkCombo_use_arrows) shr bp_TGtkCombo_use_arrows;
end;

procedure set_use_arrows(var a : TGtkCombo; __use_arrows : guint);
begin
   a.flag0:=a.flag0 or ((__use_arrows shl bp_TGtkCombo_use_arrows) and bm_TGtkCombo_use_arrows);
end;

function use_arrows_always(var a : TGtkCombo) : guint;
begin
   use_arrows_always:=(a.flag0 and bm_TGtkCombo_use_arrows_always) shr bp_TGtkCombo_use_arrows_always;
end;

procedure set_use_arrows_always(var a : TGtkCombo; __use_arrows_always : guint);
begin
   a.flag0:=a.flag0 or ((__use_arrows_always shl bp_TGtkCombo_use_arrows_always) and bm_TGtkCombo_use_arrows_always);
end;

{$ENDIF read_implementation}
// included by gtk2.pas


// included by gtk2.pas

{$IFDEF read_forward_definitions}
   PGtkLabel = ^TGtkLabel;
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   // internal type
   PGtkLabelSelectionInfo = pointer;

   TGtkLabel = record
        misc : TGtkMisc;
        _label : Pgchar;
        flag0 : word;
        mnemonic_keyval : guint;
        text : Pgchar;
        attrs : PPangoAttrList;
        effective_attrs : PPangoAttrList;
        layout : PPangoLayout;
        mnemonic_widget : PGtkWidget;
        mnemonic_window : PGtkWindow;
        select_info : PGtkLabelSelectionInfo;
     end;

{ Hook to customize right-click popup for selectable labels  }
{ Padding for future expansion  }
   PGtkLabelClass = ^TGtkLabelClass;
   TGtkLabelClass = record
        parent_class : TGtkMiscClass;
        move_cursor : procedure (_label:PGtkLabel; step:TGtkMovementStep; count:gint; extend_selection:gboolean); cdecl;
        copy_clipboard : procedure (_label:PGtkLabel); cdecl;
        populate_popup : procedure (_label:PGtkLabel; menu:PGtkMenu); cdecl;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}

const
   bm_TGtkLabel_jtype = $3;
   bp_TGtkLabel_jtype = 0;
   bm_TGtkLabel_wrap = $4;
   bp_TGtkLabel_wrap = 2;
   bm_TGtkLabel_use_underline = $8;
   bp_TGtkLabel_use_underline = 3;
   bm_TGtkLabel_use_markup = $10;
   bp_TGtkLabel_use_markup = 4;


function GTK_TYPE_LABEL : GType;
function GTK_LABEL(obj: pointer) : PGtkLabel;
function GTK_LABEL_CLASS(klass: pointer) : PGtkLabelClass;
function GTK_IS_LABEL(obj: pointer) : boolean;
function GTK_IS_LABEL_CLASS(klass: pointer) : boolean;
function GTK_LABEL_GET_CLASS(obj: pointer) : PGtkLabelClass;


function jtype(var a : TGtkLabel) : guint;
procedure set_jtype(var a : TGtkLabel; __jtype : guint);
function wrap(var a : TGtkLabel) : guint; overload;
procedure set_wrap(var a : TGtkLabel; __wrap : guint); overload;
function use_underline(var a : TGtkLabel) : guint; overload;
procedure set_use_underline(var a : TGtkLabel; __use_underline : guint); overload;
function use_markup(var a : TGtkLabel) : guint;
procedure set_use_markup(var a : TGtkLabel; __use_markup : guint);



function gtk_label_get_type:TGtkType; cdecl; external gtklib;
function gtk_label_new(str:Pchar):PGtkWidget; cdecl; external gtklib;
function gtk_label_new_with_mnemonic(str:Pchar):PGtkWidget; cdecl; external gtklib;
procedure gtk_label_set_text(_label:PGtkLabel; str:Pchar); cdecl; external gtklib;
function gtk_label_get_text(_label:PGtkLabel):Pgchar; cdecl; external gtklib;
procedure gtk_label_set_attributes(_label:PGtkLabel; attrs:PPangoAttrList); cdecl; external gtklib;
function gtk_label_get_attributes(_label:PGtkLabel):PPangoAttrList; cdecl; external gtklib;
procedure gtk_label_set_label(_label:PGtkLabel; str:Pgchar); cdecl; external gtklib;
function gtk_label_get_label(_label:PGtkLabel):Pgchar; cdecl; external gtklib;
procedure gtk_label_set_markup(_label:PGtkLabel; str:Pgchar); cdecl; external gtklib;
procedure gtk_label_set_use_markup(_label:PGtkLabel; setting:gboolean); cdecl; external gtklib;
function gtk_label_get_use_markup(_label:PGtkLabel):gboolean; cdecl; external gtklib;
procedure gtk_label_set_use_underline(_label:PGtkLabel; setting:gboolean); cdecl; external gtklib;
function gtk_label_get_use_underline(_label:PGtkLabel):gboolean; cdecl; external gtklib;
procedure gtk_label_set_markup_with_mnemonic(_label:PGtkLabel; str:Pgchar); cdecl; external gtklib;
function gtk_label_get_mnemonic_keyval(_label:PGtkLabel):guint; cdecl; external gtklib;
procedure gtk_label_set_mnemonic_widget(_label:PGtkLabel; widget:PGtkWidget); cdecl; external gtklib;
function gtk_label_get_mnemonic_widget(_label:PGtkLabel):PGtkWidget; cdecl; external gtklib;
procedure gtk_label_set_text_with_mnemonic(_label:PGtkLabel; str:Pgchar); cdecl; external gtklib;
procedure gtk_label_set_justify(_label:PGtkLabel; jtype:TGtkJustification); cdecl; external gtklib;
function gtk_label_get_justify(_label:PGtkLabel):TGtkJustification; cdecl; external gtklib;
procedure gtk_label_set_pattern(_label:PGtkLabel; pattern:Pgchar); cdecl; external gtklib;
procedure gtk_label_set_line_wrap(_label:PGtkLabel; wrap:gboolean); cdecl; external gtklib;
function gtk_label_get_line_wrap(_label:PGtkLabel):gboolean; cdecl; external gtklib;
procedure gtk_label_set_selectable(_label:PGtkLabel; setting:gboolean); cdecl; external gtklib;
function gtk_label_get_selectable(_label:PGtkLabel):gboolean; cdecl; external gtklib;
procedure gtk_label_select_region(_label:PGtkLabel; start_offset:gint; end_offset:gint); cdecl; external gtklib;
function gtk_label_get_selection_bounds(_label:PGtkLabel; start:Pgint; theEnd:Pgint):gboolean; cdecl; external gtklib;
function gtk_label_get_layout(_label:PGtkLabel):PPangoLayout; cdecl; external gtklib;
procedure gtk_label_get_layout_offsets(_label:PGtkLabel; x:Pgint; y:Pgint); cdecl; external gtklib;

{$ifndef GTK_DISABLE_DEPRECATED}
procedure gtk_label_set(_label:PGtkLabel; str:Pchar);

procedure gtk_label_get(_label:PGtkLabel; str:PPchar); cdecl; external gtklib;
{ Convenience function to set the name and pattern by parsing
   a string with embedded underscores, and return the appropriate
   key symbol for the accelerator.
  }
function gtk_label_parse_uline(_label:PGtkLabel; _string:Pgchar):guint; cdecl; external gtklib;
{$endif}
{ GTK_DISABLE_DEPRECATED  }
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_LABEL : GType;
begin
   GTK_TYPE_LABEL:=gtk_label_get_type;
end;

function GTK_LABEL(obj: pointer) : PGtkLabel;
begin
   GTK_LABEL:=PGtkLabel(GTK_CHECK_CAST(obj,GTK_TYPE_LABEL));
end;

function GTK_LABEL_CLASS(klass: pointer) : PGtkLabelClass;
begin
   GTK_LABEL_CLASS:=PGtkLabelClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_LABEL));
end;

function GTK_IS_LABEL(obj: pointer) : boolean;
begin
   GTK_IS_LABEL:=GTK_CHECK_TYPE(obj,GTK_TYPE_LABEL);
end;

function GTK_IS_LABEL_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_LABEL_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_LABEL);
end;

function GTK_LABEL_GET_CLASS(obj: pointer) : PGtkLabelClass;
begin
   GTK_LABEL_GET_CLASS:=PGtkLabelClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_LABEL));
end;

function jtype(var a : TGtkLabel) : guint;
begin
   jtype:=(a.flag0 and bm_TGtkLabel_jtype) shr bp_TGtkLabel_jtype;
end;

procedure set_jtype(var a : TGtkLabel; __jtype : guint);
begin
   a.flag0:=a.flag0 or ((__jtype shl bp_TGtkLabel_jtype) and bm_TGtkLabel_jtype);
end;

function wrap(var a : TGtkLabel) : guint;
begin
   wrap:=(a.flag0 and bm_TGtkLabel_wrap) shr bp_TGtkLabel_wrap;
end;

procedure set_wrap(var a : TGtkLabel; __wrap : guint);
begin
   a.flag0:=a.flag0 or ((__wrap shl bp_TGtkLabel_wrap) and bm_TGtkLabel_wrap);
end;

function use_underline(var a : TGtkLabel) : guint;
begin
   use_underline:=(a.flag0 and bm_TGtkLabel_use_underline) shr bp_TGtkLabel_use_underline;
end;

procedure set_use_underline(var a : TGtkLabel; __use_underline : guint);
begin
   a.flag0:=a.flag0 or ((__use_underline shl bp_TGtkLabel_use_underline) and bm_TGtkLabel_use_underline);
end;

function use_markup(var a : TGtkLabel) : guint;
begin
   use_markup:=(a.flag0 and bm_TGtkLabel_use_markup) shr bp_TGtkLabel_use_markup;
end;

procedure set_use_markup(var a : TGtkLabel; __use_markup : guint);
begin
   a.flag0:=a.flag0 or ((__use_markup shl bp_TGtkLabel_use_markup) and bm_TGtkLabel_use_markup);
end;

{$ifndef GTK_DISABLE_DEPRECATED}
procedure gtk_label_set(_label:PGtkLabel; str:Pchar);
begin
   gtk_label_set_text(_label,str);
end;
{$endif}
{ GTK_DISABLE_DEPRECATED  }

{$ENDIF read_implementation}
// included by gtk2.pas


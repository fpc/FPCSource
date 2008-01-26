// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkCellRendererText = ^TGtkCellRendererText;
   TGtkCellRendererText = record
        parent : TGtkCellRenderer;
        text : Pgchar;
        font : PPangoFontDescription;
        font_scale : gdouble;
        foreground : TPangoColor;
        background : TPangoColor;
        extra_attrs : PPangoAttrList;
        underline_style : TPangoUnderline;
        rise : gint;
        fixed_height_rows : gint;
        flag0 : word;
     end;

{ Padding for future expansion  }
   PGtkCellRendererTextClass = ^TGtkCellRendererTextClass;
   TGtkCellRendererTextClass = record
        parent_class : TGtkCellRendererClass;
        edited : procedure (cell_renderer_text:PGtkCellRendererText; path:Pgchar; new_text:Pgchar); cdecl;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}

const
   bm_TGtkCellRendererText_strikethrough = $1;
   bp_TGtkCellRendererText_strikethrough = 0;
   bm_TGtkCellRendererText_editable = $2;
   bp_TGtkCellRendererText_editable = 1;
   bm_TGtkCellRendererText_scale_set = $4;
   bp_TGtkCellRendererText_scale_set = 2;
   bm_TGtkCellRendererText_foreground_set = $8;
   bp_TGtkCellRendererText_foreground_set = 3;
   bm_TGtkCellRendererText_background_set = $10;
   bp_TGtkCellRendererText_background_set = 4;
   bm_TGtkCellRendererText_underline_set = $20;
   bp_TGtkCellRendererText_underline_set = 5;
   bm_TGtkCellRendererText_rise_set = $40;
   bp_TGtkCellRendererText_rise_set = 6;
   bm_TGtkCellRendererText_strikethrough_set = $80;
   bp_TGtkCellRendererText_strikethrough_set = 7;
   bm_TGtkCellRendererText_editable_set = $100;
   bp_TGtkCellRendererText_editable_set = 8;
   bm_TGtkCellRendererText_calc_fixed_height = $200;
   bp_TGtkCellRendererText_calc_fixed_height = 9;


function GTK_TYPE_CELL_RENDERER_TEXT : GType;
function GTK_CELL_RENDERER_TEXT(obj: pointer) : PGtkCellRendererText;
function GTK_CELL_RENDERER_TEXT_CLASS(klass: pointer) : PGtkCellRendererTextClass;
function GTK_IS_CELL_RENDERER_TEXT(obj: pointer) : boolean;
function GTK_IS_CELL_RENDERER_TEXT_CLASS(klass: pointer) : boolean;
function GTK_CELL_RENDERER_TEXT_GET_CLASS(obj: pointer) : PGtkCellRendererTextClass;


function strikethrough(var a : TGtkCellRendererText) : guint; overload;
procedure set_strikethrough(var a : TGtkCellRendererText; __strikethrough : guint); overload;
function editable(var a : TGtkCellRendererText) : guint; overload;
procedure set_editable(var a : TGtkCellRendererText; __editable : guint); overload;
function scale_set(var a : TGtkCellRendererText) : guint; overload;
procedure set_scale_set(var a : TGtkCellRendererText; __scale_set : guint); overload;
function foreground_set(var a : TGtkCellRendererText) : guint;
procedure set_foreground_set(var a : TGtkCellRendererText; __foreground_set : guint);
function background_set(var a : TGtkCellRendererText) : guint;
procedure set_background_set(var a : TGtkCellRendererText; __background_set : guint);
function underline_set(var a : TGtkCellRendererText) : guint; overload;
procedure set_underline_set(var a : TGtkCellRendererText; __underline_set : guint); overload;
function rise_set(var a : TGtkCellRendererText) : guint; overload;
procedure set_rise_set(var a : TGtkCellRendererText; __rise_set : guint); overload;
function strikethrough_set(var a : TGtkCellRendererText) : guint; overload;
procedure set_strikethrough_set(var a : TGtkCellRendererText; __strikethrough_set : guint); overload;
function editable_set(var a : TGtkCellRendererText) : guint; overload;
procedure set_editable_set(var a : TGtkCellRendererText; __editable_set : guint); overload;
function calc_fixed_height(var a : TGtkCellRendererText) : guint;
procedure set_calc_fixed_height(var a : TGtkCellRendererText; __calc_fixed_height : guint);

function gtk_cell_renderer_text_get_type:TGtkType; cdecl; external gtklib;
function gtk_cell_renderer_text_new:PGtkCellRenderer; cdecl; external gtklib;
procedure gtk_cell_renderer_text_set_fixed_height_from_font(renderer:PGtkCellRendererText; number_of_rows:gint); cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_CELL_RENDERER_TEXT : GType;
begin
   GTK_TYPE_CELL_RENDERER_TEXT:=gtk_cell_renderer_text_get_type;
end;

function GTK_CELL_RENDERER_TEXT(obj: pointer) : PGtkCellRendererText;
begin
   GTK_CELL_RENDERER_TEXT:=PGtkCellRendererText(GTK_CHECK_CAST(obj,GTK_TYPE_CELL_RENDERER_TEXT));
end;

function GTK_CELL_RENDERER_TEXT_CLASS(klass: pointer) : PGtkCellRendererTextClass;
begin
   GTK_CELL_RENDERER_TEXT_CLASS:=PGtkCellRendererTextClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_CELL_RENDERER_TEXT));
end;

function GTK_IS_CELL_RENDERER_TEXT(obj: pointer) : boolean;
begin
   GTK_IS_CELL_RENDERER_TEXT:=GTK_CHECK_TYPE(obj,GTK_TYPE_CELL_RENDERER_TEXT);
end;

function GTK_IS_CELL_RENDERER_TEXT_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_CELL_RENDERER_TEXT_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_CELL_RENDERER_TEXT);
end;

function GTK_CELL_RENDERER_TEXT_GET_CLASS(obj: pointer) : PGtkCellRendererTextClass;
begin
   GTK_CELL_RENDERER_TEXT_GET_CLASS:=PGtkCellRendererTextClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_CELL_RENDERER_TEXT));
end;

function strikethrough(var a : TGtkCellRendererText) : guint;
begin
   strikethrough:=(a.flag0 and bm_TGtkCellRendererText_strikethrough) shr bp_TGtkCellRendererText_strikethrough;
end;

procedure set_strikethrough(var a : TGtkCellRendererText; __strikethrough : guint);
begin
   a.flag0:=a.flag0 or ((__strikethrough shl bp_TGtkCellRendererText_strikethrough) and bm_TGtkCellRendererText_strikethrough);
end;

function editable(var a : TGtkCellRendererText) : guint;
begin
   editable:=(a.flag0 and bm_TGtkCellRendererText_editable) shr bp_TGtkCellRendererText_editable;
end;

procedure set_editable(var a : TGtkCellRendererText; __editable : guint);
begin
   a.flag0:=a.flag0 or ((__editable shl bp_TGtkCellRendererText_editable) and bm_TGtkCellRendererText_editable);
end;

function scale_set(var a : TGtkCellRendererText) : guint;
begin
   scale_set:=(a.flag0 and bm_TGtkCellRendererText_scale_set) shr bp_TGtkCellRendererText_scale_set;
end;

procedure set_scale_set(var a : TGtkCellRendererText; __scale_set : guint);
begin
   a.flag0:=a.flag0 or ((__scale_set shl bp_TGtkCellRendererText_scale_set) and bm_TGtkCellRendererText_scale_set);
end;

function foreground_set(var a : TGtkCellRendererText) : guint;
begin
   foreground_set:=(a.flag0 and bm_TGtkCellRendererText_foreground_set) shr bp_TGtkCellRendererText_foreground_set;
end;

procedure set_foreground_set(var a : TGtkCellRendererText; __foreground_set : guint);
begin
   a.flag0:=a.flag0 or ((__foreground_set shl bp_TGtkCellRendererText_foreground_set) and bm_TGtkCellRendererText_foreground_set);
end;

function background_set(var a : TGtkCellRendererText) : guint;
begin
   background_set:=(a.flag0 and bm_TGtkCellRendererText_background_set) shr bp_TGtkCellRendererText_background_set;
end;

procedure set_background_set(var a : TGtkCellRendererText; __background_set : guint);
begin
   a.flag0:=a.flag0 or ((__background_set shl bp_TGtkCellRendererText_background_set) and bm_TGtkCellRendererText_background_set);
end;

function underline_set(var a : TGtkCellRendererText) : guint;
begin
   underline_set:=(a.flag0 and bm_TGtkCellRendererText_underline_set) shr bp_TGtkCellRendererText_underline_set;
end;

procedure set_underline_set(var a : TGtkCellRendererText; __underline_set : guint);
begin
   a.flag0:=a.flag0 or ((__underline_set shl bp_TGtkCellRendererText_underline_set) and bm_TGtkCellRendererText_underline_set);
end;

function rise_set(var a : TGtkCellRendererText) : guint;
begin
   rise_set:=(a.flag0 and bm_TGtkCellRendererText_rise_set) shr bp_TGtkCellRendererText_rise_set;
end;

procedure set_rise_set(var a : TGtkCellRendererText; __rise_set : guint);
begin
   a.flag0:=a.flag0 or ((__rise_set shl bp_TGtkCellRendererText_rise_set) and bm_TGtkCellRendererText_rise_set);
end;

function strikethrough_set(var a : TGtkCellRendererText) : guint;
begin
   strikethrough_set:=(a.flag0 and bm_TGtkCellRendererText_strikethrough_set) shr bp_TGtkCellRendererText_strikethrough_set;
end;

procedure set_strikethrough_set(var a : TGtkCellRendererText; __strikethrough_set : guint);
begin
   a.flag0:=a.flag0 or ((__strikethrough_set shl bp_TGtkCellRendererText_strikethrough_set) and bm_TGtkCellRendererText_strikethrough_set);
end;

function editable_set(var a : TGtkCellRendererText) : guint;
begin
   editable_set:=(a.flag0 and bm_TGtkCellRendererText_editable_set) shr bp_TGtkCellRendererText_editable_set;
end;

procedure set_editable_set(var a : TGtkCellRendererText; __editable_set : guint);
begin
   a.flag0:=a.flag0 or ((__editable_set shl bp_TGtkCellRendererText_editable_set) and bm_TGtkCellRendererText_editable_set);
end;

function calc_fixed_height(var a : TGtkCellRendererText) : guint;
begin
   calc_fixed_height:=(a.flag0 and bm_TGtkCellRendererText_calc_fixed_height) shr bp_TGtkCellRendererText_calc_fixed_height;
end;

procedure set_calc_fixed_height(var a : TGtkCellRendererText; __calc_fixed_height : guint);
begin
   a.flag0:=a.flag0 or ((__calc_fixed_height shl bp_TGtkCellRendererText_calc_fixed_height) and bm_TGtkCellRendererText_calc_fixed_height);
end;

{$ENDIF read_implementation}
// included by gtk2.pas


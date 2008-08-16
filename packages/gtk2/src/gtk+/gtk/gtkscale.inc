// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkScale = ^TGtkScale;
   TGtkScale = record
        range : TGtkRange;
        digits : gint;
        flag0 : word;
     end;


{ Padding for future expansion  }
   PGtkScaleClass = ^TGtkScaleClass;
   TGtkScaleClass = record
        parent_class : TGtkRangeClass;
        format_value : function (scale:PGtkScale; value:gdouble):Pgchar; cdecl;
        draw_value : procedure (scale:PGtkScale); cdecl;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
   bm_TGtkScale_draw_value = $1;
   bp_TGtkScale_draw_value = 0;
   bm_TGtkScale_value_pos = $6;
   bp_TGtkScale_value_pos = 1;


function GTK_TYPE_SCALE : GType;
function GTK_SCALE(obj: pointer) : PGtkScale;
function GTK_SCALE_CLASS(klass: pointer) : PGtkScaleClass;
function GTK_IS_SCALE(obj: pointer) : boolean;
function GTK_IS_SCALE_CLASS(klass: pointer) : boolean;
function GTK_SCALE_GET_CLASS(obj: pointer) : PGtkScaleClass;


function draw_value(var a : TGtkScale) : guint;
procedure set_draw_value(var a : TGtkScale; __draw_value : guint);
function value_pos(var a : TGtkScale) : guint;
procedure set_value_pos(var a : TGtkScale; __value_pos : guint);

function gtk_scale_get_type:TGtkType; cdecl; external gtklib;
procedure gtk_scale_set_digits(scale:PGtkScale; digits:gint); cdecl; external gtklib;
function gtk_scale_get_digits(scale:PGtkScale):gint; cdecl; external gtklib;
procedure gtk_scale_set_draw_value(scale:PGtkScale; draw_value:gboolean); cdecl; external gtklib;
function gtk_scale_get_draw_value(scale:PGtkScale):gboolean; cdecl; external gtklib;
procedure gtk_scale_set_value_pos(scale:PGtkScale; pos:TGtkPositionType); cdecl; external gtklib;
function gtk_scale_get_value_pos(scale:PGtkScale):TGtkPositionType; cdecl; external gtklib;
procedure _gtk_scale_get_value_size(scale:PGtkScale; width:Pgint; height:Pgint); cdecl; external gtklib;
function _gtk_scale_format_value(scale:PGtkScale; value:gdouble):Pgchar; cdecl; external gtklib;

{$ifdef HasGTK2_4}
function gtk_scale_get_layout(scale: PGtkScale): PPangoLayout; cdecl; external gtklib;
procedure gtk_scale_get_layout_offsets(scale: PGtkScale; x: Pgint; y: Pgint); cdecl; external gtklib;
{$endif}

{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_SCALE : GType;
begin
   GTK_TYPE_SCALE:=gtk_scale_get_type;
end;

function GTK_SCALE(obj: pointer) : PGtkScale;
begin
   GTK_SCALE:=PGtkScale(GTK_CHECK_CAST(obj,GTK_TYPE_SCALE));
end;

function GTK_SCALE_CLASS(klass: pointer) : PGtkScaleClass;
begin
   GTK_SCALE_CLASS:=PGtkScaleClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_SCALE));
end;

function GTK_IS_SCALE(obj: pointer) : boolean;
begin
   GTK_IS_SCALE:=GTK_CHECK_TYPE(obj,GTK_TYPE_SCALE);
end;

function GTK_IS_SCALE_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_SCALE_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_SCALE);
end;

function GTK_SCALE_GET_CLASS(obj: pointer) : PGtkScaleClass;
begin
   GTK_SCALE_GET_CLASS:=PGtkScaleClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_SCALE));
end;

function draw_value(var a : TGtkScale) : guint;
begin
   draw_value:=(a.flag0 and bm_TGtkScale_draw_value) shr bp_TGtkScale_draw_value;
end;

procedure set_draw_value(var a : TGtkScale; __draw_value : guint);
begin
   a.flag0:=a.flag0 or ((__draw_value shl bp_TGtkScale_draw_value) and bm_TGtkScale_draw_value);
end;

function value_pos(var a : TGtkScale) : guint;
begin
   value_pos:=(a.flag0 and bm_TGtkScale_value_pos) shr bp_TGtkScale_value_pos;
end;

procedure set_value_pos(var a : TGtkScale; __value_pos : guint);
begin
   a.flag0:=a.flag0 or ((__value_pos shl bp_TGtkScale_value_pos) and bm_TGtkScale_value_pos);
end;

{$ENDIF read_implementation}
// included by gtk2.pas


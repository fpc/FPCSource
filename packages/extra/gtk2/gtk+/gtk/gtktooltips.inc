// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkTooltips = ^TGtkTooltips;

   PGtkTooltipsData = ^TGtkTooltipsData;
   TGtkTooltipsData = record
        tooltips : PGtkTooltips;
        widget : PGtkWidget;
        tip_text : Pgchar;
        tip_private : Pgchar;
     end;

   TGtkTooltips = record
        parent_instance : TGtkObject;
        tip_window : PGtkWidget;
        tip_label : PGtkWidget;
        active_tips_data : PGtkTooltipsData;
        tips_data_list : PGList;
        flag0 : longint;
        flag1 : word;
        timer_tag : gint;
        last_popdown : TGTimeVal;
     end;

{ Padding for future expansion  }
   PGtkTooltipsClass = ^TGtkTooltipsClass;
   TGtkTooltipsClass = record
        parent_class : TGtkObjectClass;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
   bm_TGtkTooltips_delay = $3FFFFFFF;
   bp_TGtkTooltips_delay = 0;
   bm_TGtkTooltips_enabled = $40000000;
   bp_TGtkTooltips_enabled = 30;
   bm_TGtkTooltips_have_grab = $80000000;
   bp_TGtkTooltips_have_grab = 31;
   bm_TGtkTooltips_use_sticky_delay = $1;
   bp_TGtkTooltips_use_sticky_delay = 0;


function GTK_TYPE_TOOLTIPS : GType;
function GTK_TOOLTIPS(obj: pointer) : PGtkTooltips;
function GTK_TOOLTIPS_CLASS(klass: pointer) : PGtkTooltipsClass;
function GTK_IS_TOOLTIPS(obj: pointer) : boolean;
function GTK_IS_TOOLTIPS_CLASS(klass: pointer) : boolean;
function GTK_TOOLTIPS_GET_CLASS(obj: pointer) : PGtkTooltipsClass;

function delay(var a : TGtkTooltips) : guint;
procedure set_delay(var a : TGtkTooltips; __delay : guint);
function enabled(var a : TGtkTooltips) : guint;
procedure set_enabled(var a : TGtkTooltips; __enabled : guint);
function have_grab(var a : TGtkTooltips) : guint; overload;
procedure set_have_grab(var a : TGtkTooltips; __have_grab : guint); overload;
function use_sticky_delay(var a : TGtkTooltips) : guint;
procedure set_use_sticky_delay(var a : TGtkTooltips; __use_sticky_delay : guint);

function gtk_tooltips_get_type:TGtkType; cdecl; external gtklib;
function gtk_tooltips_new:PGtkTooltips; cdecl; external gtklib;
procedure gtk_tooltips_enable(tooltips:PGtkTooltips); cdecl; external gtklib;
procedure gtk_tooltips_disable(tooltips:PGtkTooltips); cdecl; external gtklib;

{$ifndef GTK_DISABLE_DEPRECATED}
procedure gtk_tooltips_set_delay(tooltips:PGtkTooltips; delay:guint); cdecl; external gtklib;
{$endif}
{ GTK_DISABLE_DEPRECATED  }

procedure gtk_tooltips_set_tip(tooltips:PGtkTooltips; widget:PGtkWidget; tip_text:Pgchar; tip_private:Pgchar); cdecl; external gtklib;
function gtk_tooltips_data_get(widget:PGtkWidget):PGtkTooltipsData; cdecl; external gtklib;
procedure gtk_tooltips_force_window(tooltips:PGtkTooltips); cdecl; external gtklib;
procedure _gtk_tooltips_toggle_keyboard_mode(widget:PGtkWidget); cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_TOOLTIPS : GType;
begin
   GTK_TYPE_TOOLTIPS:=gtk_tooltips_get_type;
end;

function GTK_TOOLTIPS(obj: pointer) : PGtkTooltips;
begin
   GTK_TOOLTIPS:=PGtkTooltips(GTK_CHECK_CAST(obj,GTK_TYPE_TOOLTIPS));
end;

function GTK_TOOLTIPS_CLASS(klass: pointer) : PGtkTooltipsClass;
begin
   GTK_TOOLTIPS_CLASS:=PGtkTooltipsClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_TOOLTIPS));
end;

function GTK_IS_TOOLTIPS(obj: pointer) : boolean;
begin
   GTK_IS_TOOLTIPS:=GTK_CHECK_TYPE(obj,GTK_TYPE_TOOLTIPS);
end;

function GTK_IS_TOOLTIPS_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_TOOLTIPS_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_TOOLTIPS);
end;

function GTK_TOOLTIPS_GET_CLASS(obj: pointer) : PGtkTooltipsClass;
begin
   GTK_TOOLTIPS_GET_CLASS:=PGtkTooltipsClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_TOOLTIPS));
end;

function delay(var a : TGtkTooltips) : guint;
begin
   delay:=(a.flag0 and bm_TGtkTooltips_delay) shr bp_TGtkTooltips_delay;
end;

procedure set_delay(var a : TGtkTooltips; __delay : guint);
begin
   a.flag0:=a.flag0 or ((__delay shl bp_TGtkTooltips_delay) and bm_TGtkTooltips_delay);
end;

function enabled(var a : TGtkTooltips) : guint;
begin
   enabled:=(a.flag0 and bm_TGtkTooltips_enabled) shr bp_TGtkTooltips_enabled;
end;

procedure set_enabled(var a : TGtkTooltips; __enabled : guint);
begin
   a.flag0:=a.flag0 or ((__enabled shl bp_TGtkTooltips_enabled) and bm_TGtkTooltips_enabled);
end;

function have_grab(var a : TGtkTooltips) : guint;
begin
   have_grab:=(a.flag0 and bm_TGtkTooltips_have_grab) shr bp_TGtkTooltips_have_grab;
end;

procedure set_have_grab(var a : TGtkTooltips; __have_grab : guint);
begin
   a.flag0:=a.flag0 or ((__have_grab shl bp_TGtkTooltips_have_grab) and bm_TGtkTooltips_have_grab);
end;

function use_sticky_delay(var a : TGtkTooltips) : guint;
begin
   use_sticky_delay:=(a.flag0 and bm_TGtkTooltips_use_sticky_delay) shr bp_TGtkTooltips_use_sticky_delay;
end;

procedure set_use_sticky_delay(var a : TGtkTooltips; __use_sticky_delay : guint);
begin
   a.flag0:=a.flag0 or ((__use_sticky_delay shl bp_TGtkTooltips_use_sticky_delay) and bm_TGtkTooltips_use_sticky_delay);
end;

{$ENDIF read_implementation}
// included by gtk2.pas


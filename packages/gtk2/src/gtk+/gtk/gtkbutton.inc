// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkButton = ^TGtkButton;
   TGtkButton = record
        bin : TGtkBin;
        event_window : PGdkWindow;
        label_text : Pgchar;
        activate_timeout : guint;
        flag0 : word;
     end;

{ Padding for future expansion  }
   PGtkButtonClass = ^TGtkButtonClass;
   TGtkButtonClass = record
        parent_class : TGtkBinClass;
        pressed : procedure (button:PGtkButton); cdecl;
        released : procedure (button:PGtkButton); cdecl;
        clicked : procedure (button:PGtkButton); cdecl;
        enter : procedure (button:PGtkButton); cdecl;
        leave : procedure (button:PGtkButton); cdecl;
        activate : procedure (button:PGtkButton); cdecl;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
   bm_TGtkButton_constructed = $1;
   bp_TGtkButton_constructed = 0;
   bm_TGtkButton_in_button = $2;
   bp_TGtkButton_in_button = 1;
   bm_TGtkButton_button_down = $4;
   bp_TGtkButton_button_down = 2;
   bm_TGtkButton_relief = $18;
   bp_TGtkButton_relief = 3;
   bm_TGtkButton_use_underline = $20;
   bp_TGtkButton_use_underline = 5;
   bm_TGtkButton_use_stock = $40;
   bp_TGtkButton_use_stock = 6;
   bm_TGtkButton_depressed = $80;
   bp_TGtkButton_depressed = 7;
   bm_TGtkButton_depress_on_activate = $100;
   bp_TGtkButton_depress_on_activate = 8;


function GTK_TYPE_BUTTON : GType;
function GTK_BUTTON(obj: pointer) : PGtkButton;
function GTK_BUTTON_CLASS(klass: pointer) : PGtkButtonClass;
function GTK_IS_BUTTON(obj: pointer) : boolean;
function GTK_IS_BUTTON_CLASS(klass: pointer) : boolean;
function GTK_BUTTON_GET_CLASS(obj: pointer) : PGtkButtonClass;


function constructed(var a : TGtkButton) : guint;
procedure set_constructed(var a : TGtkButton; __constructed : guint);
function in_button(var a : TGtkButton) : guint;
procedure set_in_button(var a : TGtkButton; __in_button : guint);
function button_down(var a : TGtkButton) : guint;
procedure set_button_down(var a : TGtkButton; __button_down : guint);
function relief(var a : TGtkButton) : guint;
procedure set_relief(var a : TGtkButton; __relief : guint);
function use_underline(var a : TGtkButton) : guint; overload;
procedure set_use_underline(var a : TGtkButton; __use_underline : guint); overload;
function use_stock(var a : TGtkButton) : guint;
procedure set_use_stock(var a : TGtkButton; __use_stock : guint);
function depressed(var a : TGtkButton) : guint;
procedure set_depressed(var a : TGtkButton; __depressed : guint);
function depress_on_activate(var a : TGtkButton) : guint;
procedure set_depress_on_activate(var a : TGtkButton; __depress_on_activate : guint);


function gtk_button_get_type:TGtkType; cdecl; external gtklib;
function gtk_button_new:PGtkWidget; cdecl; external gtklib;
function gtk_button_new_with_label(_label:Pgchar):PGtkWidget; cdecl; external gtklib;
function gtk_button_new_from_stock(stock_id:Pgchar):PGtkWidget; cdecl; external gtklib;
function gtk_button_new_with_mnemonic(_label:Pgchar):PGtkWidget; cdecl; external gtklib;
procedure gtk_button_pressed(button:PGtkButton); cdecl; external gtklib;
procedure gtk_button_released(button:PGtkButton); cdecl; external gtklib;
procedure gtk_button_clicked(button:PGtkButton); cdecl; external gtklib;
procedure gtk_button_enter(button:PGtkButton); cdecl; external gtklib;
procedure gtk_button_leave(button:PGtkButton); cdecl; external gtklib;
procedure gtk_button_set_relief(button:PGtkButton; newstyle:TGtkReliefStyle); cdecl; external gtklib;
function gtk_button_get_relief(button:PGtkButton):TGtkReliefStyle; cdecl; external gtklib;
procedure gtk_button_set_label(button:PGtkButton; _label:Pgchar); cdecl; external gtklib;
function gtk_button_get_label(button:PGtkButton):Pgchar; cdecl; external gtklib;
procedure gtk_button_set_use_underline(button:PGtkButton; use_underline:gboolean); cdecl; external gtklib;
function gtk_button_get_use_underline(button:PGtkButton):gboolean; cdecl; external gtklib;
procedure gtk_button_set_use_stock(button:PGtkButton; use_stock:gboolean); cdecl; external gtklib;
function gtk_button_get_use_stock(button:PGtkButton):gboolean; cdecl; external gtklib;
procedure _gtk_button_set_depressed(button:PGtkButton; depressed:gboolean); cdecl; external gtklib;
procedure _gtk_button_paint(button:PGtkButton; area:PGdkRectangle; state_type:TGtkStateType; shadow_type:TGtkShadowType; main_detail:Pgchar;
            default_detail:Pgchar); cdecl; external gtklib;

{$IFDEF HasGTK2_4}
function  gtk_button_get_focus_on_click(window:PGtkButton):gboolean; cdecl; external gtklib;
procedure gtk_button_set_focus_on_click(window:PGtkButton; setting:gboolean); cdecl; external gtklib;
procedure gtk_button_get_alignment(window:PGtkButton; x,y: PGFloat) cdecl; external gtklib;
procedure gtk_button_set_alignment(window:PGtkButton; x,y: GFloat); cdecl; external gtklib;
{$ENDIF HasGTK2_4}

{$ifdef HasGTK2_6}
function gtk_button_get_image(button:PGtkButton):PGtkWidget; cdecl; external gtklib;
procedure gtk_button_set_image(button:PGtkButton;image:PGtkWidget); cdecl; external gtklib;
{$endif}

{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_BUTTON : GType;
begin
   GTK_TYPE_BUTTON:=gtk_button_get_type;
end;

function GTK_BUTTON(obj: pointer) : PGtkButton;
begin
   GTK_BUTTON:=PGtkButton(GTK_CHECK_CAST(obj,GTK_TYPE_BUTTON));
end;

function GTK_BUTTON_CLASS(klass: pointer) : PGtkButtonClass;
begin
   GTK_BUTTON_CLASS:=PGtkButtonClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_BUTTON));
end;

function GTK_IS_BUTTON(obj: pointer) : boolean;
begin
   GTK_IS_BUTTON:=GTK_CHECK_TYPE(obj,GTK_TYPE_BUTTON);
end;

function GTK_IS_BUTTON_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_BUTTON_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_BUTTON);
end;

function GTK_BUTTON_GET_CLASS(obj: pointer) : PGtkButtonClass;
begin
   GTK_BUTTON_GET_CLASS:=PGtkButtonClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_BUTTON));
end;

function constructed(var a : TGtkButton) : guint;
begin
   constructed:=(a.flag0 and bm_TGtkButton_constructed) shr bp_TGtkButton_constructed;
end;

procedure set_constructed(var a : TGtkButton; __constructed : guint);
begin
   a.flag0:=a.flag0 or ((__constructed shl bp_TGtkButton_constructed) and bm_TGtkButton_constructed);
end;

function in_button(var a : TGtkButton) : guint;
begin
   in_button:=(a.flag0 and bm_TGtkButton_in_button) shr bp_TGtkButton_in_button;
end;

procedure set_in_button(var a : TGtkButton; __in_button : guint);
begin
   a.flag0:=a.flag0 or ((__in_button shl bp_TGtkButton_in_button) and bm_TGtkButton_in_button);
end;

function button_down(var a : TGtkButton) : guint;
begin
   button_down:=(a.flag0 and bm_TGtkButton_button_down) shr bp_TGtkButton_button_down;
end;

procedure set_button_down(var a : TGtkButton; __button_down : guint);
begin
   a.flag0:=a.flag0 or ((__button_down shl bp_TGtkButton_button_down) and bm_TGtkButton_button_down);
end;

function relief(var a : TGtkButton) : guint;
begin
   relief:=(a.flag0 and bm_TGtkButton_relief) shr bp_TGtkButton_relief;
end;

procedure set_relief(var a : TGtkButton; __relief : guint);
begin
   a.flag0:=a.flag0 or ((__relief shl bp_TGtkButton_relief) and bm_TGtkButton_relief);
end;

function use_underline(var a : TGtkButton) : guint;
begin
   use_underline:=(a.flag0 and bm_TGtkButton_use_underline) shr bp_TGtkButton_use_underline;
end;

procedure set_use_underline(var a : TGtkButton; __use_underline : guint);
begin
   a.flag0:=a.flag0 or ((__use_underline shl bp_TGtkButton_use_underline) and bm_TGtkButton_use_underline);
end;

function use_stock(var a : TGtkButton) : guint;
begin
   use_stock:=(a.flag0 and bm_TGtkButton_use_stock) shr bp_TGtkButton_use_stock;
end;

procedure set_use_stock(var a : TGtkButton; __use_stock : guint);
begin
   a.flag0:=a.flag0 or ((__use_stock shl bp_TGtkButton_use_stock) and bm_TGtkButton_use_stock);
end;

function depressed(var a : TGtkButton) : guint;
begin
   depressed:=(a.flag0 and bm_TGtkButton_depressed) shr bp_TGtkButton_depressed;
end;

procedure set_depressed(var a : TGtkButton; __depressed : guint);
begin
   a.flag0:=a.flag0 or ((__depressed shl bp_TGtkButton_depressed) and bm_TGtkButton_depressed);
end;

function depress_on_activate(var a : TGtkButton) : guint;
begin
   depress_on_activate:=(a.flag0 and bm_TGtkButton_depress_on_activate) shr bp_TGtkButton_depress_on_activate;
end;

procedure set_depress_on_activate(var a : TGtkButton; __depress_on_activate : guint);
begin
   a.flag0:=a.flag0 or ((__depress_on_activate shl bp_TGtkButton_depress_on_activate) and bm_TGtkButton_depress_on_activate);
end;

{$ENDIF read_implementation}
// included by gtk2.pas


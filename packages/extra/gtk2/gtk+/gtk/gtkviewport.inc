// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkViewport = ^TGtkViewport;
   TGtkViewport = record
        bin : TGtkBin;
        shadow_type : TGtkShadowType;
        view_window : PGdkWindow;
        bin_window : PGdkWindow;
        hadjustment : PGtkAdjustment;
        vadjustment : PGtkAdjustment;
     end;

   PGtkViewportClass = ^TGtkViewportClass;
   TGtkViewportClass = record
        parent_class : TGtkBinClass;
        set_scroll_adjustments : procedure (viewport:PGtkViewport; hadjustment:PGtkAdjustment; vadjustment:PGtkAdjustment); cdecl;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_VIEWPORT : GType;
function GTK_VIEWPORT(obj: pointer) : PGtkViewport;
function GTK_VIEWPORT_CLASS(klass: pointer) : PGtkViewportClass;
function GTK_IS_VIEWPORT(obj: pointer) : boolean;
function GTK_IS_VIEWPORT_CLASS(klass: pointer) : boolean;
function GTK_VIEWPORT_GET_CLASS(obj: pointer) : PGtkViewportClass;


function gtk_viewport_get_type:TGtkType; cdecl; external gtklib;
function gtk_viewport_new(hadjustment:PGtkAdjustment; vadjustment:PGtkAdjustment):PGtkWidget; cdecl; external gtklib;
function gtk_viewport_get_hadjustment(viewport:PGtkViewport):PGtkAdjustment; cdecl; external gtklib;
function gtk_viewport_get_vadjustment(viewport:PGtkViewport):PGtkAdjustment; cdecl; external gtklib;
procedure gtk_viewport_set_hadjustment(viewport:PGtkViewport; adjustment:PGtkAdjustment); cdecl; external gtklib;
procedure gtk_viewport_set_vadjustment(viewport:PGtkViewport; adjustment:PGtkAdjustment); cdecl; external gtklib;
procedure gtk_viewport_set_shadow_type(viewport:PGtkViewport; _type:TGtkShadowType); cdecl; external gtklib;
function gtk_viewport_get_shadow_type(viewport:PGtkViewport):TGtkShadowType; cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_VIEWPORT : GType;
begin
   GTK_TYPE_VIEWPORT:=gtk_viewport_get_type;
end;

function GTK_VIEWPORT(obj: pointer) : PGtkViewport;
begin
   GTK_VIEWPORT:=PGtkViewport(GTK_CHECK_CAST(obj,GTK_TYPE_VIEWPORT));
end;

function GTK_VIEWPORT_CLASS(klass: pointer) : PGtkViewportClass;
begin
   GTK_VIEWPORT_CLASS:=PGtkViewportClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_VIEWPORT));
end;

function GTK_IS_VIEWPORT(obj: pointer) : boolean;
begin
   GTK_IS_VIEWPORT:=GTK_CHECK_TYPE(obj,GTK_TYPE_VIEWPORT);
end;

function GTK_IS_VIEWPORT_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_VIEWPORT_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_VIEWPORT);
end;

function GTK_VIEWPORT_GET_CLASS(obj: pointer) : PGtkViewportClass;
begin
   GTK_VIEWPORT_GET_CLASS:=PGtkViewportClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_VIEWPORT));
end;

{$ENDIF read_implementation}
// included by gtk2.pas


// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkCellRendererToggle = ^TGtkCellRendererToggle;
   TGtkCellRendererToggle = record
        parent : TGtkCellRenderer;
        flag0 : word;
     end;

{ Padding for future expansion  }
   PGtkCellRendererToggleClass = ^TGtkCellRendererToggleClass;
   TGtkCellRendererToggleClass = record
        parent_class : TGtkCellRendererClass;
        toggled : procedure (cell_renderer_toggle:PGtkCellRendererToggle; path:Pgchar); cdecl;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}

const
   bm_TGtkCellRendererToggle_active = $1;
   bp_TGtkCellRendererToggle_active = 0;
   bm_TGtkCellRendererToggle_activatable = $2;
   bp_TGtkCellRendererToggle_activatable = 1;
   bm_TGtkCellRendererToggle_radio = $4;
   bp_TGtkCellRendererToggle_radio = 2;


function GTK_TYPE_CELL_RENDERER_TOGGLE : GType;
function GTK_CELL_RENDERER_TOGGLE(obj: pointer) : PGtkCellRendererToggle;
function GTK_CELL_RENDERER_TOGGLE_CLASS(klass: pointer) : PGtkCellRendererToggleClass;
function GTK_IS_CELL_RENDERER_TOGGLE(obj: pointer) : boolean;
function GTK_IS_CELL_RENDERER_TOGGLE_CLASS(klass: pointer) : boolean;
function GTK_CELL_RENDERER_TOGGLE_GET_CLASS(obj: pointer) : PGtkCellRendererToggleClass;


function active(var a : TGtkCellRendererToggle) : guint; overload;
procedure set_active(var a : TGtkCellRendererToggle; __active : guint); overload;
function activatable(var a : TGtkCellRendererToggle) : guint;
procedure set_activatable(var a : TGtkCellRendererToggle; __activatable : guint);
function radio(var a : TGtkCellRendererToggle) : guint;
procedure set_radio(var a : TGtkCellRendererToggle; __radio : guint);


function gtk_cell_renderer_toggle_get_type:TGtkType; cdecl; external gtklib;
function gtk_cell_renderer_toggle_new:PGtkCellRenderer; cdecl; external gtklib;
function gtk_cell_renderer_toggle_get_radio(toggle:PGtkCellRendererToggle):gboolean; cdecl; external gtklib;
procedure gtk_cell_renderer_toggle_set_radio(toggle:PGtkCellRendererToggle; radio:gboolean); cdecl; external gtklib;
function gtk_cell_renderer_toggle_get_active(toggle:PGtkCellRendererToggle):gboolean; cdecl; external gtklib;
procedure gtk_cell_renderer_toggle_set_active(toggle:PGtkCellRendererToggle; setting:gboolean); cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_CELL_RENDERER_TOGGLE : GType;
begin
   GTK_TYPE_CELL_RENDERER_TOGGLE:=gtk_cell_renderer_toggle_get_type;
end;

function GTK_CELL_RENDERER_TOGGLE(obj: pointer) : PGtkCellRendererToggle;
begin
   GTK_CELL_RENDERER_TOGGLE:=PGtkCellRendererToggle(GTK_CHECK_CAST(obj,GTK_TYPE_CELL_RENDERER_TOGGLE));
end;

function GTK_CELL_RENDERER_TOGGLE_CLASS(klass: pointer) : PGtkCellRendererToggleClass;
begin
   GTK_CELL_RENDERER_TOGGLE_CLASS:=PGtkCellRendererToggleClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_CELL_RENDERER_TOGGLE));
end;

function GTK_IS_CELL_RENDERER_TOGGLE(obj: pointer) : boolean;
begin
   GTK_IS_CELL_RENDERER_TOGGLE:=GTK_CHECK_TYPE(obj,GTK_TYPE_CELL_RENDERER_TOGGLE);
end;

function GTK_IS_CELL_RENDERER_TOGGLE_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_CELL_RENDERER_TOGGLE_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_CELL_RENDERER_TOGGLE);
end;

function GTK_CELL_RENDERER_TOGGLE_GET_CLASS(obj: pointer) : PGtkCellRendererToggleClass;
begin
   GTK_CELL_RENDERER_TOGGLE_GET_CLASS:=PGtkCellRendererToggleClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_CELL_RENDERER_TOGGLE));
end;

function active(var a : TGtkCellRendererToggle) : guint;
begin
   active:=(a.flag0 and bm_TGtkCellRendererToggle_active) shr bp_TGtkCellRendererToggle_active;
end;

procedure set_active(var a : TGtkCellRendererToggle; __active : guint);
begin
   a.flag0:=a.flag0 or ((__active shl bp_TGtkCellRendererToggle_active) and bm_TGtkCellRendererToggle_active);
end;

function activatable(var a : TGtkCellRendererToggle) : guint;
begin
   activatable:=(a.flag0 and bm_TGtkCellRendererToggle_activatable) shr bp_TGtkCellRendererToggle_activatable;
end;

procedure set_activatable(var a : TGtkCellRendererToggle; __activatable : guint);
begin
   a.flag0:=a.flag0 or ((__activatable shl bp_TGtkCellRendererToggle_activatable) and bm_TGtkCellRendererToggle_activatable);
end;

function radio(var a : TGtkCellRendererToggle) : guint;
begin
   radio:=(a.flag0 and bm_TGtkCellRendererToggle_radio) shr bp_TGtkCellRendererToggle_radio;
end;

procedure set_radio(var a : TGtkCellRendererToggle; __radio : guint);
begin
   a.flag0:=a.flag0 or ((__radio shl bp_TGtkCellRendererToggle_radio) and bm_TGtkCellRendererToggle_radio);
end;

{$ENDIF read_implementation}
// included by gtk2.pas


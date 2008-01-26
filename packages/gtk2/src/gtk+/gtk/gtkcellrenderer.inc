// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
{ this flag means the cell is in the sort column/row  }
   PGtkCellRendererState = ^TGtkCellRendererState;
   TGtkCellRendererState = longint;

   PGtkCellRendererMode = ^TGtkCellRendererMode;
   TGtkCellRendererMode = (
     GTK_CELL_RENDERER_MODE_INERT,
     GTK_CELL_RENDERER_MODE_ACTIVATABLE,
     GTK_CELL_RENDERER_MODE_EDITABLE
   );

   PGtkCellRenderer = ^TGtkCellRenderer;
   TGtkCellRenderer = record
        parent : TGtkObject;
        xalign : gfloat;
        yalign : gfloat;
        width : gint;
        height : gint;
        xpad : guint16;
        ypad : guint16;
        flag0 : word;
     end;

{ vtable - not signals  }
{ Padding for future expansion  }
   PGtkCellRendererClass = ^TGtkCellRendererClass;
   TGtkCellRendererClass = record
        parent_class : TGtkObjectClass;
        get_size : procedure (cell:PGtkCellRenderer; widget:PGtkWidget; cell_area:PGdkRectangle; x_offset:Pgint; y_offset:Pgint;
                      width:Pgint; height:Pgint); cdecl;
        render : procedure (cell:PGtkCellRenderer; window:PGdkWindow; widget:PGtkWidget; background_area:PGdkRectangle; cell_area:PGdkRectangle;
                      expose_area:PGdkRectangle; flags:TGtkCellRendererState); cdecl;
        activate : function (cell:PGtkCellRenderer; event:PGdkEvent; widget:PGtkWidget; path:Pgchar; background_area:PGdkRectangle;
                     cell_area:PGdkRectangle; flags:TGtkCellRendererState):gboolean; cdecl;
        start_editing : function (cell:PGtkCellRenderer; event:PGdkEvent; widget:PGtkWidget; path:Pgchar; background_area:PGdkRectangle;
                     cell_area:PGdkRectangle; flags:TGtkCellRendererState):PGtkCellEditable; cdecl;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}


const
   GTK_CELL_RENDERER_SELECTED = 1 shl 0;
   GTK_CELL_RENDERER_PRELIT = 1 shl 1;
   GTK_CELL_RENDERER_INSENSITIVE = 1 shl 2;
   GTK_CELL_RENDERER_SORTED = 1 shl 3;


const
   bm_TGtkCellRenderer_mode = $3;
   bp_TGtkCellRenderer_mode = 0;
   bm_TGtkCellRenderer_visible = $4;
   bp_TGtkCellRenderer_visible = 2;
   bm_TGtkCellRenderer_is_expander = $8;
   bp_TGtkCellRenderer_is_expander = 3;
   bm_TGtkCellRenderer_is_expanded = $10;
   bp_TGtkCellRenderer_is_expanded = 4;
   bm_TGtkCellRenderer_cell_background_set = $20;
   bp_TGtkCellRenderer_cell_background_set = 5;

function GTK_TYPE_CELL_RENDERER : GType;
function GTK_CELL_RENDERER(obj: pointer) : PGtkCellRenderer;
function GTK_CELL_RENDERER_CLASS(klass: pointer) : PGtkCellRendererClass;
function GTK_IS_CELL_RENDERER(obj: pointer) : boolean;
function GTK_IS_CELL_RENDERER_CLASS(klass: pointer) : boolean;
function GTK_CELL_RENDERER_GET_CLASS(obj: pointer) : PGtkCellRendererClass;


function mode(var a : TGtkCellRenderer) : guint;
procedure set_mode(var a : TGtkCellRenderer; __mode : guint);
function visible(var a : TGtkCellRenderer) : guint; overload;
procedure set_visible(var a : TGtkCellRenderer; __visible : guint); overload;
function is_expander(var a : TGtkCellRenderer) : guint;
procedure set_is_expander(var a : TGtkCellRenderer; __is_expander : guint);
function is_expanded(var a : TGtkCellRenderer) : guint;
procedure set_is_expanded(var a : TGtkCellRenderer; __is_expanded : guint);
function cell_background_set(var a : TGtkCellRenderer) : guint;
procedure set_cell_background_set(var a : TGtkCellRenderer; __cell_background_set : guint);

function gtk_cell_renderer_get_type:GType; cdecl; external gtklib;
procedure gtk_cell_renderer_get_size(cell:PGtkCellRenderer; widget:PGtkWidget; cell_area:PGdkRectangle; x_offset:Pgint; y_offset:Pgint;
            width:Pgint; height:Pgint); cdecl; external gtklib;
procedure gtk_cell_renderer_render(cell:PGtkCellRenderer; window:PGdkWindow; widget:PGtkWidget; background_area:PGdkRectangle; cell_area:PGdkRectangle;
            expose_area:PGdkRectangle; flags:TGtkCellRendererState); cdecl; external gtklib;
function gtk_cell_renderer_activate(cell:PGtkCellRenderer; event:PGdkEvent; widget:PGtkWidget; path:Pgchar; background_area:PGdkRectangle;
           cell_area:PGdkRectangle; flags:TGtkCellRendererState):gboolean; cdecl; external gtklib;
function gtk_cell_renderer_start_editing(cell:PGtkCellRenderer; event:PGdkEvent; widget:PGtkWidget; path:Pgchar; background_area:PGdkRectangle;
           cell_area:PGdkRectangle; flags:TGtkCellRendererState):PGtkCellEditable; cdecl; external gtklib;
procedure gtk_cell_renderer_set_fixed_size(cell:PGtkCellRenderer; width:gint; height:gint); cdecl; external gtklib;
procedure gtk_cell_renderer_get_fixed_size(cell:PGtkCellRenderer; width:Pgint; height:Pgint); cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_CELL_RENDERER : GType;
begin
   GTK_TYPE_CELL_RENDERER:=gtk_cell_renderer_get_type;
end;

function GTK_CELL_RENDERER(obj: pointer) : PGtkCellRenderer;
begin
   GTK_CELL_RENDERER:=PGtkCellRenderer(GTK_CHECK_CAST(obj,GTK_TYPE_CELL_RENDERER));
end;

function GTK_CELL_RENDERER_CLASS(klass: pointer) : PGtkCellRendererClass;
begin
   GTK_CELL_RENDERER_CLASS:=PGtkCellRendererClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_CELL_RENDERER));
end;

function GTK_IS_CELL_RENDERER(obj: pointer) : boolean;
begin
   GTK_IS_CELL_RENDERER:=GTK_CHECK_TYPE(obj,GTK_TYPE_CELL_RENDERER);
end;

function GTK_IS_CELL_RENDERER_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_CELL_RENDERER_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_CELL_RENDERER);
end;

function GTK_CELL_RENDERER_GET_CLASS(obj: pointer) : PGtkCellRendererClass;
begin
   GTK_CELL_RENDERER_GET_CLASS:=PGtkCellRendererClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_CELL_RENDERER));
end;

function mode(var a : TGtkCellRenderer) : guint;
begin
   mode:=(a.flag0 and bm_TGtkCellRenderer_mode) shr bp_TGtkCellRenderer_mode;
end;

procedure set_mode(var a : TGtkCellRenderer; __mode : guint);
begin
   a.flag0:=a.flag0 or ((__mode shl bp_TGtkCellRenderer_mode) and bm_TGtkCellRenderer_mode);
end;

function visible(var a : TGtkCellRenderer) : guint;
begin
   visible:=(a.flag0 and bm_TGtkCellRenderer_visible) shr bp_TGtkCellRenderer_visible;
end;

procedure set_visible(var a : TGtkCellRenderer; __visible : guint);
begin
   a.flag0:=a.flag0 or ((__visible shl bp_TGtkCellRenderer_visible) and bm_TGtkCellRenderer_visible);
end;

function is_expander(var a : TGtkCellRenderer) : guint;
begin
   is_expander:=(a.flag0 and bm_TGtkCellRenderer_is_expander) shr bp_TGtkCellRenderer_is_expander;
end;

procedure set_is_expander(var a : TGtkCellRenderer; __is_expander : guint);
begin
   a.flag0:=a.flag0 or ((__is_expander shl bp_TGtkCellRenderer_is_expander) and bm_TGtkCellRenderer_is_expander);
end;

function is_expanded(var a : TGtkCellRenderer) : guint;
begin
   is_expanded:=(a.flag0 and bm_TGtkCellRenderer_is_expanded) shr bp_TGtkCellRenderer_is_expanded;
end;

procedure set_is_expanded(var a : TGtkCellRenderer; __is_expanded : guint);
begin
   a.flag0:=a.flag0 or ((__is_expanded shl bp_TGtkCellRenderer_is_expanded) and bm_TGtkCellRenderer_is_expanded);
end;

function cell_background_set(var a : TGtkCellRenderer) : guint;
begin
   cell_background_set:=(a.flag0 and bm_TGtkCellRenderer_cell_background_set) shr bp_TGtkCellRenderer_cell_background_set;
end;

procedure set_cell_background_set(var a : TGtkCellRenderer; __cell_background_set : guint);
begin
   a.flag0:=a.flag0 or ((__cell_background_set shl bp_TGtkCellRenderer_cell_background_set) and bm_TGtkCellRenderer_cell_background_set);
end;

{$ENDIF read_implementation}
// included by gtk2.pas


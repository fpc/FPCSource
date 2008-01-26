// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkCellRendererPixbuf = ^TGtkCellRendererPixbuf;
   TGtkCellRendererPixbuf = record
        parent : TGtkCellRenderer;
        pixbuf : PGdkPixbuf;
        pixbuf_expander_open : PGdkPixbuf;
        pixbuf_expander_closed : PGdkPixbuf;
     end;

{ Padding for future expansion  }
   PGtkCellRendererPixbufClass = ^TGtkCellRendererPixbufClass;
   TGtkCellRendererPixbufClass = record
        parent_class : TGtkCellRendererClass;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_CELL_RENDERER_PIXBUF : GType;
function GTK_CELL_RENDERER_PIXBUF(obj: pointer) : PGtkCellRendererPixbuf;
function GTK_CELL_RENDERER_PIXBUF_CLASS(klass: pointer) : PGtkCellRendererPixbufClass;
function GTK_IS_CELL_RENDERER_PIXBUF(obj: pointer) : boolean;
function GTK_IS_CELL_RENDERER_PIXBUF_CLASS(klass: pointer) : boolean;
function GTK_CELL_RENDERER_PIXBUF_GET_CLASS(obj: pointer) : PGtkCellRendererPixbufClass;


function gtk_cell_renderer_pixbuf_get_type:TGtkType; cdecl; external gtklib;
function gtk_cell_renderer_pixbuf_new:PGtkCellRenderer; cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_CELL_RENDERER_PIXBUF : GType;
begin
   GTK_TYPE_CELL_RENDERER_PIXBUF:=gtk_cell_renderer_pixbuf_get_type;
end;

function GTK_CELL_RENDERER_PIXBUF(obj: pointer) : PGtkCellRendererPixbuf;
begin
   GTK_CELL_RENDERER_PIXBUF:=PGtkCellRendererPixbuf(GTK_CHECK_CAST(obj,GTK_TYPE_CELL_RENDERER_PIXBUF));
end;

function GTK_CELL_RENDERER_PIXBUF_CLASS(klass: pointer) : PGtkCellRendererPixbufClass;
begin
   GTK_CELL_RENDERER_PIXBUF_CLASS:=PGtkCellRendererPixbufClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_CELL_RENDERER_PIXBUF));
end;

function GTK_IS_CELL_RENDERER_PIXBUF(obj: pointer) : boolean;
begin
   GTK_IS_CELL_RENDERER_PIXBUF:=GTK_CHECK_TYPE(obj,GTK_TYPE_CELL_RENDERER_PIXBUF);
end;

function GTK_IS_CELL_RENDERER_PIXBUF_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_CELL_RENDERER_PIXBUF_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_CELL_RENDERER_PIXBUF);
end;

function GTK_CELL_RENDERER_PIXBUF_GET_CLASS(obj: pointer) : PGtkCellRendererPixbufClass;
begin
   GTK_CELL_RENDERER_PIXBUF_GET_CLASS:=PGtkCellRendererPixbufClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_CELL_RENDERER_PIXBUF));
end;

{$ENDIF read_implementation}
// included by gtk2.pas


// included by gtk2.pas

{
   NOTE this widget is considered too specialized/little-used for
   GTK+, and will in the future be moved to some other package.  If
   your application needs this widget, feel free to use it, as the
   widget does work and is useful in some applications; it's just not
   of general interest. However, we are not accepting new features for
   the widget, and it will eventually move out of the GTK+
   distribution.
  }

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkHRuler = ^TGtkHRuler;
   TGtkHRuler = record
        ruler : TGtkRuler;
     end;

   PGtkHRulerClass = ^TGtkHRulerClass;
   TGtkHRulerClass = record
        parent_class : TGtkRulerClass;
     end;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_HRULER : GType;
function GTK_HRULER(obj: pointer) : PGtkHRuler;
function GTK_HRULER_CLASS(klass: pointer) : PGtkHRulerClass;
function GTK_IS_HRULER(obj: pointer) : boolean;
function GTK_IS_HRULER_CLASS(klass: pointer) : boolean;
function GTK_HRULER_GET_CLASS(obj: pointer) : PGtkHRulerClass;



function gtk_hruler_get_type:TGtkType; cdecl; external gtklib;
function gtk_hruler_new:PGtkWidget; cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_HRULER : GType;
begin
   GTK_TYPE_HRULER:=gtk_hruler_get_type;
end;

function GTK_HRULER(obj: pointer) : PGtkHRuler;
begin
   GTK_HRULER:=PGtkHRuler(GTK_CHECK_CAST(obj,GTK_TYPE_HRULER));
end;

function GTK_HRULER_CLASS(klass: pointer) : PGtkHRulerClass;
begin
   GTK_HRULER_CLASS:=PGtkHRulerClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_HRULER));
end;

function GTK_IS_HRULER(obj: pointer) : boolean;
begin
   GTK_IS_HRULER:=GTK_CHECK_TYPE(obj,GTK_TYPE_HRULER);
end;

function GTK_IS_HRULER_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_HRULER_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_HRULER);
end;

function GTK_HRULER_GET_CLASS(obj: pointer) : PGtkHRulerClass;
begin
   GTK_HRULER_GET_CLASS:=PGtkHRulerClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_HRULER));
end;

{$ENDIF read_implementation}
// included by gtk2.pas


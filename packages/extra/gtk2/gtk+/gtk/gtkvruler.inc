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
   PGtkVRuler = ^TGtkVRuler;
   TGtkVRuler = record
        ruler : TGtkRuler;
     end;

   PGtkVRulerClass = ^TGtkVRulerClass;
   TGtkVRulerClass = record
        parent_class : TGtkRulerClass;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_VRULER : GType;
function GTK_VRULER(obj: pointer) : PGtkVRuler;
function GTK_VRULER_CLASS(klass: pointer) : PGtkVRulerClass;
function GTK_IS_VRULER(obj: pointer) : boolean;
function GTK_IS_VRULER_CLASS(klass: pointer) : boolean;
function GTK_VRULER_GET_CLASS(obj: pointer) : PGtkVRulerClass;


function gtk_vruler_get_type:TGtkType; cdecl; external gtklib;
function gtk_vruler_new:PGtkWidget; cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_VRULER : GType;
begin
   GTK_TYPE_VRULER:=gtk_vruler_get_type;
end;

function GTK_VRULER(obj: pointer) : PGtkVRuler;
begin
   GTK_VRULER:=PGtkVRuler(GTK_CHECK_CAST(obj,GTK_TYPE_VRULER));
end;

function GTK_VRULER_CLASS(klass: pointer) : PGtkVRulerClass;
begin
   GTK_VRULER_CLASS:=PGtkVRulerClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_VRULER));
end;

function GTK_IS_VRULER(obj: pointer) : boolean;
begin
   GTK_IS_VRULER:=GTK_CHECK_TYPE(obj,GTK_TYPE_VRULER);
end;

function GTK_IS_VRULER_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_VRULER_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_VRULER);
end;

function GTK_VRULER_GET_CLASS(obj: pointer) : PGtkVRulerClass;
begin
   GTK_VRULER_GET_CLASS:=PGtkVRulerClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_VRULER));
end;

{$ENDIF read_implementation}
// included by gtk2.pas


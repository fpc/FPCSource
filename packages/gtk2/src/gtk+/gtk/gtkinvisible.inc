// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkInvisible = ^TGtkInvisible;
   TGtkInvisible = record
        widget : TGtkWidget;
        has_user_ref_count : gboolean;
        screen : PGdkScreen;
     end;

{ Padding for future expansion  }
   PGtkInvisibleClass = ^TGtkInvisibleClass;
   TGtkInvisibleClass = record
        parent_class : TGtkWidgetClass;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_INVISIBLE : GType;
function GTK_INVISIBLE(obj: pointer) : PGtkInvisible;
function GTK_INVISIBLE_CLASS(klass: pointer) : PGtkInvisibleClass;
function GTK_IS_INVISIBLE(obj: pointer) : boolean;
function GTK_IS_INVISIBLE_CLASS(klass: pointer) : boolean;
function GTK_INVISIBLE_GET_CLASS(obj: pointer) : PGtkInvisibleClass;


function gtk_invisible_get_type:TGtkType; cdecl; external gtklib;
function gtk_invisible_new:PGtkWidget; cdecl; external gtklib;

{$IFDEF HasGTK2_2}
function gtk_invisible_get_screen(invisible:PGtkInvisible):PGdkScreen; cdecl; external gtklib;
function gtk_invisible_new_for_screen(screen:PGdkScreen):PGtkWidget; cdecl; external gtklib;
procedure gtk_invisible_set_screen(invisible:PGtkInvisible; screen:PGdkScreen); cdecl; external gtklib;
{$ENDIF}
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_INVISIBLE : GType;
begin
   GTK_TYPE_INVISIBLE:=gtk_invisible_get_type;
end;

function GTK_INVISIBLE(obj: pointer) : PGtkInvisible;
begin
   GTK_INVISIBLE:=PGtkInvisible(GTK_CHECK_CAST(obj,GTK_TYPE_INVISIBLE));
end;

function GTK_INVISIBLE_CLASS(klass: pointer) : PGtkInvisibleClass;
begin
   GTK_INVISIBLE_CLASS:=PGtkInvisibleClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_INVISIBLE));
end;

function GTK_IS_INVISIBLE(obj: pointer) : boolean;
begin
   GTK_IS_INVISIBLE:=GTK_CHECK_TYPE(obj,GTK_TYPE_INVISIBLE);
end;

function GTK_IS_INVISIBLE_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_INVISIBLE_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_INVISIBLE);
end;

function GTK_INVISIBLE_GET_CLASS(obj: pointer) : PGtkInvisibleClass;
begin
   GTK_INVISIBLE_GET_CLASS:=PGtkInvisibleClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_INVISIBLE));
end;

{$ENDIF read_implementation}
// included by gtk2.pas


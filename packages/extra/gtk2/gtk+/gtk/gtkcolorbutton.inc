// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

{$IFDEF read_interface_types}
type
   PGtkColorButtonPrivate = pointer;

   PGtkColorButton = ^TGtkColorButton;
   TGtkColorButton = record
        button : TGtkButton;
        priv : PGtkColorButtonPrivate;
     end;

{ Padding for future expansion  }
   PGtkColorButtonClass = ^TGtkColorButtonClass;
   TGtkColorButtonClass = record
        parent_class : TGtkButtonClass;
        color_set : procedure (cp:PGtkColorButton);cdecl;
        _gtk_reserved1 : procedure ;
        _gtk_reserved2 : procedure ;
        _gtk_reserved3 : procedure ;
        _gtk_reserved4 : procedure ;
     end;

{$ENDIF read_interface_types}

{$IFDEF read_interface_rest}

function GTK_TYPE_COLOR_BUTTON : GType;
function GTK_COLOR_BUTTON(obj : pointer) : PGtkColorButton;
function GTK_COLOR_BUTTON_CLASS(klass : pointer) : PGtkColorButtonClass;
function GTK_IS_COLOR_BUTTON(obj : pointer) : gboolean;
function GTK_IS_COLOR_BUTTON_CLASS(klass : pointer) : gboolean;
function GTK_COLOR_BUTTON_GET_CLASS(obj : pointer) : PGtkColorButtonClass;



function gtk_color_button_get_type:GType;cdecl;external gtklib name 'gtk_color_button_get_type';
function gtk_color_button_new:PGtkWidget;cdecl;external gtklib name 'gtk_color_button_new';

function gtk_color_button_new_with_color(color:PGdkColor):PGtkWidget;cdecl;external gtklib name 'gtk_color_button_new_with_color';

procedure gtk_color_button_set_color(color_button:PGtkColorButton; color:PGdkColor);cdecl;external gtklib name 'gtk_color_button_set_color';
procedure gtk_color_button_set_alpha(color_button:PGtkColorButton; alpha:guint16);cdecl;external gtklib name 'gtk_color_button_set_alpha';
procedure gtk_color_button_get_color(color_button:PGtkColorButton; color:PGdkColor);cdecl;external gtklib name 'gtk_color_button_get_color';
function gtk_color_button_get_alpha(color_button:PGtkColorButton):guint16;cdecl;external gtklib name 'gtk_color_button_get_alpha';

procedure gtk_color_button_set_use_alpha(color_button:PGtkColorButton; use_alpha:gboolean);cdecl;external gtklib name 'gtk_color_button_set_use_alpha';
function gtk_color_button_get_use_alpha(color_button:PGtkColorButton):gboolean;cdecl;external gtklib name 'gtk_color_button_get_use_alpha';


procedure gtk_color_button_set_title(color_button:PGtkColorButton; title:Pgchar);cdecl;external gtklib name 'gtk_color_button_set_title';
function gtk_color_button_get_title(color_button:PGtkColorButton):Pgchar;cdecl;external gtklib name 'gtk_color_button_get_title';

{$endif read_interface_rest}

{$ifdef read_implementation}

function GTK_TYPE_COLOR_BUTTON : GType;
begin
   GTK_TYPE_COLOR_BUTTON:=gtk_color_button_get_type;
end;

function GTK_COLOR_BUTTON(obj : pointer) : PGtkColorButton;
begin
   GTK_COLOR_BUTTON:=PGtkColorButton(G_TYPE_CHECK_INSTANCE_CAST(obj,GTK_TYPE_COLOR_BUTTON));
end;

function GTK_COLOR_BUTTON_CLASS(klass : pointer) : PGtkColorButtonClass;
begin
   GTK_COLOR_BUTTON_CLASS:=PGtkColorButtonClass(G_TYPE_CHECK_CLASS_CAST(klass,GTK_TYPE_COLOR_BUTTON));
end;

function GTK_IS_COLOR_BUTTON(obj : pointer) : gboolean;
begin
   GTK_IS_COLOR_BUTTON:=G_TYPE_CHECK_INSTANCE_TYPE(obj,GTK_TYPE_COLOR_BUTTON);
end;

function GTK_IS_COLOR_BUTTON_CLASS(klass : pointer) : gboolean;
begin
   GTK_IS_COLOR_BUTTON_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,GTK_TYPE_COLOR_BUTTON);
end;

function GTK_COLOR_BUTTON_GET_CLASS(obj : pointer) : PGtkColorButtonClass;
begin
   GTK_COLOR_BUTTON_GET_CLASS:=PGtkColorButtonClass(G_TYPE_INSTANCE_GET_CLASS(obj,GTK_TYPE_COLOR_BUTTON));
end;

{$endif read_implementation}


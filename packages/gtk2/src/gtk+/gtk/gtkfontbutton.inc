
{ GtkFontButton is a button widget that allow user to select a font. }

// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

{$IFDEF read_interface_types}
type
   PGtkFontButtonPrivate = pointer;

   PGtkFontButton = ^TGtkFontButton;
   TGtkFontButton = record
        button : TGtkButton;
        priv : PGtkFontButtonPrivate;
     end;

{ font_set signal is emitted when font is chosen  }
{ Padding for future expansion  }
   PGtkFontButtonClass = ^TGtkFontButtonClass;
   TGtkFontButtonClass = record
        parent_class : TGtkButtonClass;
        font_set : procedure (gfp:PGtkFontButton);cdecl;
        _gtk_reserved1 : procedure ;
        _gtk_reserved2 : procedure ;
        _gtk_reserved3 : procedure ;
        _gtk_reserved4 : procedure ;
     end;

{$ENDIF read_interface_types}

{$IFDEF read_interface_rest}

function GTK_TYPE_FONT_BUTTON : GType;
function GTK_FONT_BUTTON(obj : pointer) : PGtkFontButton;
function GTK_FONT_BUTTON_CLASS(klass : pointer) : PGtkFontButtonClass;
function GTK_IS_FONT_BUTTON(obj : pointer) : gboolean;
function GTK_IS_FONT_BUTTON_CLASS(klass : pointer) : gboolean;
function GTK_FONT_BUTTON_GET_CLASS(obj : pointer) : PGtkFontButtonClass;

function gtk_font_button_get_type:GType;cdecl;external gtklib name 'gtk_font_button_get_type';

function gtk_font_button_new:PGtkWidget;cdecl;external gtklib name 'gtk_font_button_new';
function gtk_font_button_new_with_font(fontname:Pgchar):PGtkWidget;cdecl;external gtklib name 'gtk_font_button_new_with_font';

function gtk_font_button_get_title(font_button:PGtkFontButton):Pgchar;cdecl;external gtklib name 'gtk_font_button_get_title';
procedure gtk_font_button_set_title(font_button:PGtkFontButton; title:Pgchar);cdecl;external gtklib name 'gtk_font_button_set_title';

function gtk_font_button_get_use_font(font_button:PGtkFontButton):gboolean;cdecl;external gtklib name 'gtk_font_button_get_use_font';
procedure gtk_font_button_set_use_font(font_button:PGtkFontButton; use_font:gboolean);cdecl;external gtklib name 'gtk_font_button_set_use_font';

function gtk_font_button_get_use_size(font_button:PGtkFontButton):gboolean;cdecl;external gtklib name 'gtk_font_button_get_use_size';
procedure gtk_font_button_set_use_size(font_button:PGtkFontButton; use_size:gboolean);cdecl;external gtklib name 'gtk_font_button_set_use_size';

function gtk_font_button_get_font_name(font_button:PGtkFontButton):Pgchar;cdecl;external gtklib name 'gtk_font_button_get_font_name';
function gtk_font_button_set_font_name(font_button:PGtkFontButton; fontname:Pgchar):gboolean;cdecl;external gtklib name 'gtk_font_button_set_font_name';

function gtk_font_button_get_show_style(font_button:PGtkFontButton):gboolean;cdecl;external gtklib name 'gtk_font_button_get_show_style';
procedure gtk_font_button_set_show_style(font_button:PGtkFontButton; show_style:gboolean);cdecl;external gtklib name 'gtk_font_button_set_show_style';

function gtk_font_button_get_show_size(font_button:PGtkFontButton):gboolean;cdecl;external gtklib name 'gtk_font_button_get_show_size';
procedure gtk_font_button_set_show_size(font_button:PGtkFontButton; show_size:gboolean);cdecl;external gtklib name 'gtk_font_button_set_show_size';

{$endif read_interface_rest}

{$ifdef read_implementation}

function GTK_TYPE_FONT_BUTTON : GType;
begin
   GTK_TYPE_FONT_BUTTON:=gtk_font_button_get_type;
end;

function GTK_FONT_BUTTON(obj : pointer) : PGtkFontButton;
begin
   GTK_FONT_BUTTON:=PGtkFontButton(G_TYPE_CHECK_INSTANCE_CAST(obj,GTK_TYPE_FONT_BUTTON));
end;

function GTK_FONT_BUTTON_CLASS(klass : pointer) : PGtkFontButtonClass;
begin
   GTK_FONT_BUTTON_CLASS:=PGtkFontButtonClass(G_TYPE_CHECK_CLASS_CAST(klass,GTK_TYPE_FONT_BUTTON));
end;

function GTK_IS_FONT_BUTTON(obj : pointer) : gboolean;
begin
   GTK_IS_FONT_BUTTON:=G_TYPE_CHECK_INSTANCE_TYPE(obj,GTK_TYPE_FONT_BUTTON);
end;

function GTK_IS_FONT_BUTTON_CLASS(klass : pointer) : gboolean;
begin
   GTK_IS_FONT_BUTTON_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,GTK_TYPE_FONT_BUTTON);
end;

function GTK_FONT_BUTTON_GET_CLASS(obj : pointer) : PGtkFontButtonClass;
begin
   GTK_FONT_BUTTON_GET_CLASS:=PGtkFontButtonClass(G_TYPE_INSTANCE_GET_CLASS(obj,GTK_TYPE_FONT_BUTTON));
end;

{$endif read_implementation}

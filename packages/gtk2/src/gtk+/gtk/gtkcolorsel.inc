// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   TGtkColorSelectionChangePaletteFunc = procedure (colors:PGdkColor; n_colors:gint); cdecl;

   TGtkColorSelectionChangePaletteWithScreenFunc = procedure (screen:PGdkScreen; colors:PGdkColor; n_colors:gint); cdecl;
{ < private_data >  }
   PGtkColorSelection = ^TGtkColorSelection;
   TGtkColorSelection = record
        parent_instance : TGtkVBox;
        private_data : gpointer;
     end;

{ Padding for future expansion  }
   PGtkColorSelectionClass = ^TGtkColorSelectionClass;
   TGtkColorSelectionClass = record
        parent_class : TGtkVBoxClass;
        color_changed : procedure (color_selection:PGtkColorSelection); cdecl;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_COLOR_SELECTION : GType;
function GTK_COLOR_SELECTION(obj: pointer) : PGtkColorSelection;
function GTK_COLOR_SELECTION_CLASS(klass: pointer) : PGtkColorSelectionClass;
function GTK_IS_COLOR_SELECTION(obj: pointer) : boolean;
function GTK_IS_COLOR_SELECTION_CLASS(klass: pointer) : boolean;
function GTK_COLOR_SELECTION_GET_CLASS(obj: pointer) : PGtkColorSelectionClass;


{ ColorSelection  }

function gtk_color_selection_get_type:TGtkType; cdecl; external gtklib;
function gtk_color_selection_new:PGtkWidget; cdecl; external gtklib;
function gtk_color_selection_get_has_opacity_control(colorsel:PGtkColorSelection):gboolean; cdecl; external gtklib;
procedure gtk_color_selection_set_has_opacity_control(colorsel:PGtkColorSelection; has_opacity:gboolean); cdecl; external gtklib;
function gtk_color_selection_get_has_palette(colorsel:PGtkColorSelection):gboolean; cdecl; external gtklib;
procedure gtk_color_selection_set_has_palette(colorsel:PGtkColorSelection; has_palette:gboolean); cdecl; external gtklib;
procedure gtk_color_selection_set_current_color(colorsel:PGtkColorSelection; color:PGdkColor); cdecl; external gtklib;
procedure gtk_color_selection_set_current_alpha(colorsel:PGtkColorSelection; alpha:guint16); cdecl; external gtklib;
procedure gtk_color_selection_get_current_color(colorsel:PGtkColorSelection; color:PGdkColor); cdecl; external gtklib;
function gtk_color_selection_get_current_alpha(colorsel:PGtkColorSelection):guint16; cdecl; external gtklib;
procedure gtk_color_selection_set_previous_color(colorsel:PGtkColorSelection; color:PGdkColor); cdecl; external gtklib;
procedure gtk_color_selection_set_previous_alpha(colorsel:PGtkColorSelection; alpha:guint16); cdecl; external gtklib;
procedure gtk_color_selection_get_previous_color(colorsel:PGtkColorSelection; color:PGdkColor); cdecl; external gtklib;
function gtk_color_selection_get_previous_alpha(colorsel:PGtkColorSelection):guint16; cdecl; external gtklib;
function gtk_color_selection_is_adjusting(colorsel:PGtkColorSelection):gboolean; cdecl; external gtklib;
function gtk_color_selection_palette_from_string(str:Pgchar; var colors:PGdkColor; n_colors:Pgint):gboolean; cdecl; external gtklib;
function gtk_color_selection_palette_to_string(colors:PGdkColor; n_colors:gint):Pgchar; cdecl; external gtklib;

{$ifndef GDK_MULTIHEAD_SAFE}
function gtk_color_selection_set_change_palette_hook(func:TGtkColorSelectionChangePaletteFunc):TGtkColorSelectionChangePaletteFunc; cdecl; external gtklib;
{$endif}

{$ifndef GTK_DISABLE_DEPRECATED}
{ Deprecated calls:  }
procedure gtk_color_selection_set_color(colorsel:PGtkColorSelection; color:Pgdouble); cdecl; external gtklib;
procedure gtk_color_selection_get_color(colorsel:PGtkColorSelection; color:Pgdouble); cdecl; external gtklib;
procedure gtk_color_selection_set_update_policy(colorsel:PGtkColorSelection; policy:TGtkUpdateType); cdecl; external gtklib;
{$endif}
{ GTK_DISABLE_DEPRECATED  }

{$IFDEF HasGTK2_2}
function gtk_color_selection_set_change_palette_with_screen_hook(func:TGtkColorSelectionChangePaletteWithScreenFunc):TGtkColorSelectionChangePaletteWithScreenFunc; cdecl; external gtklib;
{$ENDIF}

{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_COLOR_SELECTION : GType;
begin
   GTK_TYPE_COLOR_SELECTION:=gtk_color_selection_get_type;
end;

function GTK_COLOR_SELECTION(obj: pointer) : PGtkColorSelection;
begin
   GTK_COLOR_SELECTION:=PGtkColorSelection(GTK_CHECK_CAST(obj,GTK_TYPE_COLOR_SELECTION));
end;

function GTK_COLOR_SELECTION_CLASS(klass: pointer) : PGtkColorSelectionClass;
begin
   GTK_COLOR_SELECTION_CLASS:=PGtkColorSelectionClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_COLOR_SELECTION));
end;

function GTK_IS_COLOR_SELECTION(obj: pointer) : boolean;
begin
   GTK_IS_COLOR_SELECTION:=GTK_CHECK_TYPE(obj,GTK_TYPE_COLOR_SELECTION);
end;

function GTK_IS_COLOR_SELECTION_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_COLOR_SELECTION_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_COLOR_SELECTION);
end;

function GTK_COLOR_SELECTION_GET_CLASS(obj: pointer) : PGtkColorSelectionClass;
begin
   GTK_COLOR_SELECTION_GET_CLASS:=PGtkColorSelectionClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_COLOR_SELECTION));
end;

{$ENDIF read_implementation}
// included by gtk2.pas


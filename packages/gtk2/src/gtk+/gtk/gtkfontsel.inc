// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
{ Current family  }
{ Current face  }
{ Cache for gdk_font_selection_get_font, so we can preserve
                                   refcounting behavior
                                  }
   PGtkFontSelection = ^TGtkFontSelection;
   TGtkFontSelection = record
        parent_instance : TGtkVBox;
        font_entry : PGtkWidget;
        family_list : PGtkWidget;
        font_style_entry : PGtkWidget;
        face_list : PGtkWidget;
        size_entry : PGtkWidget;
        size_list : PGtkWidget;
        pixels_button : PGtkWidget;
        points_button : PGtkWidget;
        filter_button : PGtkWidget;
        preview_entry : PGtkWidget;
        family : PPangoFontFamily;
        face : PPangoFontFace;
        size : gint;
        font : PGdkFont;
     end;

{ Padding for future expansion  }
   PGtkFontSelectionClass = ^TGtkFontSelectionClass;
   TGtkFontSelectionClass = record
        parent_class : TGtkVBoxClass;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;

{ The 'Apply' button is not shown by default but you can show/hide it.  }
{ If the user changes the width of the dialog, we turn auto-shrink off.  }
   PGtkFontSelectionDialog = ^TGtkFontSelectionDialog;
   TGtkFontSelectionDialog = record
        parent_instance : TGtkDialog;
        fontsel : PGtkWidget;
        main_vbox : PGtkWidget;
        action_area : PGtkWidget;
        ok_button : PGtkWidget;
        apply_button : PGtkWidget;
        cancel_button : PGtkWidget;
        dialog_width : gint;
        auto_resize : gboolean;
     end;

{ Padding for future expansion  }
   PGtkFontSelectionDialogClass = ^TGtkFontSelectionDialogClass;
   TGtkFontSelectionDialogClass = record
        parent_class : TGtkDialogClass;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_FONT_SELECTION : GType;
function GTK_FONT_SELECTION(obj: pointer) : PGtkFontSelection;
function GTK_FONT_SELECTION_CLASS(klass: pointer) : PGtkFontSelectionClass;
function GTK_IS_FONT_SELECTION(obj: pointer) : boolean;
function GTK_IS_FONT_SELECTION_CLASS(klass: pointer) : boolean;
function GTK_FONT_SELECTION_GET_CLASS(obj: pointer) : PGtkFontSelectionClass;

function GTK_TYPE_FONT_SELECTION_DIALOG : GType;
function GTK_FONT_SELECTION_DIALOG(obj: pointer) : PGtkFontSelectionDialog;
function GTK_FONT_SELECTION_DIALOG_CLASS(klass: pointer) : PGtkFontSelectionDialogClass;
function GTK_IS_FONT_SELECTION_DIALOG(obj: pointer) : boolean;
function GTK_IS_FONT_SELECTION_DIALOG_CLASS(klass: pointer) : boolean;
function GTK_FONT_SELECTION_DIALOG_GET_CLASS(obj: pointer) : PGtkFontSelectionDialogClass;


{
   GtkFontSelection functions.
     see the comments in the GtkFontSelectionDialog functions.
                                                                              }

function gtk_font_selection_get_type:TGtkType; cdecl; external gtklib;
function gtk_font_selection_new:PGtkWidget; cdecl; external gtklib;
function gtk_font_selection_get_font_name(fontsel:PGtkFontSelection):Pgchar; cdecl; external gtklib;

{$ifndef GTK_DISABLE_DEPRECATED}
function gtk_font_selection_get_font(fontsel:PGtkFontSelection):PGdkFont; cdecl; external gtklib;
{$endif}
{ GTK_DISABLE_DEPRECATED  }

function gtk_font_selection_set_font_name(fontsel:PGtkFontSelection; fontname:Pgchar):gboolean; cdecl; external gtklib;
function gtk_font_selection_get_preview_text(fontsel:PGtkFontSelection):Pgchar; cdecl; external gtklib;
procedure gtk_font_selection_set_preview_text(fontsel:PGtkFontSelection; text:Pgchar); cdecl; external gtklib;
{
   GtkFontSelectionDialog functions.
     most of these functions simply call the corresponding function in the
     GtkFontSelection.
                                                                              }
function gtk_font_selection_dialog_get_type:TGtkType; cdecl; external gtklib;
function gtk_font_selection_dialog_new(title:Pgchar):PGtkWidget; cdecl; external gtklib;
{ This returns the X Logical Font Description fontname, or NULL if no font
   is selected. Note that there is a slight possibility that the font might not
   have been loaded OK. You should call gtk_font_selection_dialog_get_font()
   to see if it has been loaded OK.
   You should g_free() the returned font name after you're done with it.  }
function gtk_font_selection_dialog_get_font_name(fsd:PGtkFontSelectionDialog):Pgchar; cdecl; external gtklib;

{$ifndef GTK_DISABLE_DEPRECATED}
{ This will return the current GdkFont, or NULL if none is selected or there
   was a problem loading it. Remember to use gdk_font_ref/unref() if you want
   to use the font (in a style, for example).  }

function gtk_font_selection_dialog_get_font(fsd:PGtkFontSelectionDialog):PGdkFont; cdecl; external gtklib;
{$endif}
{ GTK_DISABLE_DEPRECATED  }


{ This sets the currently displayed font. It should be a valid X Logical
   Font Description font name (anything else will be ignored), e.g.
   "-adobe-courier-bold-o-normal--25- - - - - - - "
   It returns TRUE on success.  }
function gtk_font_selection_dialog_set_font_name(fsd:PGtkFontSelectionDialog; fontname:Pgchar):gboolean; cdecl; external gtklib;
{ This returns the text in the preview entry. You should copy the returned
   text if you need it.  }
function gtk_font_selection_dialog_get_preview_text(fsd:PGtkFontSelectionDialog):Pgchar; cdecl; external gtklib;
{ This sets the text in the preview entry. It will be copied by the entry,
   so there's no need to g_strdup() it first.  }
procedure gtk_font_selection_dialog_set_preview_text(fsd:PGtkFontSelectionDialog; text:Pgchar); cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_FONT_SELECTION : GType;
begin
   GTK_TYPE_FONT_SELECTION:=gtk_font_selection_get_type;
end;

function GTK_FONT_SELECTION(obj: pointer) : PGtkFontSelection;
begin
   GTK_FONT_SELECTION:=PGtkFontSelection(GTK_CHECK_CAST(obj,GTK_TYPE_FONT_SELECTION));
end;

function GTK_FONT_SELECTION_CLASS(klass: pointer) : PGtkFontSelectionClass;
begin
   GTK_FONT_SELECTION_CLASS:=PGtkFontSelectionClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_FONT_SELECTION));
end;

function GTK_IS_FONT_SELECTION(obj: pointer) : boolean;
begin
   GTK_IS_FONT_SELECTION:=GTK_CHECK_TYPE(obj,GTK_TYPE_FONT_SELECTION);
end;

function GTK_IS_FONT_SELECTION_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_FONT_SELECTION_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_FONT_SELECTION);
end;

function GTK_FONT_SELECTION_GET_CLASS(obj: pointer) : PGtkFontSelectionClass;
begin
   GTK_FONT_SELECTION_GET_CLASS:=PGtkFontSelectionClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_FONT_SELECTION));
end;

function GTK_TYPE_FONT_SELECTION_DIALOG : GType;
begin
   GTK_TYPE_FONT_SELECTION_DIALOG:=gtk_font_selection_dialog_get_type;
end;

function GTK_FONT_SELECTION_DIALOG(obj: pointer) : PGtkFontSelectionDialog;
begin
   GTK_FONT_SELECTION_DIALOG:=PGtkFontSelectionDialog(GTK_CHECK_CAST(obj,GTK_TYPE_FONT_SELECTION_DIALOG));
end;

function GTK_FONT_SELECTION_DIALOG_CLASS(klass: pointer) : PGtkFontSelectionDialogClass;
begin
   GTK_FONT_SELECTION_DIALOG_CLASS:=PGtkFontSelectionDialogClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_FONT_SELECTION_DIALOG));
end;

function GTK_IS_FONT_SELECTION_DIALOG(obj: pointer) : boolean;
begin
   GTK_IS_FONT_SELECTION_DIALOG:=GTK_CHECK_TYPE(obj,GTK_TYPE_FONT_SELECTION_DIALOG);
end;

function GTK_IS_FONT_SELECTION_DIALOG_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_FONT_SELECTION_DIALOG_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_FONT_SELECTION_DIALOG);
end;

function GTK_FONT_SELECTION_DIALOG_GET_CLASS(obj: pointer) : PGtkFontSelectionDialogClass;
begin
   GTK_FONT_SELECTION_DIALOG_GET_CLASS:=PGtkFontSelectionDialogClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_FONT_SELECTION_DIALOG));
end;

{$ENDIF read_implementation}
// included by gtk2.pas


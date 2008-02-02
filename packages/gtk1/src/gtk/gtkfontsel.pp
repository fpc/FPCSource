{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  const
     GTK_NUM_FONT_PROPERTIES = 6;
     GTK_NUM_STYLE_PROPERTIES = 5;
     GTK_NUM_FONT_FILTERS = 2;

  type
     TGtkFontMetricType = (GTK_FONT_METRIC_PIXELS,GTK_FONT_METRIC_POINTS);

     TGtkFontType = (GTK_FONT_BITMAP := 1 shl 0,GTK_FONT_SCALABLE := 1 shl 1,
       GTK_FONT_SCALABLE_BITMAP := 1 shl 2,GTK_FONT_ALL := $07);

     TGtkFontFilterType = (GTK_FONT_FILTER_BASE,GTK_FONT_FILTER_USER);

     PGtkFontFilter = ^TGtkFontFilter;
     TGtkFontFilter = record
          font_type : gint;
          property_filters : array[0..(GTK_NUM_FONT_PROPERTIES)-1] of Pguint16;
          property_nfilters : array[0..(GTK_NUM_FONT_PROPERTIES)-1] of guint16;
       end;

     PGtkFontSelection = ^TGtkFontSelection;
     TGtkFontSelection = record
          notebook : TGtkNotebook;
          main_vbox : PGtkWidget;
          font_label : PGtkWidget;
          font_entry : PGtkWidget;
          font_clist : PGtkWidget;
          font_style_entry : PGtkWidget;
          font_style_clist : PGtkWidget;
          size_entry : PGtkWidget;
          size_clist : PGtkWidget;
          pixels_button : PGtkWidget;
          points_button : PGtkWidget;
          filter_button : PGtkWidget;
          preview_entry : PGtkWidget;
          message_label : PGtkWidget;
          info_vbox : PGtkWidget;
          info_clist : PGtkWidget;
          requested_font_name : PGtkWidget;
          actual_font_name : PGtkWidget;
          filter_vbox : PGtkWidget;
          type_bitmaps_button : PGtkWidget;
          type_scalable_button : PGtkWidget;
          type_scaled_bitmaps_button : PGtkWidget;
          filter_clists : array[0..(GTK_NUM_FONT_PROPERTIES)-1] of PGtkWidget;
          font : PGdkFont;
          font_index : gint;
          style : gint;
          metric : TGtkFontMetricType;
          size : gint;
          selected_size : gint;
          property_values : array[0..(GTK_NUM_STYLE_PROPERTIES)-1] of guint16;
          filters : array[0..(GTK_NUM_FONT_FILTERS)-1] of TGtkFontFilter;
       end;

     PGtkFontSelectionClass = ^TGtkFontSelectionClass;
     TGtkFontSelectionClass = record
          parent_class : TGtkNotebookClass;
       end;

     PGtkFontSelectionDialog = ^TGtkFontSelectionDialog;
     TGtkFontSelectionDialog = record
          window : TGtkWindow;
          fontsel : PGtkWidget;
          main_vbox : PGtkWidget;
          action_area : PGtkWidget;
          ok_button : PGtkWidget;
          apply_button : PGtkWidget;
          cancel_button : PGtkWidget;
          dialog_width : gint;
          auto_resize : gboolean;
       end;

     PGtkFontSelectionDialogClass = ^TGtkFontSelectionDialogClass;
     TGtkFontSelectionDialogClass = record
          parent_class : TGtkWindowClass;
       end;

type
  GTK_FONT_SELECTION=PGtkFontSelection;
  GTK_FONT_SELECTION_CLASS=PGtkFontSelectionClass;

function  GTK_FONT_SELECTION_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_font_selection_get_type';
function  GTK_IS_FONT_SELECTION(obj:pointer):boolean;
function  GTK_IS_FONT_SELECTION_CLASS(klass:pointer):boolean;

function  gtk_font_selection_get_type:TGtkType;cdecl;external gtkdll name 'gtk_font_selection_get_type';
function  gtk_font_selection_new:PGtkWidget;cdecl;external gtkdll name 'gtk_font_selection_new';
function  gtk_font_selection_get_font_name(fontsel:PGtkFontSelection):Pgchar;cdecl;external gtkdll name 'gtk_font_selection_get_font_name';
function  gtk_font_selection_get_font(fontsel:PGtkFontSelection):PGdkFont;cdecl;external gtkdll name 'gtk_font_selection_get_font';
function  gtk_font_selection_set_font_name(fontsel:PGtkFontSelection; fontname:Pgchar):gboolean;cdecl;external gtkdll name 'gtk_font_selection_set_font_name';
procedure gtk_font_selection_set_filter(fontsel:PGtkFontSelection; filter_type:TGtkFontFilterType; font_type:TGtkFontType; foundries:PPgchar; weights:PPgchar; slants:PPgchar; setwidths:PPgchar; spacings:PPgchar; charsets:PPgchar);cdecl;external gtkdll name 'gtk_font_selection_set_filter';
function  gtk_font_selection_get_preview_text(fontsel:PGtkFontSelection):Pgchar;cdecl;external gtkdll name 'gtk_font_selection_get_preview_text';
procedure gtk_font_selection_set_preview_text(fontsel:PGtkFontSelection; text:Pgchar);cdecl;external gtkdll name 'gtk_font_selection_set_preview_text';

type
  GTK_FONT_SELECTION_DIALOG=PGtkFontSelectionDialog;
  GTK_FONT_SELECTION_DIALOG_CLASS=PGtkFontSelectionDialogClass;

function  GTK_FONT_SELECTION_DIALOG_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_font_selection_dialog_get_type';
function  GTK_IS_FONT_SELECTION_DIALOG(obj:pointer):boolean;
function  GTK_IS_FONT_SELECTION_DIALOG_CLASS(klass:pointer):boolean;

function  gtk_font_selection_dialog_get_type:TGtkType;cdecl;external gtkdll name 'gtk_font_selection_dialog_get_type';
function  gtk_font_selection_dialog_new(title:Pgchar):PGtkWidget;cdecl;external gtkdll name 'gtk_font_selection_dialog_new';
function  gtk_font_selection_dialog_get_font_name(fsd:PGtkFontSelectionDialog):Pgchar;cdecl;external gtkdll name 'gtk_font_selection_dialog_get_font_name';
function  gtk_font_selection_dialog_get_font(fsd:PGtkFontSelectionDialog):PGdkFont;cdecl;external gtkdll name 'gtk_font_selection_dialog_get_font';
function  gtk_font_selection_dialog_set_font_name(fsd:PGtkFontSelectionDialog; fontname:Pgchar):gboolean;cdecl;external gtkdll name 'gtk_font_selection_dialog_set_font_name';
procedure gtk_font_selection_dialog_set_filter(fsd:PGtkFontSelectionDialog; filter_type:TGtkFontFilterType; font_type:TGtkFontType; foundries:PPgchar; weights:PPgchar; slants:PPgchar; setwidths:PPgchar; spacings:PPgchar; charsets:PPgchar);cdecl;external gtkdll name 'gtk_font_selection_dialog_set_filter';
function  gtk_font_selection_dialog_get_preview_text(fsd:PGtkFontSelectionDialog):Pgchar;cdecl;external gtkdll name 'gtk_font_selection_dialog_get_preview_text';
procedure gtk_font_selection_dialog_set_preview_text(fsd:PGtkFontSelectionDialog; text:Pgchar);cdecl;external gtkdll name 'gtk_font_selection_dialog_set_preview_text';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_FONT_SELECTION(obj:pointer):boolean;
begin
  GTK_IS_FONT_SELECTION:=(obj<>nil) and GTK_IS_FONT_SELECTION_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_FONT_SELECTION_CLASS(klass:pointer):boolean;
begin
  GTK_IS_FONT_SELECTION_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_FONT_SELECTION_TYPE);
end;

function  GTK_IS_FONT_SELECTION_DIALOG(obj:pointer):boolean;
begin
  GTK_IS_FONT_SELECTION_DIALOG:=(obj<>nil) and GTK_IS_FONT_SELECTION_DIALOG_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_FONT_SELECTION_DIALOG_CLASS(klass:pointer):boolean;
begin
  GTK_IS_FONT_SELECTION_DIALOG_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_FONT_SELECTION_DIALOG_TYPE);
end;

{$endif read_implementation}



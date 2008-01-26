{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

    type
       PGtkColorSelection = ^TGtkColorSelection;
       TGtkColorSelection = record
            vbox : TGtkVBox;
            wheel_area : PGtkWidget;
            value_area : PGtkWidget;
            sample_area : PGtkWidget;
            sample_area_eb : PGtkWidget;
            scales : array[0..7] of PGtkWidget;
            entries : array[0..7] of PGtkWidget;
            opacity_label : PGtkWidget;
            wheel_gc : PGdkGC;
            value_gc : PGdkGC;
            sample_gc : PGdkGC;
            policy : TGtkUpdateType;
            use_opacity : gint;
            timer_active : gint;
            timer_tag : gint;
            values : array[0..7] of gdouble;
            old_values : array[0..7] of gdouble;
            wheel_buf : Pguchar;
            value_buf : Pguchar;
            sample_buf : Pguchar;
         end;

       PGtkColorSelectionClass = ^TGtkColorSelectionClass;
       TGtkColorSelectionClass = record
            parent_class : TGtkVBoxClass;
            color_changed : procedure (colorsel:PGtkColorSelection); cdecl;
         end;

       PGtkColorSelectionDialog = ^TGtkColorSelectionDialog;
       TGtkColorSelectionDialog = record
            window : TGtkWindow;
            colorsel : PGtkWidget;
            main_vbox : PGtkWidget;
            ok_button : PGtkWidget;
            reset_button : PGtkWidget;
            cancel_button : PGtkWidget;
            help_button : PGtkWidget;
         end;

       PGtkColorSelectionDialogClass = ^TGtkColorSelectionDialogClass;
       TGtkColorSelectionDialogClass = record
            parent_class : TGtkWindowClass;
         end;

Type
  GTK_COLOR_SELECTION=PGtkColorSelection;
  GTK_COLOR_SELECTION_CLASS=PGtkColorSelectionClass;

  GTK_COLOR_SELECTION_DIALOG=PGtkColorSelectionDialog;
  GTK_COLOR_SELECTION_DIALOG_CLASS=PGtkColorSelectionDialogClass;

{ ColorSelection }
function  GTK_COLOR_SELECTION_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_color_selection_get_type';
function  GTK_IS_COLOR_SELECTION(obj:pointer):boolean;
function  GTK_IS_COLOR_SELECTION_CLASS(klass:pointer):boolean;

function  gtk_color_selection_get_type:TGtkType;cdecl;external gtkdll name 'gtk_color_selection_get_type';
function  gtk_color_selection_new:PGtkWidget;cdecl;external gtkdll name 'gtk_color_selection_new';
procedure gtk_color_selection_set_update_policy(colorsel:PGtkColorSelection; policy:TGtkUpdateType);cdecl;external gtkdll name 'gtk_color_selection_set_update_policy';
procedure gtk_color_selection_set_opacity(colorsel:PGtkColorSelection; use_opacity:gint);cdecl;external gtkdll name 'gtk_color_selection_set_opacity';
procedure gtk_color_selection_set_color(colorsel:PGtkColorSelection; color:Pgdouble);cdecl;external gtkdll name 'gtk_color_selection_set_color';
procedure gtk_color_selection_get_color(colorsel:PGtkColorSelection; color:Pgdouble);cdecl;external gtkdll name 'gtk_color_selection_get_color';

{ ColorSelectionDialog }
function  gtk_color_selection_dialog_get_type:guint;cdecl;external gtkdll name 'gtk_color_selection_dialog_get_type';
function  gtk_color_selection_dialog_new (title:Pgchar):PGtkWidget;cdecl;external gtkdll name 'gtk_color_selection_dialog_new';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_COLOR_SELECTION(obj:pointer):boolean;
begin
  GTK_IS_COLOR_SELECTION:=(obj<>nil) and GTK_IS_COLOR_SELECTION_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_COLOR_SELECTION_CLASS(klass:pointer):boolean;
begin
  GTK_IS_COLOR_SELECTION_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_COLOR_SELECTION_TYPE);
end;

{$endif read_implementation}



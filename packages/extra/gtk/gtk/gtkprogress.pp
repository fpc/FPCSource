{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     PGtkProgress = ^TGtkProgress;
     TGtkProgress = record
          widget : TGtkWidget;
          adjustment : PGtkAdjustment;
          offscreen_pixmap : PGdkPixmap;
          format : Pgchar;
          x_align : gfloat;
          y_align : gfloat;
          flag0 : {$ifdef win32}longint{$else}word{$endif};
       end;

  const
     bm_TGtkProgress_show_text = $1;
     bp_TGtkProgress_show_text = 0;
     bm_TGtkProgress_activity_mode = $2;
     bp_TGtkProgress_activity_mode = 1;
function  show_text(var a : TGtkProgress) : guint;
procedure set_show_text(var a : TGtkProgress; __show_text : guint);
function  activity_mode(var a : TGtkProgress) : guint;
procedure set_activity_mode(var a : TGtkProgress; __activity_mode : guint);

  type
     PGtkProgressClass = ^TGtkProgressClass;
     TGtkProgressClass = record
          parent_class : TGtkWidgetClass;
          paint : procedure (progress:PGtkProgress);cdecl;
          update : procedure (progress:PGtkProgress);cdecl;
          act_mode_enter : procedure (progress:PGtkProgress);cdecl;
       end;

type
  GTK_PROGRESS=PGtkProgress;
  GTK_PROGRESS_CLASS=PGtkProgressClass;

function  GTK_PROGRESS_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_progress_get_type';
function  GTK_IS_PROGRESS(obj:pointer):boolean;
function  GTK_IS_PROGRESS_CLASS(klass:pointer):boolean;

function  gtk_progress_get_type:TGtkType;cdecl;external gtkdll name 'gtk_progress_get_type';
procedure gtk_progress_set_show_text(progress:PGtkProgress; show_text:gint);cdecl;external gtkdll name 'gtk_progress_set_show_text';
procedure gtk_progress_set_text_alignment(progress:PGtkProgress; x_align:gfloat; y_align:gfloat);cdecl;external gtkdll name 'gtk_progress_set_text_alignment';
procedure gtk_progress_set_format_string(progress:PGtkProgress; format:Pgchar);cdecl;external gtkdll name 'gtk_progress_set_format_string';
procedure gtk_progress_set_adjustment(progress:PGtkProgress; adjustment:PGtkAdjustment);cdecl;external gtkdll name 'gtk_progress_set_adjustment';
procedure gtk_progress_configure(progress:PGtkProgress; value:gfloat; min:gfloat; max:gfloat);cdecl;external gtkdll name 'gtk_progress_configure';
procedure gtk_progress_set_percentage(progress:PGtkProgress; percentage:gfloat);cdecl;external gtkdll name 'gtk_progress_set_percentage';
procedure gtk_progress_set_value(progress:PGtkProgress; value:gfloat);cdecl;external gtkdll name 'gtk_progress_set_value';
function  gtk_progress_get_value(progress:PGtkProgress):gfloat;cdecl;external gtkdll name 'gtk_progress_get_value';
procedure gtk_progress_set_activity_mode(progress:PGtkProgress; activity_mode:guint);cdecl;external gtkdll name 'gtk_progress_set_activity_mode';
function  gtk_progress_get_current_text(progress:PGtkProgress):Pgchar;cdecl;external gtkdll name 'gtk_progress_get_current_text';
function  gtk_progress_get_text_from_value(progress:PGtkProgress; value:gfloat):Pgchar;cdecl;external gtkdll name 'gtk_progress_get_text_from_value';
function  gtk_progress_get_current_percentage(progress:PGtkProgress):gfloat;cdecl;external gtkdll name 'gtk_progress_get_current_percentage';
function  gtk_progress_get_percentage_from_value(progress:PGtkProgress; value:gfloat):gfloat;cdecl;external gtkdll name 'gtk_progress_get_percentage_from_value';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  show_text(var a : TGtkProgress) : guint;
    begin
       show_text:=(a.flag0 and bm_TGtkProgress_show_text) shr bp_TGtkProgress_show_text;
    end;

procedure set_show_text(var a : TGtkProgress; __show_text : guint);
    begin
       a.flag0:=a.flag0 or ((__show_text shl bp_TGtkProgress_show_text) and bm_TGtkProgress_show_text);
    end;

function  activity_mode(var a : TGtkProgress) : guint;
    begin
       activity_mode:=(a.flag0 and bm_TGtkProgress_activity_mode) shr bp_TGtkProgress_activity_mode;
    end;

procedure set_activity_mode(var a : TGtkProgress; __activity_mode : guint);
    begin
       a.flag0:=a.flag0 or ((__activity_mode shl bp_TGtkProgress_activity_mode) and bm_TGtkProgress_activity_mode);
    end;

function  GTK_IS_PROGRESS(obj:pointer):boolean;
begin
  GTK_IS_PROGRESS:=(obj<>nil) and GTK_IS_PROGRESS_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_PROGRESS_CLASS(klass:pointer):boolean;
begin
  GTK_IS_PROGRESS_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_PROGRESS_TYPE);
end;

{$endif read_implementation}



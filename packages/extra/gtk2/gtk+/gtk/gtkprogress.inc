// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkProgress = ^TGtkProgress;
   TGtkProgress = record
        widget : TGtkWidget;
        adjustment : PGtkAdjustment;
        offscreen_pixmap : PGdkPixmap;
        format : Pgchar;
        x_align : gfloat;
        y_align : gfloat;
        flag0 : word;
     end;

{ Padding for future expansion  }
   PGtkProgressClass = ^TGtkProgressClass;
   TGtkProgressClass = record
        parent_class : TGtkWidgetClass;
        paint : procedure (progress:PGtkProgress); cdecl;
        update : procedure (progress:PGtkProgress); cdecl;
        act_mode_enter : procedure (progress:PGtkProgress); cdecl;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
   bm_TGtkProgress_show_text = $1;
   bp_TGtkProgress_show_text = 0;
   bm_TGtkProgress_activity_mode = $2;
   bp_TGtkProgress_activity_mode = 1;
   bm_TGtkProgress_use_text_format = $4;
   bp_TGtkProgress_use_text_format = 2;


{$ifndef GTK_DISABLE_DEPRECATED}
function GTK_TYPE_PROGRESS : GType;
function GTK_PROGRESS(obj: pointer) : PGtkProgress;
function GTK_PROGRESS_CLASS(klass: pointer) : PGtkProgressClass;
function GTK_IS_PROGRESS(obj: pointer) : boolean;
function GTK_IS_PROGRESS_CLASS(klass: pointer) : boolean;
function GTK_PROGRESS_GET_CLASS(obj: pointer) : PGtkProgressClass;

{$endif}
{ GTK_DISABLE_DEPRECATED  }
function show_text(var a : TGtkProgress) : guint;
procedure set_show_text(var a : TGtkProgress; __show_text : guint);
function activity_mode(var a : TGtkProgress) : guint;
procedure set_activity_mode(var a : TGtkProgress; __activity_mode : guint);
function use_text_format(var a : TGtkProgress) : guint;
procedure set_use_text_format(var a : TGtkProgress; __use_text_format : guint);

{ This entire interface is deprecated. Use GtkProgressBar
   directly.
  }
{$ifndef GTK_DISABLE_DEPRECATED}

function gtk_progress_get_type:TGtkType; cdecl; external gtklib;
procedure gtk_progress_set_show_text(progress:PGtkProgress; show_text:gboolean); cdecl; external gtklib;
procedure gtk_progress_set_text_alignment(progress:PGtkProgress; x_align:gfloat; y_align:gfloat); cdecl; external gtklib;
procedure gtk_progress_set_format_string(progress:PGtkProgress; format:Pgchar); cdecl; external gtklib;
procedure gtk_progress_set_adjustment(progress:PGtkProgress; adjustment:PGtkAdjustment); cdecl; external gtklib;
procedure gtk_progress_configure(progress:PGtkProgress; value:gdouble; min:gdouble; max:gdouble); cdecl; external gtklib;
procedure gtk_progress_set_percentage(progress:PGtkProgress; percentage:gdouble); cdecl; external gtklib;
procedure gtk_progress_set_value(progress:PGtkProgress; value:gdouble); cdecl; external gtklib;
function gtk_progress_get_value(progress:PGtkProgress):gdouble; cdecl; external gtklib;
procedure gtk_progress_set_activity_mode(progress:PGtkProgress; activity_mode:gboolean); cdecl; external gtklib;
function gtk_progress_get_current_text(progress:PGtkProgress):Pgchar; cdecl; external gtklib;
function gtk_progress_get_text_from_value(progress:PGtkProgress; value:gdouble):Pgchar; cdecl; external gtklib;
function gtk_progress_get_current_percentage(progress:PGtkProgress):gdouble; cdecl; external gtklib;
function gtk_progress_get_percentage_from_value(progress:PGtkProgress; value:gdouble):gdouble; cdecl; external gtklib;
{$endif}
{ GTK_DISABLE_DEPRECATED  }
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_PROGRESS : GType;
begin
   GTK_TYPE_PROGRESS:=gtk_progress_get_type;
end;

function GTK_PROGRESS(obj: pointer) : PGtkProgress;
begin
   GTK_PROGRESS:=PGtkProgress(GTK_CHECK_CAST(obj,GTK_TYPE_PROGRESS));
end;

function GTK_PROGRESS_CLASS(klass: pointer) : PGtkProgressClass;
begin
   GTK_PROGRESS_CLASS:=PGtkProgressClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_PROGRESS));
end;

function GTK_IS_PROGRESS(obj: pointer) : boolean;
begin
   GTK_IS_PROGRESS:=GTK_CHECK_TYPE(obj,GTK_TYPE_PROGRESS);
end;

function GTK_IS_PROGRESS_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_PROGRESS_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_PROGRESS);
end;

function GTK_PROGRESS_GET_CLASS(obj: pointer) : PGtkProgressClass;
begin
   GTK_PROGRESS_GET_CLASS:=PGtkProgressClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_PROGRESS));
end;

function show_text(var a : TGtkProgress) : guint;
begin
   show_text:=(a.flag0 and bm_TGtkProgress_show_text) shr bp_TGtkProgress_show_text;
end;

procedure set_show_text(var a : TGtkProgress; __show_text : guint);
begin
   a.flag0:=a.flag0 or ((__show_text shl bp_TGtkProgress_show_text) and bm_TGtkProgress_show_text);
end;

function activity_mode(var a : TGtkProgress) : guint;
begin
   activity_mode:=(a.flag0 and bm_TGtkProgress_activity_mode) shr bp_TGtkProgress_activity_mode;
end;

procedure set_activity_mode(var a : TGtkProgress; __activity_mode : guint);
begin
   a.flag0:=a.flag0 or ((__activity_mode shl bp_TGtkProgress_activity_mode) and bm_TGtkProgress_activity_mode);
end;

function use_text_format(var a : TGtkProgress) : guint;
begin
   use_text_format:=(a.flag0 and bm_TGtkProgress_use_text_format) shr bp_TGtkProgress_use_text_format;
end;

procedure set_use_text_format(var a : TGtkProgress; __use_text_format : guint);
begin
   a.flag0:=a.flag0 or ((__use_text_format shl bp_TGtkProgress_use_text_format) and bm_TGtkProgress_use_text_format);
end;

{$ENDIF read_implementation}
// included by gtk2.pas


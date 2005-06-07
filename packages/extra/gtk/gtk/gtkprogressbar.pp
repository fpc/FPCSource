{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     TGtkProgressBarStyle = (GTK_PROGRESS_CONTINUOUS,GTK_PROGRESS_DISCRETE
       );

     TGtkProgressBarOrientation = (GTK_PROGRESS_LEFT_TO_RIGHT,GTK_PROGRESS_RIGHT_TO_LEFT,
       GTK_PROGRESS_BOTTOM_TO_TOP,GTK_PROGRESS_TOP_TO_BOTTOM
       );

     PGtkProgressBar = ^TGtkProgressBar;
     TGtkProgressBar = record
          progress : TGtkProgress;
          bar_style : TGtkProgressBarStyle;
          orientation : TGtkProgressBarOrientation;
          blocks : guint;
          in_block : gint;
          activity_pos : gint;
          activity_step : guint;
          activity_blocks : guint;
          flag0 : {$ifdef win32}longint{$else}word{$endif};
       end;

  const
     bm_TGtkProgressBar_activity_dir = $1;
     bp_TGtkProgressBar_activity_dir = 0;
function  activity_dir(var a : TGtkProgressBar) : guint;
procedure set_activity_dir(var a : TGtkProgressBar; __activity_dir : guint);

  type
     PGtkProgressBarClass = ^TGtkProgressBarClass;
     TGtkProgressBarClass = record
          parent_class : TGtkProgressClass;
       end;

Type
  GTK_PROGRESS_BAR=PGtkProgressBar;
  GTK_PROGRESS_BAR_CLASS=PGtkProgressBarClass;

function  GTK_PROGRESS_BAR_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_progress_bar_get_type';
function  GTK_IS_PROGRESS_BAR(obj:pointer):boolean;
function  GTK_IS_PROGRESS_BAR_CLASS(klass:pointer):boolean;

function  gtk_progress_bar_get_type:TGtkType;cdecl;external gtkdll name 'gtk_progress_bar_get_type';
function  gtk_progress_bar_new:PGtkWidget;cdecl;external gtkdll name 'gtk_progress_bar_new';
function  gtk_progress_bar_new_with_adjustment(adjustment:PGtkAdjustment):PGtkWidget;cdecl;external gtkdll name 'gtk_progress_bar_new_with_adjustment';
procedure gtk_progress_bar_set_bar_style(pbar:PGtkProgressBar; style:TGtkProgressBarStyle);cdecl;external gtkdll name 'gtk_progress_bar_set_bar_style';
procedure gtk_progress_bar_set_discrete_blocks(pbar:PGtkProgressBar; blocks:guint);cdecl;external gtkdll name 'gtk_progress_bar_set_discrete_blocks';
procedure gtk_progress_bar_set_activity_step(pbar:PGtkProgressBar; step:guint);cdecl;external gtkdll name 'gtk_progress_bar_set_activity_step';
procedure gtk_progress_bar_set_activity_blocks(pbar:PGtkProgressBar; blocks:guint);cdecl;external gtkdll name 'gtk_progress_bar_set_activity_blocks';
procedure gtk_progress_bar_set_orientation(pbar:PGtkProgressBar; orientation:TGtkProgressBarOrientation);cdecl;external gtkdll name 'gtk_progress_bar_set_orientation';
procedure gtk_progress_bar_update(pbar:PGtkProgressBar; percentage:gfloat);cdecl;external gtkdll name 'gtk_progress_bar_update';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  activity_dir(var a : TGtkProgressBar) : guint;
    begin
       activity_dir:=(a.flag0 and bm_TGtkProgressBar_activity_dir) shr bp_TGtkProgressBar_activity_dir;
    end;

procedure set_activity_dir(var a : TGtkProgressBar; __activity_dir : guint);
    begin
       a.flag0:=a.flag0 or ((__activity_dir shl bp_TGtkProgressBar_activity_dir) and bm_TGtkProgressBar_activity_dir);
    end;

function  GTK_IS_PROGRESS_BAR(obj:pointer):boolean;
begin
  GTK_IS_PROGRESS_BAR:=(obj<>nil) and GTK_IS_PROGRESS_BAR_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_PROGRESS_BAR_CLASS(klass:pointer):boolean;
begin
  GTK_IS_PROGRESS_BAR_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_PROGRESS_BAR_TYPE);
end;

{$endif read_implementation}



{
   $Id$
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


{
  $Log$
  Revision 1.1.2.1  2000-09-09 18:42:53  peter
    * gtk win32 fixes

  Revision 1.1  2000/07/13 06:34:05  michael
  + Initial import

  Revision 1.1  1999/11/24 23:36:36  peter
    * moved to packages dir

  Revision 1.10  1999/10/06 17:42:50  peter
    * external is now only in the interface
    * removed gtk 1.0 support

  Revision 1.9  1999/07/23 16:12:57  peter
    * use packrecords C

  Revision 1.8  1999/05/11 00:39:13  peter
    * win32 fixes

  Revision 1.7  1999/05/10 15:20:10  peter
    * cdecl fixes

  Revision 1.6  1999/05/10 09:03:40  peter
    * gtk 1.2 port working

  Revision 1.5  1998/11/24 12:59:03  peter
    * fixed uses clause (from mailinglist)

  Revision 1.4  1998/11/09 10:10:22  peter
    + C type casts are now correctly handled

  Revision 1.3  1998/10/21 20:23:01  peter
    * cdecl, packrecord fixes (from the gtk.tar.gz)
    * win32 support
    * gtk.pp,gdk.pp for an all in one unit

}


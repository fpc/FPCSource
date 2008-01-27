{$IfDef read_interface}

type
   TGnomeAnimatorStatus = (GNOME_ANIMATOR_STATUS_STOPPED,GNOME_ANIMATOR_STATUS_RUNNING);

   TGnomeAnimatorLoopType = (GNOME_ANIMATOR_LOOP_NONE,GNOME_ANIMATOR_LOOP_RESTART,
     GNOME_ANIMATOR_LOOP_PING_PONG);

   PGnomeAnimatorPrivate = ^TGnomeAnimatorPrivate;
   TGnomeAnimatorPrivate = record
   end;

   PGnomeAnimatorFrame = ^TGnomeAnimatorFrame;
   TGnomeAnimatorFrame = record
   end;

   PGnomeAnimator = ^TGnomeAnimator;
   TGnomeAnimator = record
        widget : PGtkWidget;
        num_frames : guint;
        current_frame_number : guint;
        status : TGnomeAnimatorStatus;
        loop_type : TGnomeAnimatorLoopType;
        playback_direction : gint;
        playback_speed : gdouble;
        privat : PGnomeAnimatorPrivate;
     end;
   GNOME_ANIMATOR = ^TGnomeAnimator;

   PGnomeAnimatorClass = ^TGnomeAnimatorClass;
   TGnomeAnimatorClass = record
        parent_class : TGtkWidgetClass;
     end;
   GNOME_ANIMATOR_CLASS = ^TGnomeAnimatorClass;

function GNOME_TYPE_ANIMATOR : TGTKType;
function GNOME_IS_ANIMATOR(obj : Pointer) : Boolean;
function GNOME_IS_ANIMATOR_CLASS(klass : Pointer) : Boolean;

function gnome_animator_get_type:TGTKType;cdecl;external libgnomeuidll name 'gnome_animator_get_type';
function gnome_animator_new_with_size(width:guint; height:guint):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_animator_new_with_size';
procedure gnome_animator_set_loop_type(animator:PGnomeAnimator; loop_type:TGnomeAnimatorLoopType);cdecl;external libgnomeuidll name 'gnome_animator_set_loop_type';
function gnome_animator_get_loop_type(animator:PGnomeAnimator):TGnomeAnimatorLoopType;cdecl;external libgnomeuidll name 'gnome_animator_get_loop_type';
procedure gnome_animator_set_playback_direction(animator:PGnomeAnimator; playback_direction:gint);cdecl;external libgnomeuidll name 'gnome_animator_set_playback_direction';
function gnome_animator_get_playback_direction(animator:PGnomeAnimator):gint;cdecl;external libgnomeuidll name 'gnome_animator_get_playback_direction';
function gnome_animator_append_frame_from_imlib_at_size(animator:PGnomeAnimator; image:PGdkImlibImage; x_offset:gint; y_offset:gint; interval:guint32;
           width:guint; height:guint):gboolean;cdecl;external libgnomeuidll name 'gnome_animator_append_frame_from_imlib_at_size';
function gnome_animator_append_frame_from_imlib(animator:PGnomeAnimator; image:PGdkImlibImage; x_offset:gint; y_offset:gint; interval:guint32):gboolean;cdecl;external libgnomeuidll name 'gnome_animator_append_frame_from_imlib';
function gnome_animator_append_frame_from_file_at_size(animator:PGnomeAnimator; name:Pgchar; x_offset:gint; y_offset:gint; interval:guint32;
           width:guint; height:guint):gboolean;cdecl;external libgnomeuidll name 'gnome_animator_append_frame_from_file_at_size';
function gnome_animator_append_frame_from_file(animator:PGnomeAnimator; name:Pgchar; x_offset:gint; y_offset:gint; interval:guint32):gboolean;cdecl;external libgnomeuidll name 'gnome_animator_append_frame_from_file';
function gnome_animator_append_frames_from_imlib_at_size(animator:PGnomeAnimator; image:PGdkImlibImage; x_offset:gint; y_offset:gint; interval:guint32;
           x_unit:gint; width:guint; height:guint):gboolean;cdecl;external libgnomeuidll name 'gnome_animator_append_frames_from_imlib_at_size';
function gnome_animator_append_frames_from_imlib(animator:PGnomeAnimator; image:PGdkImlibImage; x_offset:gint; y_offset:gint; interval:guint32;
           x_unit:gint):gboolean;cdecl;external libgnomeuidll name 'gnome_animator_append_frames_from_imlib';
function gnome_animator_append_frames_from_file_at_size(animator:PGnomeAnimator; name:Pgchar; x_offset:gint; y_offset:gint; interval:guint32;
           x_unit:gint; width:guint; height:guint):gboolean;cdecl;external libgnomeuidll name 'gnome_animator_append_frames_from_file_at_size';
function gnome_animator_append_frames_from_file(animator:PGnomeAnimator; name:Pgchar; x_offset:gint; y_offset:gint; interval:guint32;
           x_unit:gint):gboolean;cdecl;external libgnomeuidll name 'gnome_animator_append_frames_from_file';
function gnome_animator_append_frame_from_gnome_pixmap(animator:PGnomeAnimator; pixmap:PGnomePixmap; x_offset:gint; y_offset:gint; interval:guint32):gboolean;cdecl;external libgnomeuidll name 'gnome_animator_append_frame_from_gnome_pixmap';
procedure gnome_animator_start(animator:PGnomeAnimator);cdecl;external libgnomeuidll name 'gnome_animator_start';
procedure gnome_animator_stop(animator:PGnomeAnimator);cdecl;external libgnomeuidll name 'gnome_animator_stop';
function gnome_animator_advance(animator:PGnomeAnimator; num:gint):gboolean;cdecl;external libgnomeuidll name 'gnome_animator_advance';
procedure gnome_animator_goto_frame(animator:PGnomeAnimator; frame_number:guint);cdecl;external libgnomeuidll name 'gnome_animator_goto_frame';
function gnome_animator_get_current_frame_number(animator:PGnomeAnimator):guint;cdecl;external libgnomeuidll name 'gnome_animator_get_current_frame_number';
function gnome_animator_get_status(animator:PGnomeAnimator):TGnomeAnimatorStatus;cdecl;external libgnomeuidll name 'gnome_animator_get_status';
procedure gnome_animator_set_playback_speed(animator:PGnomeAnimator; speed:gdouble);cdecl;external libgnomeuidll name 'gnome_animator_set_playback_speed';
function gnome_animator_get_playback_speed(animator:PGnomeAnimator):gdouble;cdecl;external libgnomeuidll name 'gnome_animator_get_playback_speed';

{$EndIf read_interface}

{$Ifdef read_implementation}

function GNOME_TYPE_ANIMATOR : TGTKType;
begin
  GNOME_TYPE_ANIMATOR:=gnome_animator_get_type;
end;

function GNOME_IS_ANIMATOR(obj : Pointer) : Boolean;
begin
   GNOME_IS_ANIMATOR:=(obj<>nil) and GNOME_IS_ANIMATOR_CLASS(PGtkTypeObject(obj)^.klass);
end;

function GNOME_IS_ANIMATOR_CLASS(klass : Pointer) : Boolean;
begin
   GNOME_IS_ANIMATOR_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GNOME_TYPE_ANIMATOR);
end;

{$Endif read_implementation}

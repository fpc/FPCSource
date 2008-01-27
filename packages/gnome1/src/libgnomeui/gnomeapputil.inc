{$IfDef read_interface}

function gnome_app_message(app:PGnomeApp; message:Pgchar):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_app_message';
procedure gnome_app_flash(app:PGnomeApp; flash:Pgchar);cdecl;external libgnomeuidll name 'gnome_app_flash';
function gnome_app_error(app:PGnomeApp; error:Pgchar):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_app_error';
function gnome_app_warning(app:PGnomeApp; warning:Pgchar):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_app_warning';
function gnome_app_question(app:PGnomeApp; question:Pgchar; callback:TGnomeReplyCallback; data:gpointer):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_app_question';
function gnome_app_question_modal(app:PGnomeApp; question:Pgchar; callback:TGnomeReplyCallback; data:gpointer):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_app_question_modal';
function gnome_app_ok_cancel(app:PGnomeApp; message:Pgchar; callback:TGnomeReplyCallback; data:gpointer):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_app_ok_cancel';
function gnome_app_ok_cancel_modal(app:PGnomeApp; message:Pgchar; callback:TGnomeReplyCallback; data:gpointer):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_app_ok_cancel_modal';
function gnome_app_request_string(app:PGnomeApp; prompt:Pgchar; callback:TGnomeStringCallback; data:gpointer):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_app_request_string';
function gnome_app_request_password(app:PGnomeApp; prompt:Pgchar; callback:TGnomeStringCallback; data:gpointer):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_app_request_password';

type
   TGnomeAppProgressFunc = function (data:gpointer):gdouble;cdecl;
   TGnomeAppProgressCancelFunc = procedure (data:gpointer);cdecl;
   TGnomeAppProgressKey = gpointer;

function gnome_app_progress_timeout(app:PGnomeApp; description:Pgchar; interval:guint32; percentage_cb:TGnomeAppProgressFunc; cancel_cb:TGnomeAppProgressCancelFunc;
           data:gpointer):TGnomeAppProgressKey;cdecl;external libgnomeuidll name 'gnome_app_progress_timeout';
function gnome_app_progress_manual(app:PGnomeApp; description:Pgchar; cancel_cb:TGnomeAppProgressCancelFunc; data:gpointer):TGnomeAppProgressKey;cdecl;external libgnomeuidll name 'gnome_app_progress_manual';
procedure gnome_app_set_progress(key:TGnomeAppProgressKey; percent:gdouble);cdecl;external libgnomeuidll name 'gnome_app_set_progress';
procedure gnome_app_progress_done(key:TGnomeAppProgressKey);cdecl;external libgnomeuidll name 'gnome_app_progress_done';

{$EndIf read_interface}

{$Ifdef read_implementation}

{$Endif read_implementation}

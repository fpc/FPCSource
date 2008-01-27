{$IfDef read_interface}

function gnome_ok_dialog(message:Pgchar):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_ok_dialog';
function gnome_ok_dialog_parented(message:Pgchar; parent:PGtkWindow):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_ok_dialog_parented';
function gnome_error_dialog(error:Pgchar):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_error_dialog';
function gnome_error_dialog_parented(error:Pgchar; parent:PGtkWindow):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_error_dialog_parented';
function gnome_warning_dialog(warning:Pgchar):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_warning_dialog';
function gnome_warning_dialog_parented(warning:Pgchar; parent:PGtkWindow):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_warning_dialog_parented';
function gnome_question_dialog(question:Pgchar; callback:TGnomeReplyCallback; data:gpointer):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_question_dialog';
function gnome_question_dialog_parented(question:Pgchar; callback:TGnomeReplyCallback; data:gpointer; parent:PGtkWindow):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_question_dialog_parented';
function gnome_question_dialog_modal(question:Pgchar; callback:TGnomeReplyCallback; data:gpointer):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_question_dialog_modal';
function gnome_question_dialog_modal_parented(question:Pgchar; callback:TGnomeReplyCallback; data:gpointer; parent:PGtkWindow):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_question_dialog_modal_parented';
function gnome_ok_cancel_dialog(message:Pgchar; callback:TGnomeReplyCallback; data:gpointer):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_ok_cancel_dialog';
function gnome_ok_cancel_dialog_parented(message:Pgchar; callback:TGnomeReplyCallback; data:gpointer; parent:PGtkWindow):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_ok_cancel_dialog_parented';
function gnome_ok_cancel_dialog_modal(message:Pgchar; callback:TGnomeReplyCallback; data:gpointer):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_ok_cancel_dialog_modal';
function gnome_ok_cancel_dialog_modal_parented(message:Pgchar; callback:TGnomeReplyCallback; data:gpointer; parent:PGtkWindow):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_ok_cancel_dialog_modal_parented';
function gnome_request_dialog(password:gboolean; prompt:Pgchar; default_text:Pgchar; max_length:guint16; callback:TGnomeStringCallback;
           data:gpointer; parent:PGtkWindow):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_request_dialog';

{$EndIf read_interface}

{$Ifdef read_implementation}

{$Endif read_implementation}

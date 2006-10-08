{$IfDef read_interface}
type
  PGnomeDialog = ^TGnomeDialog;

  TGnomeDialog = record
      window : TGtkWindow;
      vbox : PGtkWidget;
      action_area : PGtkWidget;
      buttons : PGList;
      accelerators : PGtkAccelGroup;
      flag0 : word;
      padding : gpointer;
   end;
  GNOME_DIALOG = PGnomeDialog;

const
   bm_TGnomeDialog_click_closes = $1;
   bp_TGnomeDialog_click_closes = 0;
   bm_TGnomeDialog_just_hide = $2;
   bp_TGnomeDialog_just_hide = 1;

function click_closes(var a : TGnomeDialog) : Boolean;
procedure set_click_closes(var a : TGnomeDialog; __click_closes : Boolean);
function just_hide(var a : TGnomeDialog) : Boolean;
procedure set_just_hide(var a : TGnomeDialog; __just_hide : Boolean);

type
   PGnomeDialogClass = ^TGnomeDialogClass;
   TGnomeDialogClass = record
        parent_class : TGtkWindowClass;
        clicked : procedure (dialog:PGnomeDialog; button_number:gint);cdecl;
        close : function (dialog:PGnomeDialog):gboolean; cdecl;
    end;
    GNOME_DIALOG_CLASS = PGnomeDialogClass;

function GNOME_TYPE_DIALOG : TGTKType;
function GNOME_IS_DIALOG(obj : Pointer) : Boolean;
function GNOME_IS_DIALOG_CLASS(klass : Pointer) : Boolean;

function gnome_dialog_get_type:TGtkType;cdecl;external libgnomeuidll name 'gnome_dialog_get_type';
function gnome_dialog_new(title:Pgchar; args:array of const):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_dialog_new';
function gnome_dialog_new(title:Pgchar):PGtkWidget;cdecl; varargs; external libgnomeuidll name 'gnome_dialog_new';
function gnome_dialog_newv(title:Pgchar; buttons:PPgchar):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_dialog_newv';
procedure gnome_dialog_set_parent(dialog:PGnomeDialog; parent:PGtkWindow);cdecl;external libgnomeuidll name 'gnome_dialog_set_parent';
procedure gnome_dialog_button_connect(dialog:PGnomeDialog; button:gint; callback:TGtkSignalFunc; data:gpointer);cdecl;external libgnomeuidll name 'gnome_dialog_button_connect';
procedure gnome_dialog_button_connect_object(dialog:PGnomeDialog; button:gint; callback:TGtkSignalFunc; obj:PGtkObject);cdecl;external libgnomeuidll name 'gnome_dialog_button_connect_object';
function gnome_dialog_run(dialog:PGnomeDialog):gint;cdecl;external libgnomeuidll name 'gnome_dialog_run';
function gnome_dialog_run_and_close(dialog:PGnomeDialog):gint;cdecl;external libgnomeuidll name 'gnome_dialog_run_and_close';
procedure gnome_dialog_set_default(dialog:PGnomeDialog; button:gint);cdecl;external libgnomeuidll name 'gnome_dialog_set_default';
procedure gnome_dialog_grab_focus(dialog:PGnomeDialog; button:gint);cdecl;external libgnomeuidll name 'gnome_dialog_grab_focus';
procedure gnome_dialog_set_sensitive(dialog:PGnomeDialog; button:gint; setting:gboolean);cdecl;external libgnomeuidll name 'gnome_dialog_set_sensitive';
procedure gnome_dialog_set_accelerator(dialog:PGnomeDialog; button:gint; accelerator_key:guchar; accelerator_mods:guint8);cdecl;external libgnomeuidll name 'gnome_dialog_set_accelerator';
procedure gnome_dialog_close(dialog:PGnomeDialog);cdecl;external libgnomeuidll name 'gnome_dialog_close';
procedure gnome_dialog_close_hides(dialog:PGnomeDialog; just_hide:gboolean);cdecl;external libgnomeuidll name 'gnome_dialog_close_hides';
procedure gnome_dialog_set_close(dialog:PGnomeDialog; click_closes:gboolean);cdecl;external libgnomeuidll name 'gnome_dialog_set_close';
procedure gnome_dialog_editable_enters(dialog:PGnomeDialog; editable:PGtkEditable);cdecl;external libgnomeuidll name 'gnome_dialog_editable_enters';
procedure gnome_dialog_append_buttons(dialog:PGnomeDialog; first:Pgchar; args:array of const);cdecl;external libgnomeuidll name 'gnome_dialog_append_buttons';
procedure gnome_dialog_append_buttons(dialog:PGnomeDialog; first:Pgchar);cdecl; varargs; external libgnomeuidll name 'gnome_dialog_append_buttons';
procedure gnome_dialog_append_button(dialog:PGnomeDialog; name:Pgchar);cdecl;external libgnomeuidll name 'gnome_dialog_append_button';
procedure gnome_dialog_append_buttonsv(dialog:PGnomeDialog; buttons:PPgchar);cdecl;external libgnomeuidll name 'gnome_dialog_append_buttonsv';
procedure gnome_dialog_append_button_with_pixmap(dialog:PGnomeDialog; name:Pgchar; pixmap:Pgchar);cdecl;external libgnomeuidll name 'gnome_dialog_append_button_with_pixmap';
procedure gnome_dialog_append_buttons_with_pixmaps(dialog:PGnomeDialog; names:PPgchar; pixmaps:PPgchar);cdecl;external libgnomeuidll name 'gnome_dialog_append_buttons_with_pixmaps';
procedure gnome_dialog_construct(dialog:PGnomeDialog; title:Pgchar; ap:va_list);cdecl;external libgnomeuidll name 'gnome_dialog_construct';
procedure gnome_dialog_constructv(dialog:PGnomeDialog; title:Pgchar; buttons:PPgchar);cdecl;external libgnomeuidll name 'gnome_dialog_constructv';

{$EndIf read_interface}

{$Ifdef read_implementation}

function GNOME_TYPE_DIALOG : TGTKType;
begin
  GNOME_TYPE_DIALOG:=gnome_dialog_get_type;
end;

function GNOME_IS_DIALOG(obj : Pointer) : Boolean;
begin
   GNOME_IS_DIALOG:=(obj<>nil) and GNOME_IS_DIALOG_CLASS(PGtkTypeObject(obj)^.klass);
end;

function GNOME_IS_DIALOG_CLASS(klass : Pointer) : Boolean;
begin
   GNOME_IS_DIALOG_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GNOME_TYPE_DIALOG);
end;

function click_closes(var a : TGnomeDialog) : Boolean;
begin
   click_closes:=Boolean((a.flag0 and bm_TGnomeDialog_click_closes) shr bp_TGnomeDialog_click_closes);
end;

procedure set_click_closes(var a : TGnomeDialog; __click_closes : Boolean);
begin
   a.flag0:=a.flag0 or ((Longint(__click_closes) shl bp_TGnomeDialog_click_closes) and bm_TGnomeDialog_click_closes);
end;

function just_hide(var a : TGnomeDialog) : Boolean;
begin
   just_hide:=Boolean((a.flag0 and bm_TGnomeDialog_just_hide) shr bp_TGnomeDialog_just_hide);
end;

procedure set_just_hide(var a : TGnomeDialog; __just_hide : Boolean);
begin
   a.flag0:=a.flag0 or ((Longint(__just_hide) shl bp_TGnomeDialog_just_hide) and bm_TGnomeDialog_just_hide);
end;

{$Endif read_implementation}

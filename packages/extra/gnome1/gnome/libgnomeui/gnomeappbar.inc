{$IfDef read_interface}
type
   PGnomeAppBar = ^TGnomeAppBar;
   TGnomeAppBar = record
        parent_widget : TGtkHBox;
        progress : PGtkWidget;
        status : PGtkWidget;
        flag0 : word;
        editable_start : gint;
        prompt : Pgchar;
        status_stack : PGSList;
        default_status : Pgchar;
     end;
   GNOME_APPBAR = PGnomeAppBar;

const
   bm__GnomeAppBar_interactive = $1;
   bp__GnomeAppBar_interactive = 0;

function interactive(var a : TGnomeAppBar) : gboolean;
procedure set_interactive(var a : TGnomeAppBar; __interactive : gboolean);

type
   PGnomeAppBarClass = ^TGnomeAppBarClass;
   TGnomeAppBarClass = record
        parent_class : TGtkHBoxClass;
        user_response : procedure (ab:PGnomeAppBar);cdecl;
        clear_prompt : procedure (ab:PGnomeAppBar);cdecl;
     end;
   GNOME_APPBAR_CLASS = PGnomeAppBarClass;

function GNOME_TYPE_APPBAR : TGTKType;
function GNOME_IS_APPBAR(obj : Pointer) : Boolean;
function GNOME_IS_APPBAR_CLASS(klass : Pointer) : Boolean;

function GNOME_APPBAR_HAS_STATUS(appbar : PGnomeAppBar) : gboolean;
function GNOME_APPBAR_HAS_PROGRESS(appbar : PGnomeAppBar) : gboolean;
function GNOME_APPBAR_INTERACTIVE(ab : PGnomeAppBar) : gboolean;

function gnome_appbar_get_type:TGTKType;cdecl;external libgnomeuidll name 'gnome_appbar_get_type';
function gnome_appbar_new(has_progress:gboolean; has_status:gboolean; interactivity:TGnomePreferencesType):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_appbar_new';
procedure gnome_appbar_set_status(appbar:PGnomeAppBar; status:Pgchar);cdecl;external libgnomeuidll name 'gnome_appbar_set_status';
procedure gnome_appbar_set_default(appbar:PGnomeAppBar; default_status:Pgchar);cdecl;external libgnomeuidll name 'gnome_appbar_set_default';
procedure gnome_appbar_push(appbar:PGnomeAppBar; status:Pgchar);cdecl;external libgnomeuidll name 'gnome_appbar_push';
procedure gnome_appbar_pop(appbar:PGnomeAppBar);cdecl;external libgnomeuidll name 'gnome_appbar_pop';
procedure gnome_appbar_clear_stack(appbar:PGnomeAppBar);cdecl;external libgnomeuidll name 'gnome_appbar_clear_stack';
procedure gnome_appbar_set_progress(appbar:PGnomeAppBar; percentage:gfloat);cdecl;external libgnomeuidll name 'gnome_appbar_set_progress';
function gnome_appbar_get_progress(appbar:PGnomeAppBar):PGtkProgress;cdecl;external libgnomeuidll name 'gnome_appbar_get_progress';
procedure gnome_appbar_refresh(appbar:PGnomeAppBar);cdecl;external libgnomeuidll name 'gnome_appbar_refresh';
procedure gnome_appbar_set_prompt(appbar:PGnomeAppBar; prompt:Pgchar; modal:gboolean);cdecl;external libgnomeuidll name 'gnome_appbar_set_prompt';
procedure gnome_appbar_clear_prompt(appbar:PGnomeAppBar);cdecl;external libgnomeuidll name 'gnome_appbar_clear_prompt';
function gnome_appbar_get_response(appbar:PGnomeAppBar):Pgchar;cdecl;external libgnomeuidll name 'gnome_appbar_get_response';
procedure gnome_appbar_construct(ab:PGnomeAppBar; has_progress:gboolean; has_status:gboolean; interactivity:TGnomePreferencesType);cdecl;external libgnomeuidll name 'gnome_appbar_construct';

{$EndIf read_interface}

{$Ifdef read_implementation}

function GNOME_TYPE_APPBAR : TGTKType;
begin
  GNOME_TYPE_APPBAR:=gnome_appbar_get_type;
end;

function GNOME_IS_APPBAR(obj : pointer) : Boolean;
begin
   GNOME_IS_APPBAR:=(obj<>nil) and GNOME_IS_APPBAR_CLASS(PGtkTypeObject(obj)^.klass);
end;

function GNOME_IS_APPBAR_CLASS(klass : pointer) : boolean;
begin
   GNOME_IS_APPBAR_CLASS:= (klass<>nil) and (PGtkTypeClass(klass)^.thetype=GNOME_TYPE_APPBAR);
end;

function GNOME_APPBAR_HAS_STATUS(appbar : PGnomeAppBar) : gboolean;
begin
   GNOME_APPBAR_HAS_STATUS:=(appbar^.status) <> NULL;
end;

function GNOME_APPBAR_HAS_PROGRESS(appbar : PGnomeAppBar) : gboolean;
begin
   GNOME_APPBAR_HAS_PROGRESS:=(appbar^.progress) <> NULL;
end;

function interactive(var a : TGnomeAppBar) : gboolean;
begin
   interactive:=gboolean((a.flag0 and bm__GnomeAppBar_interactive) shr bp__GnomeAppBar_interactive);
end;

procedure set_interactive(var a : TGnomeAppBar; __interactive : gboolean);
begin
   a.flag0:=a.flag0 or ((gint(__interactive) shl bp__GnomeAppBar_interactive) and bm__GnomeAppBar_interactive);
end;

function GNOME_APPBAR_INTERACTIVE(ab : PGnomeAppBar) : gboolean;
var
   if_local1 : gboolean;
begin
   if ab <> nil then
     if_local1:=interactive(ab^)
   else
     if_local1:=False;
   GNOME_APPBAR_INTERACTIVE:=if_local1;
end;

{$Endif read_implementation}

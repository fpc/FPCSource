{$IfDef read_interface}
const
   GNOME_MESSAGE_BOX_INFO = 'info';
   GNOME_MESSAGE_BOX_WARNING = 'warning';
   GNOME_MESSAGE_BOX_ERROR = 'error';
   GNOME_MESSAGE_BOX_QUESTION = 'question';
   GNOME_MESSAGE_BOX_GENERIC = 'generic';

type
   PGnomeMessageBox = ^TGnomeMessageBox;
   TGnomeMessageBox = record
        dialog : TGnomeDialog;
     end;
   GNOME_MESSAGE_BOX = PGnomeMessageBox;

   PGnomeMessageBoxClass = ^TGnomeMessageBoxClass;
   TGnomeMessageBoxClass = record
        parent_class : TGnomeDialogClass;
     end;
   GNOME_MESSAGE_BOX_CLASS = PGnomeMessageBoxClass;

function GNOME_TYPE_MESSAGE_BOX : TGTKType;
function GNOME_IS_MESSAGE_BOX(obj : Pointer) : Boolean;
function GNOME_IS_MESSAGE_BOX_CLASS(klass : Pointer) : Boolean;

function gnome_message_box_get_type:TGTKType;cdecl;external libgnomeuidll name 'gnome_message_box_get_type';
function gnome_message_box_new(message:Pgchar; messagebox_type:Pgchar; args:array of const):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_message_box_new';
function gnome_message_box_newv(message:Pgchar; messagebox_type:Pgchar; buttons:PPgchar):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_message_box_newv';

{$EndIf read_interface}

{$Ifdef read_implementation}

function GNOME_TYPE_MESSAGE_BOX : TGTKType;
begin
  GNOME_TYPE_MESSAGE_BOX:=gnome_message_box_get_type;
end;

function GNOME_IS_MESSAGE_BOX(obj : Pointer) : Boolean;
begin
   GNOME_IS_MESSAGE_BOX:=(obj<>nil) and GNOME_IS_MESSAGE_BOX_CLASS(PGtkTypeObject(obj)^.klass);
end;

function GNOME_IS_MESSAGE_BOX_CLASS(klass : Pointer) : Boolean;
begin
   GNOME_IS_MESSAGE_BOX_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GNOME_TYPE_MESSAGE_BOX);
end;

{$Endif read_implementation}

{$IfDef read_interface}

type
   PGnomeAbout =  ^TGnomeAbout;
   TGnomeAbout = record
        dialog : TGnomeDialog;
     end;
   GNOME_ABOUT = PGnomeAbout;

   PGnomeAboutClass =  ^TGnomeAboutClass;
   TGnomeAboutClass = record
        parent_class : TGnomeDialogClass;
     end;
   GNOME_ABOUT_CLASS = PGnomeAboutClass;

function GNOME_TYPE_ABOUT : TGTKType;
function GNOME_IS_ABOUT(obj : Pointer) : Boolean;
function GNOME_IS_ABOUT_CLASS(klass : Pointer) : Boolean;

function gnome_about_get_type:TGTKType;cdecl;external libgnomeuidll name 'gnome_about_get_type';
function gnome_about_new(title:Pgchar; version:Pgchar; copyright:Pgchar; authors:PPgchar; comments:Pgchar;
           logo:Pgchar):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_about_new';
procedure gnome_about_construct(about:PGnomeAbout; title:Pgchar; version:Pgchar; copyright:Pgchar; authors:PPgchar;
            comments:Pgchar; logo:Pgchar);cdecl;external libgnomeuidll name 'gnome_about_construct';

{$EndIf read_interface}

{$Ifdef read_implementation}

function GNOME_TYPE_ABOUT : TGTKType;
begin
  GNOME_TYPE_ABOUT:=gnome_about_get_type;
end;

function GNOME_IS_ABOUT(obj : Pointer) : Boolean;
begin
   GNOME_IS_ABOUT:=(obj<>nil) and GNOME_IS_ABOUT_CLASS(PGtkTypeObject(obj)^.klass);
end;

function GNOME_IS_ABOUT_CLASS(klass : Pointer) : Boolean;
begin
   GNOME_IS_ABOUT_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GNOME_TYPE_ABOUT);
end;

{$Endif read_implementation}

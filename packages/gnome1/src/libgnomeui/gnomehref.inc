{$IfDef read_interface}

type
   PGnomeHRef = ^TGnomeHRef;
   TGnomeHRef = record
        button : TGtkButton;
        url : Pgchar;
        thelabel : PGtkWidget;
     end;
   GNOME_HREF = PGnomeHRef;

   PGnomeHRefClass = ^TGnomeHRefClass;
   TGnomeHRefClass = record
        parent_class : TGtkButtonClass;
     end;
   GNOME_HREF_CLASS = PGnomeHRefClass;

function GNOME_TYPE_HREF : TGTKType;
function GNOME_IS_HREF(obj : Pointer) : Boolean;
function GNOME_IS_HREF_CLASS(klass : Pointer) : Boolean;

function gnome_href_get_type:TGTKType;cdecl;external libgnomeuidll name 'gnome_href_get_type';
function gnome_href_new(url:Pgchar; thelabel:Pgchar):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_href_new';
procedure gnome_href_set_url(href:PGnomeHRef; url:Pgchar);cdecl;external libgnomeuidll name 'gnome_href_set_url';
function gnome_href_get_url(href:PGnomeHRef):Pgchar;cdecl;external libgnomeuidll name 'gnome_href_get_url';
procedure gnome_href_set_label(href:PGnomeHRef; thelabel:Pgchar);cdecl;external libgnomeuidll name 'gnome_href_set_label';
function gnome_href_get_label(href:PGnomeHRef):Pgchar;cdecl;external libgnomeuidll name 'gnome_href_get_label';

{$EndIf read_interface}

{$Ifdef read_implementation}

function GNOME_TYPE_HREF : TGTKType;
begin
  GNOME_TYPE_HREF:=gnome_href_get_type;
end;

function GNOME_IS_HREF(obj : Pointer) : Boolean;
begin
   GNOME_IS_HREF:=(obj<>nil) and GNOME_IS_HREF_CLASS(PGtkTypeObject(obj)^.klass);
end;

function GNOME_IS_HREF_CLASS(klass : Pointer) : Boolean;
begin
   GNOME_IS_HREF_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GNOME_TYPE_HREF);
end;

{$Endif read_implementation}

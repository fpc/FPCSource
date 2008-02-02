{$IfDef read_interface}
Type
   PGnomenumberEntry = ^TGnomeNumberEntry;
   TGnomeNumberEntry = record
        hbox : TGtkHBox;
        calc_dialog_title : Pgchar;
        calc_dlg : PGtkWidget;
        gentry : PGtkWidget;
     end;
   GNOME_NUMBER_ENTRY = PGnomeNumberEntry;

   PGnomenumberEntryClass = ^TGnomeNumberEntryClass;
   TGnomeNumberEntryClass = record
        parent_class : TGtkHBoxClass;
     end;

   GNOME_NUMBER_ENTRY_CLASS = PGnomeNumberEntryClass;


function GNOME_TYPE_NUMBER_ENTRY : TGTKType;
function GNOME_IS_NUMBER_ENTRY(obj : Pointer) : Boolean;
function GNOME_IS_NUMBER_ENTRY_CLASS(klass : Pointer) : Boolean;

function gnome_number_entry_get_type:TGTKType;cdecl;external libgnomeuidll name 'gnome_number_entry_get_type';
function gnome_number_entry_new(history_id:Pgchar; calc_dialog_title:Pgchar):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_number_entry_new';
function gnome_number_entry_gnome_entry(nentry:PGnomeNumberEntry):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_number_entry_gnome_entry';
function gnome_number_entry_gtk_entry(nentry:PGnomeNumberEntry):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_number_entry_gtk_entry';
procedure gnome_number_entry_set_title(nentry:PGnomeNumberEntry; calc_dialog_title:Pgchar);cdecl;external libgnomeuidll name 'gnome_number_entry_set_title';
function gnome_number_entry_get_number(nentry:PGnomeNumberEntry):gdouble;cdecl;external libgnomeuidll name 'gnome_number_entry_get_number';

{$EndIf read_interface}

{$Ifdef read_implementation}

function GNOME_TYPE_NUMBER_ENTRY : TGTKType;
begin
  GNOME_TYPE_NUMBER_ENTRY:=gnome_number_entry_get_type;
end;

function GNOME_IS_NUMBER_ENTRY(obj : Pointer) : Boolean;
begin
   GNOME_IS_NUMBER_ENTRY:=(obj<>nil) and GNOME_IS_NUMBER_ENTRY_CLASS(PGtkTypeObject(obj)^.klass);
end;

function GNOME_IS_NUMBER_ENTRY_CLASS(klass : Pointer) : Boolean;
begin
   GNOME_IS_NUMBER_ENTRY_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GNOME_TYPE_NUMBER_ENTRY);
end;

{$Endif read_implementation}

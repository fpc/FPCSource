{$IfDef read_interface}
type
   PGnomeFileEntry = ^TGnomeFileEntry;
   TGnomeFileEntry = record
        hbox : TGtkHBox;
        browse_dialog_title : Pchar;
        default_path : Pchar;
        fsw : PGtkWidget;
        is_modal : gboolean;
        directory_entry : gboolean;
        gentry : PGtkWidget;
     end;
   GNOME_FILE_ENTRY = PGnomeFileEntry;

   PGnomeFileEntryClass = ^TGnomeFileEntryClass;
   TGnomeFileEntryClass = record
        parent_class : TGtkHBoxClass;
        browse_clicked : procedure (fentry:PGnomeFileEntry);cdecl;
     end;
   GNOME_FILE_ENTRY_CLASS = PGnomeFileEntryClass;


function GNOME_TYPE_FILE_ENTRY : TGTKType;
function GNOME_IS_FILE_ENTRY(obj : Pointer) : Boolean;
function GNOME_IS_FILE_ENTRY_CLASS(klass : Pointer) : Boolean;

function gnome_file_entry_get_type:TGTKType;cdecl;external libgnomeuidll name 'gnome_file_entry_get_type';
function gnome_file_entry_new(history_id:Pchar; browse_dialog_title:Pchar):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_file_entry_new';
procedure gnome_file_entry_construct(fentry:PGnomeFileEntry; history_id:Pchar; browse_dialog_title:Pchar);cdecl;external libgnomeuidll name 'gnome_file_entry_construct';
function gnome_file_entry_gnome_entry(fentry:PGnomeFileEntry):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_file_entry_gnome_entry';
function gnome_file_entry_gtk_entry(fentry:PGnomeFileEntry):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_file_entry_gtk_entry';
procedure gnome_file_entry_set_title(fentry:PGnomeFileEntry; browse_dialog_title:Pchar);cdecl;external libgnomeuidll name 'gnome_file_entry_set_title';
procedure gnome_file_entry_set_default_path(fentry:PGnomeFileEntry; path:Pchar);cdecl;external libgnomeuidll name 'gnome_file_entry_set_default_path';
procedure gnome_file_entry_set_directory(fentry:PGnomeFileEntry; directory_entry:gboolean);cdecl;external libgnomeuidll name 'gnome_file_entry_set_directory';
function gnome_file_entry_get_full_path(fentry:PGnomeFileEntry; file_must_exist:gboolean):Pchar;cdecl;external libgnomeuidll name 'gnome_file_entry_get_full_path';
procedure gnome_file_entry_set_modal(fentry:PGnomeFileEntry; is_modal:gboolean);cdecl;external libgnomeuidll name 'gnome_file_entry_set_modal';

{$EndIf read_interface}

{$Ifdef read_implementation}

function GNOME_TYPE_FILE_ENTRY : TGTKType;
begin
  GNOME_TYPE_FILE_ENTRY:=gnome_file_entry_get_type;
end;

function GNOME_IS_FILE_ENTRY(obj : Pointer) : Boolean;
begin
   GNOME_IS_FILE_ENTRY:=(obj<>nil) and GNOME_IS_FILE_ENTRY_CLASS(PGtkTypeObject(obj)^.klass);
end;

function GNOME_IS_FILE_ENTRY_CLASS(klass : Pointer) : Boolean;
begin
   GNOME_IS_FILE_ENTRY_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GNOME_TYPE_FILE_ENTRY);
end;

{$Endif read_implementation}

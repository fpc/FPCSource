{$IfDef read_interface}

type
   PGnomeIconEntry = ^TGnomeIconEntry;
   TGnomeIconEntry = record
        vbox : TGtkVBox;
        fentry : PGtkWidget;
        pickbutton : PGtkWidget;
        pick_dialog : PGtkWidget;
        pick_dialog_dir : Pgchar;
     end;
   GNOME_ICON_ENTRY = PGnomeIconEntry;

   PGnomeIconEntryClass = ^TGnomeIconEntryClass;
   TGnomeIconEntryClass = record
        parent_class : TGtkVBoxClass;
     end;
   GNOME_ICON_ENTRY_CLASS = PGnomeIconEntryClass;

function GNOME_TYPE_ICON_ENTRY : TGTKType;
function GNOME_IS_ICON_ENTRY(obj : Pointer) : Boolean;
function GNOME_IS_ICON_ENTRY_CLASS(klass : Pointer) : Boolean;

function gnome_icon_entry_get_type:TGTKType;cdecl;external libgnomeuidll name 'gnome_icon_entry_get_type';
function gnome_icon_entry_new(history_id:Pgchar; browse_dialog_title:Pgchar):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_icon_entry_new';
procedure gnome_icon_entry_set_pixmap_subdir(ientry:PGnomeIconEntry; subdir:Pgchar);cdecl;external libgnomeuidll name 'gnome_icon_entry_set_pixmap_subdir';
procedure gnome_icon_entry_set_icon(ientry:PGnomeIconEntry; filename:Pgchar);cdecl;external libgnomeuidll name 'gnome_icon_entry_set_icon';
function gnome_icon_entry_gnome_file_entry(ientry:PGnomeIconEntry):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_icon_entry_gnome_file_entry';
function gnome_icon_entry_gnome_entry(ientry:PGnomeIconEntry):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_icon_entry_gnome_entry';
function gnome_icon_entry_gtk_entry(ientry:PGnomeIconEntry):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_icon_entry_gtk_entry';
function gnome_icon_entry_get_filename(ientry:PGnomeIconEntry):Pgchar;cdecl;external libgnomeuidll name 'gnome_icon_entry_get_filename';

{$EndIf read_interface}

{$Ifdef read_implementation}

function GNOME_TYPE_ICON_ENTRY : TGTKType;
begin
  GNOME_TYPE_ICON_ENTRY:=gnome_icon_entry_get_type;
end;

function GNOME_IS_ICON_ENTRY(obj : Pointer) : Boolean;
begin
   GNOME_IS_ICON_ENTRY:=(obj<>nil) and GNOME_IS_ICON_ENTRY_CLASS(PGtkTypeObject(obj)^.klass);
end;

function GNOME_IS_ICON_ENTRY_CLASS(klass : Pointer) : Boolean;
begin
   GNOME_IS_ICON_ENTRY_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GNOME_TYPE_ICON_ENTRY);
end;

{$Endif read_implementation}

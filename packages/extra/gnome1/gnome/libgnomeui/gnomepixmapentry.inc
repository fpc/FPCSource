{$IfDef read_interface}
Type
   PGnomePixmapEntry = ^TGnomeNumberEntry;
   TGnomePixmapEntry = record
        vbox : TGtkVBox;
        fentry : PGtkWidget;
        do_preview : gboolean;
        preview : PGtkWidget;
        preview_sw : PGtkWidget;
        last_preview : Pgchar;
     end;
   GNOME_PIXMAP_ENTRY = PGnomeNumberEntry;

   PGnomePixmapEntryClass = ^TGnomePixmapEntryClass;
   TGnomePixmapEntryClass = record
        parent_class : TGtkVBoxClass;
     end;
   GNOME_PIXMAP_ENTRY_CLASS = PGnomePixmapEntryClass;


function GNOME_TYPE_PIXMAP_ENTRY : TGTKType;
function GNOME_IS_PIXMAP_ENTRY(obj : Pointer) : Boolean;
function GNOME_IS_PIXMAP_ENTRY_CLASS(klass : Pointer) : Boolean;


function gnome_pixmap_entry_get_type:TGTKType;cdecl;external libgnomeuidll name 'gnome_pixmap_entry_get_type';
function gnome_pixmap_entry_new(history_id:Pgchar; browse_dialog_title:Pgchar; do_preview:gboolean):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_pixmap_entry_new';
procedure gnome_pixmap_entry_construct(gentry:PGnomePixmapEntry; history_id:Pgchar; browse_dialog_title:Pgchar; do_preview:gboolean);cdecl;external libgnomeuidll name 'gnome_pixmap_entry_construct';
procedure gnome_pixmap_entry_set_pixmap_subdir(pentry:PGnomePixmapEntry; subdir:Pgchar);cdecl;external libgnomeuidll name 'gnome_pixmap_entry_set_pixmap_subdir';
function gnome_pixmap_entry_gnome_file_entry(pentry:PGnomePixmapEntry):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_pixmap_entry_gnome_file_entry';
function gnome_pixmap_entry_gnome_entry(pentry:PGnomePixmapEntry):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_pixmap_entry_gnome_entry';
function gnome_pixmap_entry_gtk_entry(pentry:PGnomePixmapEntry):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_pixmap_entry_gtk_entry';
procedure gnome_pixmap_entry_set_preview(pentry:PGnomePixmapEntry; do_preview:gboolean);cdecl;external libgnomeuidll name 'gnome_pixmap_entry_set_preview';
procedure gnome_pixmap_entry_set_preview_size(pentry:PGnomePixmapEntry; preview_w:gint; preview_h:gint);cdecl;external libgnomeuidll name 'gnome_pixmap_entry_set_preview_size';
function gnome_pixmap_entry_get_filename(pentry:PGnomePixmapEntry):Pgchar;cdecl;external libgnomeuidll name 'gnome_pixmap_entry_get_filename';

{$EndIf read_interface}

{$Ifdef read_implementation}

function GNOME_TYPE_PIXMAP_ENTRY : TGTKType;
begin
  GNOME_TYPE_PIXMAP_ENTRY:=gnome_pixmap_entry_get_type;
end;

function GNOME_IS_PIXMAP_ENTRY(obj : Pointer) : Boolean;
begin
   GNOME_IS_PIXMAP_ENTRY:=(obj<>nil) and GNOME_IS_PIXMAP_ENTRY_CLASS(PGtkTypeObject(obj)^.klass);
end;

function GNOME_IS_PIXMAP_ENTRY_CLASS(klass : Pointer) : Boolean;
begin
   GNOME_IS_PIXMAP_ENTRY_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GNOME_TYPE_PIXMAP_ENTRY);
end;

{$Endif read_implementation}

{$IfDef read_interface}
type
   PGnomeEntry = ^TGnomeEntry;
   TGnomeEntry = record
        combo : TGtkCombo;
        changed : gboolean;
        history_id : Pgchar;
        items : PGList;
        max_saved : guint;
     end;
   GNOME_ENTRY = PGnomeEntry;

   PGnomeEntryClass = ^TGnomeEntryClass;
   TGnomeEntryClass = record
        parent_class : TGtkComboClass;
     end;
   GNOME_ENTRY_CLASS = PGnomeEntryClass;

function GNOME_TYPE_ENTRY : TGTKType;
function GNOME_IS_ENTRY(obj : Pointer) : Boolean;
function GNOME_IS_ENTRY_CLASS(klass : Pointer) : Boolean;

function gnome_entry_get_type:TGTKType;cdecl;external libgnomeuidll name 'gnome_entry_get_type';
function gnome_entry_new(history_id:Pgchar):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_entry_new';
function gnome_entry_gtk_entry(gentry:PGnomeEntry):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_entry_gtk_entry';
procedure gnome_entry_set_history_id(gentry:PGnomeEntry; history_id:Pgchar);cdecl;external libgnomeuidll name 'gnome_entry_set_history_id';
procedure gnome_entry_set_max_saved(gentry:PGnomeEntry; max_saved:guint);cdecl;external libgnomeuidll name 'gnome_entry_set_max_saved';
procedure gnome_entry_prepend_history(gentry:PGnomeEntry; save:gint; thetext:Pgchar);cdecl;external libgnomeuidll name 'gnome_entry_prepend_history';
procedure gnome_entry_append_history(gentry:PGnomeEntry; save:gint; thetext:Pgchar);cdecl;external libgnomeuidll name 'gnome_entry_append_history';
procedure gnome_entry_load_history(gentry:PGnomeEntry);cdecl;external libgnomeuidll name 'gnome_entry_load_history';
procedure gnome_entry_save_history(gentry:PGnomeEntry);cdecl;external libgnomeuidll name 'gnome_entry_save_history';

{$EndIf read_interface}

{$Ifdef read_implementation}

function GNOME_TYPE_ENTRY : TGTKType;
begin
  GNOME_TYPE_ENTRY:=gnome_entry_get_type;
end;

function GNOME_IS_ENTRY(obj : Pointer) : Boolean;
begin
   GNOME_IS_ENTRY:=(obj<>nil) and GNOME_IS_ENTRY_CLASS(PGtkTypeObject(obj)^.klass);
end;

function GNOME_IS_ENTRY_CLASS(klass : Pointer) : Boolean;
begin
   GNOME_IS_ENTRY_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GNOME_TYPE_ENTRY);
end;

{$Endif read_implementation}

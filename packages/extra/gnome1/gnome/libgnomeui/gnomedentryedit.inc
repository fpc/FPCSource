{$IfDef read_interface}

type
   PGnomeDEntryEdit = ^TGnomeDEntryEdit;
   TGnomeDEntryEdit = record
        theobject : TGtkObject;
        child1 : PGtkWidget;
        child2 : PGtkWidget;
        name_entry : PGtkWidget;
        comment_entry : PGtkWidget;
        exec_entry : PGtkWidget;
        tryexec_entry : PGtkWidget;
        doc_entry : PGtkWidget;
        type_combo : PGtkWidget;
        terminal_button : PGtkWidget;
        icon_entry : PGtkWidget;
        translations : PGtkWidget;
        transl_lang_entry : PGtkWidget;
        transl_name_entry : PGtkWidget;
        transl_comment_entry : PGtkWidget;
     end;
   GNOME_DENTRY_EDIT = PGnomeDEntryEdit;

   PGnomeDEntryEditClass = ^TGnomeDEntryEditClass;
   TGnomeDEntryEditClass = record
        parent_class : TGtkObjectClass;
        changed : procedure (gee:PGnomeDEntryEdit);cdecl;
        icon_changed : procedure (gee:PGnomeDEntryEdit);cdecl;
        name_changed : procedure (gee:PGnomeDEntryEdit);cdecl;
     end;
   GNOME_DENTRY_EDIT_CLASS = PGnomeDEntryEditClass;

function GNOME_TYPE_DENTRY_EDIT : TGTKType;
function GNOME_IS_DENTRY_EDIT(obj : Pointer) : Boolean;
function GNOME_IS_DENTRY_EDIT_CLASS(klass : Pointer) : Boolean;

function gnome_dentry_edit_get_type:TGTKType;cdecl;external libgnomeuidll name 'gnome_dentry_edit_get_type';
function gnome_dentry_edit_new:PGtkObject;cdecl;external libgnomeuidll name 'gnome_dentry_edit_new';

function gnome_dentry_edit_child1(d : PGnomeDEntryEdit) : PGTKWidget;
function gnome_dentry_edit_child2(d : PGnomeDEntryEdit) : PGTKWidget;

function gnome_dentry_edit_new_notebook(notebook:PGtkNotebook):PGtkObject;cdecl;external libgnomeuidll name 'gnome_dentry_edit_new_notebook';
procedure gnome_dentry_edit_clear(dee:PGnomeDEntryEdit);cdecl;external libgnomeuidll name 'gnome_dentry_edit_clear';
procedure gnome_dentry_edit_load_file(dee:PGnomeDEntryEdit; path:Pgchar);cdecl;external libgnomeuidll name 'gnome_dentry_edit_load_file';
procedure gnome_dentry_edit_set_dentry(dee:PGnomeDEntryEdit; dentry:PGnomeDesktopEntry);cdecl;external libgnomeuidll name 'gnome_dentry_edit_set_dentry';
function gnome_dentry_edit_get_dentry(dee:PGnomeDEntryEdit):PGnomeDesktopEntry;cdecl;external libgnomeuidll name 'gnome_dentry_edit_get_dentry';
function gnome_dentry_get_dentry(dee:PGnomeDEntryEdit):PGnomeDesktopEntry;cdecl;external libgnomeuidll name 'gnome_dentry_get_dentry';
function gnome_dentry_edit_get_icon(dee:PGnomeDEntryEdit):Pgchar;cdecl;external libgnomeuidll name 'gnome_dentry_edit_get_icon';
function gnome_dentry_edit_get_name(dee:PGnomeDEntryEdit):Pgchar;cdecl;external libgnomeuidll name 'gnome_dentry_edit_get_name';
function gnome_dentry_get_name_entry(dee:PGnomeDEntryEdit):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_dentry_get_name_entry';
function gnome_dentry_get_comment_entry(dee:PGnomeDEntryEdit):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_dentry_get_comment_entry';
function gnome_dentry_get_exec_entry(dee:PGnomeDEntryEdit):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_dentry_get_exec_entry';
function gnome_dentry_get_tryexec_entry(dee:PGnomeDEntryEdit):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_dentry_get_tryexec_entry';
function gnome_dentry_get_doc_entry(dee:PGnomeDEntryEdit):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_dentry_get_doc_entry';
function gnome_dentry_get_icon_entry(dee:PGnomeDEntryEdit):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_dentry_get_icon_entry';

{$EndIf read_interface}

{$Ifdef read_implementation}

function GNOME_TYPE_DENTRY_EDIT : TGTKType;
begin
  GNOME_TYPE_DENTRY_EDIT:=gnome_dentry_edit_get_type;
end;

function GNOME_IS_DENTRY_EDIT(obj : Pointer) : Boolean;
begin
   GNOME_IS_DENTRY_EDIT:=(obj<>nil) and GNOME_IS_DENTRY_EDIT_CLASS(PGtkTypeObject(obj)^.klass);
end;

function GNOME_IS_DENTRY_EDIT_CLASS(klass : Pointer) : Boolean;
begin
   GNOME_IS_DENTRY_EDIT_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GNOME_TYPE_DENTRY_EDIT);
end;

function gnome_dentry_edit_child1(d : PGnomeDEntryEdit) : PGTKWidget;
begin
   gnome_dentry_edit_child1:=d^.child1;
end;

function gnome_dentry_edit_child2(d : PGnomeDEntryEdit) : PGTKWidget;
begin
   gnome_dentry_edit_child2:=d^.child2;
end;

{$Endif read_implementation}

{$IfDef read_interface}

type
   PGnomeIconSelection = ^TGnomeIconSelection;
   TGnomeIconSelection = record
        vbox : TGtkVBox;
        box : PGtkWidget;
        gil : PGtkWidget;
        file_list : PGList;
        stop_loading : longint;
     end;
   GNOME_ICON_SELECTION = PGnomeIconSelection;

   PGnomeIconSelectionClass = ^TGnomeIconSelectionClass;
   TGnomeIconSelectionClass = record
        parent_class : TGtkVBoxClass;
     end;
   GNOME_ICON_SELECTION_CLASS = PGnomeIconSelectionClass;

function GNOME_TYPE_ICON_SELECTION : TGTKType;
function GNOME_IS_ICON_SELECTION(obj : Pointer) : Boolean;
function GNOME_IS_ICON_SELECTION_CLASS(klass : Pointer) : Boolean;

function gnome_icon_selection_get_type:TGTKType;cdecl;external libgnomeuidll name 'gnome_icon_selection_get_type';
function gnome_icon_selection_new:PGtkWidget;cdecl;external libgnomeuidll name 'gnome_icon_selection_new';
procedure gnome_icon_selection_add_defaults(gis:PGnomeIconSelection);cdecl;external libgnomeuidll name 'gnome_icon_selection_add_defaults';
procedure gnome_icon_selection_add_directory(gis:PGnomeIconSelection; dir:Pgchar);cdecl;external libgnomeuidll name 'gnome_icon_selection_add_directory';
procedure gnome_icon_selection_show_icons(gis:PGnomeIconSelection);cdecl;external libgnomeuidll name 'gnome_icon_selection_show_icons';
procedure gnome_icon_selection_clear(gis:PGnomeIconSelection; not_shown:gboolean);cdecl;external libgnomeuidll name 'gnome_icon_selection_clear';
function gnome_icon_selection_get_icon(gis:PGnomeIconSelection; full_path:gboolean):Pgchar;cdecl;external libgnomeuidll name 'gnome_icon_selection_get_icon';
procedure gnome_icon_selection_select_icon(gis:PGnomeIconSelection; filename:Pgchar);cdecl;external libgnomeuidll name 'gnome_icon_selection_select_icon';
procedure gnome_icon_selection_stop_loading(gis:PGnomeIconSelection);cdecl;external libgnomeuidll name 'gnome_icon_selection_stop_loading';

{$EndIf read_interface}

{$Ifdef read_implementation}

function GNOME_TYPE_ICON_SELECTION : TGTKType;
begin
  GNOME_TYPE_ICON_SELECTION:=gnome_icon_selection_get_type;
end;

function GNOME_IS_ICON_SELECTION(obj : Pointer) : Boolean;
begin
   GNOME_IS_ICON_SELECTION:=(obj<>nil) and GNOME_IS_ICON_SELECTION_CLASS(PGtkTypeObject(obj)^.klass);
end;

function GNOME_IS_ICON_SELECTION_CLASS(klass : Pointer) : Boolean;
begin
   GNOME_IS_ICON_SELECTION_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GNOME_TYPE_ICON_SELECTION);
end;

{$Endif read_implementation}

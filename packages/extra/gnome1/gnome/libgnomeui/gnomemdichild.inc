{$IfDef read_interface}

type
   TGnomeMDIChildViewCreator = Function : PGtkWidget; cdecl;
   TGnomeMDIChildMenuCreator = Function : PGList; cdecl;
   TGnomeMDIChildConfigFunc = Function : Pgchar; cdecl;
   TGnomeMDIChildLabelFunc = Function : PGtkWidget; cdecl;

   PGnomeMDIChild = ^TGnomeMDIChild;
   TGnomeMDIChild = record
        theobject : TGtkObject;
        parent : PGtkObject;
        name : Pgchar;
        views : PGList;
        menu_template : PGnomeUIInfo;
     end;
   GNOME_MDI_CHILD = PGnomeMDIChild;

   PGnomeMDIChildClass = ^TGnomeMDIChildClass;
   TGnomeMDIChildClass = record
        parent_class : TGtkObjectClass;
        create_view : TGnomeMDIChildViewCreator;
        create_menus : TGnomeMDIChildMenuCreator;
        get_config_string : TGnomeMDIChildConfigFunc;
        set_label : TGnomeMDIChildLabelFunc;
     end;
   GNOME_MDI_CHILD_CLASS = PGnomeMDIChildClass;

function GNOME_TYPE_MDI_CHILD : TGTKType;
function GNOME_IS_MDI_CHILD(obj : Pointer) : Boolean;
function GNOME_IS_MDI_CHILD_CLASS(klass : Pointer) : Boolean;

function gnome_mdi_child_get_type:TGTKType;cdecl;external libgnomeuidll name 'gnome_mdi_child_get_type';
function gnome_mdi_child_add_view(mdi_child:PGnomeMDIChild):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_mdi_child_add_view';
procedure gnome_mdi_child_remove_view(mdi_child:PGnomeMDIChild; view:PGtkWidget);cdecl;external libgnomeuidll name 'gnome_mdi_child_remove_view';
procedure gnome_mdi_child_set_name(mdi_child:PGnomeMDIChild; name:Pgchar);cdecl;external libgnomeuidll name 'gnome_mdi_child_set_name';
procedure gnome_mdi_child_set_menu_template(mdi_child:PGnomeMDIChild; menu_tmpl:PGnomeUIInfo);cdecl;external libgnomeuidll name 'gnome_mdi_child_set_menu_template';

{$EndIf read_interface}

{$Ifdef read_implementation}

function GNOME_TYPE_MDI_CHILD : TGTKType;
begin
  GNOME_TYPE_MDI_CHILD:=gnome_mdi_child_get_type;
end;

function GNOME_IS_MDI_CHILD(obj : Pointer) : Boolean;
begin
   GNOME_IS_MDI_CHILD:=(obj<>nil) and GNOME_IS_MDI_CHILD_CLASS(PGtkTypeObject(obj)^.klass);
end;

function GNOME_IS_MDI_CHILD_CLASS(klass : Pointer) : Boolean;
begin
   GNOME_IS_MDI_CHILD_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GNOME_TYPE_MDI_CHILD);
end;

{$Endif read_implementation}

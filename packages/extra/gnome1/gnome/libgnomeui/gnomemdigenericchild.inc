{$IfDef read_interface}

type
   PGnomeMDIGenericChild = ^TGnomeMDIGenericChild;
   TGnomeMDIGenericChild = record
        mdi_child : TGnomeMDIChild;
        create_view : TGnomeMDIChildViewCreator;
        create_menus : TGnomeMDIChildMenuCreator;
        get_config_string : TGnomeMDIChildConfigFunc;
        set_label : TGnomeMDIChildLabelFunc;
        create_view_cbm : TGtkCallbackMarshal;
        create_menus_cbm : TGtkCallbackMarshal;
        get_config_string_cbm : TGtkCallbackMarshal;
        set_label_cbm : TGtkCallbackMarshal;
        create_view_dn : TGtkDestroyNotify;
        create_menus_dn : TGtkDestroyNotify;
        get_config_string_dn : TGtkDestroyNotify;
        set_label_dn : TGtkDestroyNotify;
        create_view_data : gpointer;
        create_menus_data : gpointer;
        get_config_string_data : gpointer;
        set_label_data : gpointer;
     end;
   GNOME_MDI_GENERIC_CHILD = PGnomeMDIGenericChild;

   PGnomeMDIGenericChildClass = ^TGnomeMDIGenericChildClass;
   TGnomeMDIGenericChildClass = record
        parent_class : TGnomeMDIChildClass;
     end;
   GNOME_MDI_GENERIC_CHILD_CLASS = PGnomeMDIGenericChildClass;

function GNOME_TYPE_MDI_GENERIC_CHILD : TGTKType;
function GNOME_IS_MDI_GENERIC_CHILD(obj : Pointer) : Boolean;
function GNOME_IS_MDI_GENERIC_CHILD_CLASS(klass : Pointer) : Boolean;

function gnome_mdi_generic_child_get_type:TGTKType;cdecl;external libgnomeuidll name 'gnome_mdi_generic_child_get_type';
function gnome_mdi_generic_child_new(name:Pgchar):PGnomeMDIGenericChild;cdecl;external libgnomeuidll name 'gnome_mdi_generic_child_new';
procedure gnome_mdi_generic_child_set_view_creator(child:PGnomeMDIGenericChild; func:TGnomeMDIChildViewCreator; data:gpointer);cdecl;external libgnomeuidll name 'gnome_mdi_generic_child_set_view_creator';
procedure gnome_mdi_generic_child_set_view_creator_full(child:PGnomeMDIGenericChild; func:TGnomeMDIChildViewCreator; marshal:TGtkCallbackMarshal; data:gpointer; notify:TGtkDestroyNotify);cdecl;external libgnomeuidll name 'gnome_mdi_generic_child_set_view_creator_full';
procedure gnome_mdi_generic_child_set_menu_creator(child:PGnomeMDIGenericChild; func:TGnomeMDIChildMenuCreator; data:gpointer);cdecl;external libgnomeuidll name 'gnome_mdi_generic_child_set_menu_creator';
procedure gnome_mdi_generic_child_set_menu_creator_full(child:PGnomeMDIGenericChild; func:TGnomeMDIChildMenuCreator; marshal:TGtkCallbackMarshal; data:gpointer; notify:TGtkDestroyNotify);cdecl;external libgnomeuidll name 'gnome_mdi_generic_child_set_menu_creator_full';
procedure gnome_mdi_generic_child_set_config_func(child:PGnomeMDIGenericChild; func:TGnomeMDIChildConfigFunc; data:gpointer);cdecl;external libgnomeuidll name 'gnome_mdi_generic_child_set_config_func';
procedure gnome_mdi_generic_child_set_config_func_full(child:PGnomeMDIGenericChild; func:TGnomeMDIChildConfigFunc; marshal:TGtkCallbackMarshal; data:gpointer; notify:TGtkDestroyNotify);cdecl;external libgnomeuidll name 'gnome_mdi_generic_child_set_config_func_full';
procedure gnome_mdi_generic_child_set_label_func(child:PGnomeMDIGenericChild; func:TGnomeMDIChildLabelFunc; data:gpointer);cdecl;external libgnomeuidll name 'gnome_mdi_generic_child_set_label_func';
procedure gnome_mdi_generic_child_set_label_func_full(child:PGnomeMDIGenericChild; func:TGnomeMDIChildLabelFunc; marshal:TGtkCallbackMarshal; data:gpointer; notify:TGtkDestroyNotify);cdecl;external libgnomeuidll name 'gnome_mdi_generic_child_set_label_func_full';

{$EndIf read_interface}

{$Ifdef read_implementation}

function GNOME_TYPE_MDI_GENERIC_CHILD : TGTKType;
begin
  GNOME_TYPE_MDI_GENERIC_CHILD:=gnome_mdi_generic_child_get_type;
end;

function GNOME_IS_MDI_GENERIC_CHILD(obj : Pointer) : Boolean;
begin
   GNOME_IS_MDI_GENERIC_CHILD:=(obj<>nil) and GNOME_IS_MDI_GENERIC_CHILD_CLASS(PGtkTypeObject(obj)^.klass);
end;

function GNOME_IS_MDI_GENERIC_CHILD_CLASS(klass : Pointer) : Boolean;
begin
   GNOME_IS_MDI_GENERIC_CHILD_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GNOME_TYPE_MDI_GENERIC_CHILD);
end;

{$Endif read_implementation}

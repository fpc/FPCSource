// included by glib2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
{ --- typedefs & structures ---  }
type
   TGTypePluginUse = procedure (plugin:PGTypePlugin); cdecl;

   TGTypePluginUnuse = procedure (plugin:PGTypePlugin); cdecl;

   TGTypePluginCompleteTypeInfo = procedure (plugin:PGTypePlugin; g_type:GType; info:PGTypeInfo; value_table:PGTypeValueTable); cdecl;

   TGTypePluginCompleteInterfaceInfo = procedure (plugin:PGTypePlugin; instance_type:GType; interface_type:GType; info:PGInterfaceInfo); cdecl;
   PGTypePluginClass = ^TGTypePluginClass;
   TGTypePluginClass = record
        base_iface : TGTypeInterface;
        use_plugin : TGTypePluginUse;
        unuse_plugin : TGTypePluginUnuse;
        complete_type_info : TGTypePluginCompleteTypeInfo;
        complete_interface_info : TGTypePluginCompleteInterfaceInfo;
     end;

function G_TYPE_TYPE_PLUGIN : GType;
function G_TYPE_PLUGIN(inst : Pointer) : PGTypePlugin;
function G_TYPE_PLUGIN_CLASS(vtable : Pointer) : PGTypePluginClass;
function G_IS_TYPE_PLUGIN(inst : Pointer) : boolean;
function G_IS_TYPE_PLUGIN_CLASS(vtable : Pointer) : boolean;
function G_TYPE_PLUGIN_GET_CLASS(inst : Pointer) : PGTypePluginClass;


{ --- prototypes ---  }

function g_type_plugin_get_type:GType; cdecl; external gliblib;
procedure g_type_plugin_use(plugin:PGTypePlugin); cdecl; external gliblib;
procedure g_type_plugin_unuse(plugin:PGTypePlugin); cdecl; external gliblib;
procedure g_type_plugin_complete_type_info(plugin:PGTypePlugin; g_type:GType; info:PGTypeInfo; value_table:PGTypeValueTable); cdecl; external gliblib;
procedure g_type_plugin_complete_interface_info(plugin:PGTypePlugin; instance_type:GType; interface_type:GType; info:PGInterfaceInfo); cdecl; external gliblib;
{$ENDIF read_interface_rest}

// included by glib2.pas


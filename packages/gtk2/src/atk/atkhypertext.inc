// included by atk.pp

{
   The AtkHypertext interface provides standard  mechanisms for manipulating
   hyperlinks.
  }


{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PAtkHypertextIface = ^TAtkHypertextIface;
   TAtkHypertextIface = record
        parent : TGTypeInterface;
        get_link : function (hypertext:PAtkHypertext; link_index:gint):PAtkHyperlink; cdecl;
        get_n_links : function (hypertext:PAtkHypertext):gint; cdecl;
        get_link_index : function (hypertext:PAtkHypertext; char_index:gint):gint; cdecl;
        pad1 : TAtkFunction;
        pad2 : TAtkFunction;
        pad3 : TAtkFunction;
        pad4 : TAtkFunction;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function ATK_TYPE_HYPERTEXT : GType;
function ATK_IS_HYPERTEXT(obj: pointer) : boolean;
function ATK_HYPERTEXT(obj: pointer) : PAtkHypertext;
function ATK_HYPERTEXT_GET_IFACE(obj: pointer) : PAtkHypertextIface;


function atk_hypertext_get_type:GType; cdecl; external atklib;
function atk_hypertext_get_link(hypertext:PAtkHypertext; link_index:gint):PAtkHyperlink; cdecl; external atklib;
function atk_hypertext_get_n_links(hypertext:PAtkHypertext):gint; cdecl; external atklib;
function atk_hypertext_get_link_index(hypertext:PAtkHypertext; char_index:gint):gint; cdecl; external atklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function ATK_TYPE_HYPERTEXT : GType;
begin
   ATK_TYPE_HYPERTEXT:=atk_hypertext_get_type;
end;

function ATK_IS_HYPERTEXT(obj: pointer) : boolean;
begin
   ATK_IS_HYPERTEXT:=G_TYPE_CHECK_INSTANCE_TYPE(obj,ATK_TYPE_HYPERTEXT);
end;

function ATK_HYPERTEXT(obj: pointer) : PAtkHypertext;
begin
   ATK_HYPERTEXT:=PAtkHypertext(G_TYPE_CHECK_INSTANCE_CAST(obj,ATK_TYPE_HYPERTEXT));
end;

function ATK_HYPERTEXT_GET_IFACE(obj: pointer) : PAtkHypertextIface;
begin
   ATK_HYPERTEXT_GET_IFACE:=PAtkHypertextIface(G_TYPE_INSTANCE_GET_INTERFACE(obj,ATK_TYPE_HYPERTEXT));
end;

{$ENDIF read_implementation}


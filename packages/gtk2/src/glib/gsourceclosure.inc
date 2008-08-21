// included by glib2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
procedure g_source_set_closure(source:PGSource; closure:PGClosure); cdecl; external gliblib;
function g_io_channel_get_type:GType; cdecl; external gliblib;
function g_io_condition_get_type:GType; cdecl; external gliblib;

function G_TYPE_IO_CHANNEL : GType;
function G_TYPE_IO_CONDITION : GType;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function G_TYPE_IO_CHANNEL : GType;
begin
   G_TYPE_IO_CHANNEL:=g_io_channel_get_type;
end;

function G_TYPE_IO_CONDITION : GType;
begin
   G_TYPE_IO_CONDITION:=g_io_condition_get_type;
end;

{$ENDIF read_implementation}
// included by glib2.pas


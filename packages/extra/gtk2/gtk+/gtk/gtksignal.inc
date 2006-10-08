// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
{$ifndef GTK_DISABLE_DEPRECATED}
procedure gtk_signal_default_marshaller(closure:PGClosure; return_value:PGValue;
  n_param_values:GUInt; param_values:PGValue; invocation_hint:GPointer;
  marshal_data:GPointer); cdecl;

{ --- compat defines ---  }
function gtk_signal_lookup(name:Pgchar; object_type:GType):guint;
function gtk_signal_name(signal_id:guint):Pgchar;
procedure gtk_signal_emit_stop(instance:gpointer; signal_id:guint; detail:TGQuark);
function gtk_signal_connect(anObject:PGtkObject; name:Pgchar; func:TGtkSignalFunc;
                         func_data:gpointer):gulong;
function gtk_signal_connect_after(anObject:PGtkObject; name:Pgchar; func:TGtkSignalFunc;
                                  func_data:gpointer):gulong;
function gtk_signal_connect_object(anObject:PGtkObject; name:Pgchar; func:TGtkSignalFunc;
                                   slot_object:gpointer):gulong;
function gtk_signal_connect_object_after(anObject:PGtkObject; name:Pgchar; func:TGtkSignalFunc;
                                         slot_object:gpointer):gulong;
procedure gtk_signal_disconnect(anObject: gpointer; handler_id : gulong);
procedure gtk_signal_handler_block(anObject: gpointer; handler_id : gulong);
procedure gtk_signal_handler_unblock(anObject: gpointer; handler_id : gulong);

procedure gtk_signal_disconnect_by_func(anObject:PGtkObject;
                                          func: TGtkSignalFunc; data: gpointer);
procedure gtk_signal_disconnect_by_data(anObject:PGtkObject; data : gpointer);

procedure       gtk_signal_handler_block_by_func(anObject:PGtkObject;
  func: TGtkSignalFunc; data: gpointer);
procedure       gtk_signal_handler_block_by_data(anObject:PGtkObject; data : gpointer);
procedure       gtk_signal_handler_unblock_by_func(anObject:PGtkObject;
  func: TGtkSignalFunc; data: gpointer);
procedure       gtk_signal_handler_unblock_by_data(anObject:PGtkObject; data : gpointer);
function gtk_signal_handler_pending(anObject:PGtkObject; signal_id: guint;
  may_be_blocked: gboolean): gboolean;
function gtk_signal_handler_pending_by_func(anObject:PGtkObject;
  signal_id: guint; may_be_blocked: gboolean; func: TGtkSignalFunc;
  data: gpointer): gboolean;

{  --- compat functions ---  }
function gtk_signal_newv(name:Pgchar; signal_flags:TGtkSignalRunType; object_type:TGtkType; function_offset:guint; marshaller:TGtkSignalMarshaller;
           return_val:TGtkType; n_args:guint; args:PGtkType):guint; cdecl; external gtklib;
{$IFNDEF KYLIX}
function gtk_signal_new(name:Pgchar; signal_flags:TGtkSignalRunType; object_type:TGtkType; function_offset:guint; marshaller:TGtkSignalMarshaller;
           return_val:TGtkType; n_args:guint; args:array of const):guint; cdecl; overload; external gtklib;
function gtk_signal_new(name:Pgchar; signal_flags:TGtkSignalRunType; object_type:TGtkType; function_offset:guint; marshaller:TGtkSignalMarshaller;
           return_val:TGtkType; n_args:guint):guint; cdecl; varargs; overload; external gtklib;
{$ELSE}
function gtk_signal_new(name:Pgchar; signal_flags:TGtkSignalRunType; object_type:TGtkType; function_offset:guint; marshaller:TGtkSignalMarshaller;
           return_val:TGtkType; n_args:guint):guint; varargs; cdecl; external gtklib;
{$ENDIF}
procedure gtk_signal_emit_stop_by_name(anObject:PGtkObject; name:Pgchar); cdecl; external gtklib;
procedure gtk_signal_connect_object_while_alive(anObject:PGtkObject; name:Pgchar; func:TGtkSignalFunc; alive_object:PGtkObject); cdecl; external gtklib;
procedure gtk_signal_connect_while_alive(anObject:PGtkObject; name:Pgchar; func:TGtkSignalFunc; func_data:gpointer; alive_object:PGtkObject); cdecl; external gtklib;
function gtk_signal_connect_full(anObject:PGtkObject; name:Pgchar; func:TGtkSignalFunc; unsupported:TGtkCallbackMarshal; data:gpointer;
           destroy_func:TGtkDestroyNotify; object_signal:gint; after:gint):gulong; cdecl; external gtklib;
procedure gtk_signal_emitv(anObject:PGtkObject; signal_id:guint; args:PGtkArg); cdecl; external gtklib;
{$IFNDEF KYLIX}
procedure gtk_signal_emit(anObject:PGtkObject; signal_id:guint; args:array of const); cdecl; overload; external gtklib;
procedure gtk_signal_emit(anObject:PGtkObject; signal_id:guint); cdecl; overload; varargs; external gtklib;
procedure gtk_signal_emit_by_name(anObject:PGtkObject; name:Pgchar; args:array of const); cdecl; overload; external gtklib;
procedure gtk_signal_emit_by_name(anObject:PGtkObject; name:Pgchar); cdecl; overload; varargs; external gtklib;
{$ELSE}
procedure gtk_signal_emit(anObject:PGtkObject; signal_id:guint); varargs; cdecl; external gtklib;
procedure gtk_signal_emit_by_name(anObject:PGtkObject; name:Pgchar); varargs; cdecl; external gtklib;
{$ENDIF}
procedure gtk_signal_emitv_by_name(anObject:PGtkObject; name:Pgchar; args:PGtkArg); cdecl; external gtklib;
procedure gtk_signal_compat_matched(anObject:PGtkObject; func:TGtkSignalFunc; data:gpointer; match:TGSignalMatchType; action:guint); cdecl; external gtklib;
{$endif}
{ GTK_DISABLE_DEPRECATED  }

{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function gtk_signal_lookup(name:Pgchar; object_type:GType):guint;
begin
   gtk_signal_lookup:=g_signal_lookup(name,object_type);
end;

function gtk_signal_name(signal_id:guint):Pgchar;
begin
   gtk_signal_name:=g_signal_name(signal_id);
end;

procedure gtk_signal_emit_stop(instance:gpointer; signal_id:guint; detail:TGQuark);
begin
   if detail<>0 then ;// dummy statement
   g_signal_stop_emission(instance,signal_id,0);
end;

function gtk_signal_connect(anObject:PGtkObject; name:Pgchar; func:TGtkSignalFunc;
  func_data:gpointer):gulong;
begin
   gtk_signal_connect:=gtk_signal_connect_full(anObject,name,func,nil,func_data,nil,0,0);
end;

function gtk_signal_connect_after(anObject:PGtkObject; name:Pgchar; func:TGtkSignalFunc;
  func_data:gpointer):gulong;
begin
   gtk_signal_connect_after:=gtk_signal_connect_full(anObject,name,func,nil,func_data,nil,0,1);
end;

function gtk_signal_connect_object(anObject:PGtkObject; name:Pgchar; func:TGtkSignalFunc;
  slot_object:gpointer):gulong;
begin
   gtk_signal_connect_object:=gtk_signal_connect_full(anObject,name,func,nil,slot_object,nil,1,0);
end;

function gtk_signal_connect_object_after(anObject:PGtkObject; name:Pgchar; func:TGtkSignalFunc;
  slot_object:gpointer):gulong;
begin
   gtk_signal_connect_object_after:=gtk_signal_connect_full(anObject,name,func,nil,slot_object,nil,1,1);
end;

procedure gtk_signal_disconnect(anObject: gpointer; handler_id : gulong);
begin
   g_signal_handler_disconnect(anObject,handler_id);
end;

procedure gtk_signal_handler_block(anObject: gpointer; handler_id : gulong);
begin
   g_signal_handler_block(anObject,handler_id);
end;

procedure gtk_signal_handler_unblock(anObject: gpointer; handler_id : gulong);
begin
   g_signal_handler_unblock(anObject,handler_id);
end;

procedure gtk_signal_disconnect_by_data(anObject:PGtkObject; data : gpointer);
begin
   gtk_signal_compat_matched(anObject,nil,data,G_SIGNAL_MATCH_DATA,0);
end;

procedure gtk_signal_disconnect_by_func(anObject:PGtkObject;
  func: TGtkSignalFunc; data: gpointer);
begin
  gtk_signal_compat_matched (anObject,func,data,
    TGSignalMatchType(G_SIGNAL_MATCH_FUNC or G_SIGNAL_MATCH_DATA),0);
end;

procedure       gtk_signal_handler_block_by_func(anObject:PGtkObject;
  func: TGtkSignalFunc; data: gpointer);
begin
  gtk_signal_compat_matched (anObject,func,data,
               TGSignalMatchType(G_SIGNAL_MATCH_FUNC or G_SIGNAL_MATCH_DATA),0);
end;

procedure       gtk_signal_handler_block_by_data(anObject:PGtkObject; data : gpointer);
begin
  gtk_signal_compat_matched(anObject,nil,data,G_SIGNAL_MATCH_DATA,1);
end;

procedure       gtk_signal_handler_unblock_by_func(anObject:PGtkObject;
  func: TGtkSignalFunc; data: gpointer);
begin
  gtk_signal_compat_matched(anObject,func,data,
        TGSignalMatchType(G_SIGNAL_MATCH_FUNC or G_SIGNAL_MATCH_DATA),0);
end;

procedure       gtk_signal_handler_unblock_by_data(anObject:PGtkObject; data : gpointer);
begin
  gtk_signal_compat_matched(anObject,nil,data,G_SIGNAL_MATCH_DATA,2);
end;

function gtk_signal_handler_pending(anObject:PGtkObject; signal_id: guint;
  may_be_blocked: gboolean): gboolean;
begin
  Result:=g_signal_has_handler_pending(anObject,signal_id,0,may_be_blocked);
end;

function gtk_signal_handler_pending_by_func(anObject:PGtkObject;
  signal_id: guint; may_be_blocked: gboolean; func: TGtkSignalFunc;
  data: gpointer): gboolean;
var
  t: TGSignalMatchType;
begin
  t:=TGSignalMatchType(G_SIGNAL_MATCH_ID or G_SIGNAL_MATCH_FUNC
                      or G_SIGNAL_MATCH_DATA);
  if not may_be_blocked then
    t:=t or TGSignalMatchType(G_SIGNAL_MATCH_UNBLOCKED);
  Result := g_signal_handler_find(anObject,t,signal_id,0,nil,@func,data)<>0;
end;


{$ifndef GTK_DISABLE_DEPRECATED}
procedure gtk_signal_default_marshaller(closure:PGClosure; return_value:PGValue;
  n_param_values:GUInt; param_values:PGValue; invocation_hint:GPointer;
  marshal_data:GPointer); cdecl;
begin
  g_cclosure_marshal_VOID__VOID(closure,return_value,
    n_param_values,param_values,invocation_hint,
    marshal_data);
end;
{$endif GTK_DISABLE_DEPRECATED}

{$ENDIF read_implementation}
// included by gtk2.pas


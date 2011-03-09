
{$IFDEF OGC_INTERFACE}

function LWP_OBJMASKTYPE(typ: cuint32): cuint32; inline;//		((typ)<<16)
function LWP_OBJMASKID(id: cuint32): cuint32; inline;//			((id)&0xffff)
function LWP_OBJTYPE(id: cuint32): cuint32; inline;//				((id)>>16)


type
//  _lwp_objinfo = record
//  end;
  Plwp_objinfo = ^_lwp_objinfo;

  _lwp_obj = record
    node : lwp_node;
    id : cint32;
    information : Plwp_objinfo;
  end;
  lwp_obj = _lwp_obj;
  Plwp_obj = ^_lwp_obj;
  PPlwp_obj = ^Plwp_obj;


  _lwp_objinfo = record
    min_id : cuint32;
    max_id : cuint32;
    max_nodes : cuint32;
    node_size : cuint32;
    local_table : PPlwp_obj;
    obj_blocks : pointer;
    inactives : lwp_queue;
    inactives_cnt : cuint32;
  end;
  lwp_objinfo = _lwp_objinfo;


procedure __lwp_objmgr_initinfo(info: Plwp_objinfo;
  max_nodes, node_size: cuint32); cdecl; external;

procedure __lwp_objmgr_free(info: Plwp_objinfo; object_: Plwp_obj); cdecl; external;

function __lwp_objmgr_allocate(info: Plwp_objinfo): Plwp_obj; cdecl; external;

function __lwp_objmgr_get(info: Plwp_objinfo; id: cuint32): Plwp_obj; cdecl; external;

function __lwp_objmgr_getisrdisable(info: Plwp_objinfo; id: cuint32;
  p_level: pcuint32): Plwp_obj; cdecl; external;

function __lwp_objmgr_getnoprotection(info: Plwp_objinfo; id: cuint32): Plwp_obj; cdecl; external;

{$ENDIF}

{$IFDEF OGC_IMPLEMENTATION}
function LWP_OBJMASKTYPE(typ: cuint32): cuint32; inline;
begin
  result := ((typ) shl 16);
end;

function LWP_OBJMASKID(id: cuint32): cuint32; inline;
begin
  result := ((id) and $ffff);
end;

function LWP_OBJTYPE(id: cuint32): cuint32; inline;
begin
  result := ((id) shr 16);
end;


//{$ifdef LIBOGC_INTERNAL}
//{$include libogc/lwp_objmgr.inl}
//{$endif}

{$ENDIF}

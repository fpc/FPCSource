
{$IFDEF OGC_INTERFACE}
{$ifdef _LWPQ_DEBUG}
function printk(fmt: pcchar; par1: array of const): cint; cdecl; external;

{$endif _LWPQ_DEBUG}

type
  P_lwpnode = ^_lwpnode;
  _lwpnode = record
    next : P_lwpnode;
    prev : P_lwpnode;
  end;
  lwp_node = _lwpnode;

  Plwp_node = ^lwp_node;
  _lwpqueue = record
    first : Plwp_node;
    perm_null : Plwp_node;
    last : Plwp_node;
  end;
  lwp_queue = _lwpqueue;
  Plwp_queue = ^lwp_queue;


procedure __lwp_queue_initialize(par0: Plwp_queue; par1: pointer;
  par2, par3: cuint32); cdecl; external;

function __lwp_queue_get(par0: Plwp_queue): Plwp_node; cdecl; external;

procedure __lwp_queue_append(par0: Plwp_queue; par1: Plwp_node); cdecl; external;

procedure __lwp_queue_extract(par0: Plwp_node); cdecl; external;

procedure __lwp_queue_insert(par0, par1: Plwp_node); cdecl; external;


//#ifdef LIBOGC_INTERNAL
//#include <libogc/lwp_queue.inl>
//#endif
{$ENDIF OGC_INTERFACE}


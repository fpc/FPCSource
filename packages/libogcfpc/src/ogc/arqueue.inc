
{$IFDEF OGC_INTERFACE}
const
  ARQ_MRAMTOARAM = AR_MRAMTOARAM;
  ARQ_ARAMTOMRAM = AR_ARAMTOMRAM;
  ARQ_DEF_CHUNK_SIZE = 4096;
  ARQ_PRIO_LO = 0;
  ARQ_PRIO_HI = 1;

  ARQ_TASK_READY = 0;
  ARQ_TASK_RUNNING = 1;
  ARQ_TASK_FINISHED = 2;


type
  ARQCallback = procedure(par0: pointer); cdecl;


type
  _arq_request = record
    node : lwp_node;
    owner : cuint32;
    dir : cuint32;
    prio : cuint32;
    state : cuint32;
    aram_addr : cuint32;
    mram_addr : cuint32;
    len : cuint32;
    callback : ARQCallback;
  end;
  ARQRequest = _arq_request;
  PARQRequest = ^ARQRequest;


procedure ARQ_Init; cdecl; external;

procedure ARQ_Reset; cdecl; external;


procedure ARQ_PostRequest(req: PARQRequest;
  owner, dir, prio, aram_addr, mram_addr, len: cuint32); cdecl; external;

procedure ARQ_PostRequestAsync(req: PARQRequest;
  owner, dir, prio, aram_addr, mram_addr, len: cuint32; cb: ARQCallback); cdecl; external;

procedure ARQ_RemoveRequest(req: PARQRequest); cdecl; external;

procedure ARQ_SetChunkSize(size: cuint32); cdecl; external;

function ARQ_GetChunkSize: cuint32; cdecl; external;

procedure ARQ_FlushQueue; cdecl; external;

function ARQ_RemoveOwnerRequest(owner: cuint32): cuint32; cdecl; external;


{$ENDIF OGC_INTERFACE}

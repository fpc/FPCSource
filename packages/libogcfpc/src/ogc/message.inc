
{$IFDEF OGC_INTERFACE}
const
  MQ_BOX_NULL = $ffffffff;
  MQ_ERROR_SUCCESSFUL = 0;
  MQ_ERROR_TOOMANY = - 5;
  MQ_MSG_BLOCK = 0;
  MQ_MSG_NOBLOCK = 1;

type
  mqbox_t = cuint32;
  Pmqbox_t = ^mqbox_t;

  mqmsg_t = pointer;
  pmqmsg_t = ^mqmsg_t;


function MQ_Init(mqbox: Pmqbox_t; count: cuint32): cint32; cdecl; external;

procedure MQ_Close(mqbox: mqbox_t); cdecl; external;

function MQ_Send(mqbox: mqbox_t; msg: mqmsg_t; flags: cuint32): cbool; cdecl; external;

function MQ_Jam(mqbox: mqbox_t; msg: mqmsg_t; flags: cuint32): cbool; cdecl; external;

function MQ_Receive(mqbox: mqbox_t; msg: Pmqmsg_t; flags: cuint32): cbool; cdecl; external;
{$ENDIF OGC_INTERFACE}


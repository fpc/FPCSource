{$ifdef OGC_INTERFACE}

type
  ARQMCallback = procedure(result: cint32); cdecl;


procedure ARQM_Init(arambase: cuint32; len: cint32); cdecl; external;

function ARQM_PushData(buffer: pointer; len: cint32): cuint32; cdecl; external;

function ARQM_GetZeroBuffer: cuint32; cdecl; external;

function ARQM_GetStackPointer: cuint32; cdecl; external;

function ARQM_GetFreeSize: cuint32; cdecl; external;

{$endif OGC_INTERFACE}

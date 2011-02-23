{$ifdef OGC_INTERFACE}

const
  AR_MRAMTOARAM = 0;
  AR_ARAMTOMRAM = 1;
  AR_ARAMINTALL = 0;
  AR_ARAMINTUSER = 1;

type
  ARCallback = procedure(value: pointer);
  PARCallback = ^ARCallback;

function AR_RegisterCallback(callback: ARCallback): ARCallback; cdecl; external;


function AR_GetDMAStatus(): cuint32; cdecl; external;
function AR_Init(stack_idx_array: pcuint32; num_entries: cuint32): cuint32; cdecl; external;
procedure AR_StartDMA(dir, memaddr, aramaddr, len: cuint32); cdecl; external;
function AR_Alloc(len: cuint32): cuint32; cdecl; external;
function AR_Free(len: pcuint32): cuint32; cdecl; external;
procedure AR_Clear(flag: cuint32); cdecl; external;
function AR_CheckInit(): cbool; cdecl; external;
procedure AR_Reset(); cdecl; external;
function AR_GetSize(): cuint32; cdecl; external;
function AR_GetBaseAddress(): cuint32; cdecl; external;
function AR_GetInternalSize(): cuint32; cdecl; external;

procedure AR_StartDMARead(maddr, araddr, tlen: cuint32); inline;
procedure AR_StartDMAWrite(maddr, araddr, tlen: cuint32); inline;

{$endif OGC_INTERFACE}


{$ifdef OGC_IMPLEMENTATION}
procedure AR_StartDMARead(maddr, araddr, tlen: cuint32); inline;
begin
	AR_StartDMA(AR_ARAMTOMRAM, maddr, araddr, tlen);
end;

procedure AR_StartDMAWrite(maddr, araddr, tlen: cuint32); inline;
begin
	AR_StartDMA(AR_MRAMTOARAM, maddr, araddr, tlen);
end;
{$endif OGC_IMPLEMENTATION}

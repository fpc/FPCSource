{$IFDEF OGC_INTERFACE}
type
  VIRetraceCallback = procedure(retraceCnt: cuint32); cdecl;
  VIPositionCallback = procedure(posX, posY: cuint32); cdecl;


function VIDEO_GetNextFramebuffer: pointer; cdecl; external;

function VIDEO_GetCurrentFramebuffer: pointer; cdecl; external;



procedure VIDEO_Init; cdecl; external;

procedure VIDEO_Flush; cdecl; external;

procedure VIDEO_SetBlack(black: cbool); cdecl; external;

function VIDEO_GetNextField: cuint32; cdecl; external;

function VIDEO_GetCurrentLine: cuint32; cdecl; external;

function VIDEO_GetCurrentTvMode: cuint32; cdecl; external;


procedure VIDEO_Configure(rmode: PGXRModeObj); cdecl; external;


function VIDEO_GetFrameBufferSize(rmode: PGXRModeObj): cuint32; cdecl; external;


procedure VIDEO_ClearFrameBuffer(rmode: PGXRModeObj; fb: pointer;
  color: cuint32); cdecl; external;


procedure VIDEO_WaitVSync; cdecl; external;


procedure VIDEO_SetNextFramebuffer(fb: pointer); cdecl; external;


procedure VIDEO_SetNextRightFramebuffer(fb: pointer); cdecl; external;


function VIDEO_SetPreRetraceCallback(callback: VIRetraceCallback)
  : VIRetraceCallback; cdecl; external;


function VIDEO_SetPostRetraceCallback(callback: VIRetraceCallback)
  : VIRetraceCallback; cdecl; external;


function VIDEO_HaveComponentCable: cuint32; cdecl; external;


function VIDEO_GetPreferredMode(mode: PGXRModeObj): PGXRModeObj; cdecl; external;
{$ENDIF OGC_INTERFACE}




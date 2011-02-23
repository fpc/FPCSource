{$IFDEF OGC_INTERFACE}
procedure CON_Init(framebuffer: pointer;
  xstart, ystart, xres, yres, stride: cint); cdecl; external;
function CON_InitEx(rmode: PGXRModeObj;
  conXOrigin, conYOrigin, conWidth, conHeight: cint32): cint32; cdecl;  external;
procedure CON_GetMetrics(cols, rows: pcint); cdecl; external;
procedure CON_GetPosition(cols, rows: pcint); cdecl; external;
procedure CON_EnableGecko(channel, safe: cint); cdecl; external;

// Function aliases 
procedure console_init(framebuffer: pointer;
  xstart, ystart, xres, yres, stride: cint); cdecl; external name 'CON_Init';
function SYS_ConsoleInit(rmode: PGXRModeObj;
  conXOrigin, conYOrigin, conWidth, conHeight: cint32): cint32; cdecl;  external name 'CON_InitEx';

{$ENDIF}



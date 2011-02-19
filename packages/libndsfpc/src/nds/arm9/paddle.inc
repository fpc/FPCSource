{$ifdef NDS_INTERFACE}
function paddleIsInserted(): cbool; cdecl; external;
function paddleRead(): cuint16; cdecl; external;
procedure paddleReset(); cdecl; external;

{$endif NDS_INTERFACE}

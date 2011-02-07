{$ifdef NDS_INTERFACE}
  
procedure scanKeys(); cdecl; external;

function keysCurrent(): cuint32; cdecl; external;

function keysHeld(): cuint32; cdecl; external;

function keysDown(): cuint32; cdecl; external;

function keysDownRepeat(): cuint32; cdecl; external;

procedure keysSetRepeat(setDelay: cuint8; setRepeat: cuint8); cdecl; external;

function keysUp(): cuint32; cdecl; external;

function touchReadXY(): touchPosition; cdecl; external;

procedure touchRead(var data: touchPosition); cdecl; external;
{$endif NDS_INTERFACE}

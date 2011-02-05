{$ifdef NDS_INTERFACE}


const
  GUITARGRIP_GREEN   = (1 shl 6);
  GUITARGRIP_RED     = (1 shl 5);
  GUITARGRIP_YELLOW  = (1 shl 4);
  GUITARGRIP_BLUE    = (1 shl 3);

function guitarGripIsInserted(): cbool; cdecl; external;
procedure guitarGripScanKeys(); cdecl; external;

function guitarGripKeysHeld(): cuint8; cdecl; external;
function guitarGripKeysDown(): cuint8; cdecl; external;
function guitarGripKeysUp(): cuint8; cdecl; external;

{$endif NDS_INTERFACE}

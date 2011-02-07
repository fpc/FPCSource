{$ifdef NDS_INTERFACE}
const
  PIANO_PAK: pcuint16 = pointer($09FFFFFE);
  PIANO_C   = (1 shl 0);
  PIANO_CS  = (1 shl 1);
  PIANO_D   = (1 shl 2);
  PIANO_DS  = (1 shl 3);
  PIANO_E   = (1 shl 4);
  PIANO_F   = (1 shl 5);
  PIANO_FS  = (1 shl 6);
  PIANO_G   = (1 shl 7);
  PIANO_GS  = (1 shl 8);
  PIANO_A   = (1 shl 9);
  PIANO_AS  = (1 shl 10);
  PIANO_B   = (1 shl 13);
  PIANO_C2  = (1 shl 14);

function pianoIsInserted(): cbool; cdecl; external;
procedure pianoScanKeys(); cdecl; external;

function pianoKeysHeld(): cuint16; cdecl; external;
function pianoKeysDown(): cuint16; cdecl; external;
function pianoKeysUp(): cuint16; cdecl; external;
{$endif NDS_INTERFACE}
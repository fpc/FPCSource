{$ifdef NDS_INTERFACE}
const
  RUMBLE_PAK: pcuint16 = pointer($08000000);
	WARIOWARE_PAK:		pcuint16 = pointer($080000C4);
	WARIOWARE_ENABLE:	pcuint16 = pointer($080000C6);
 
type 
  RUMBLE_TYPE = (
	  RUMBLE, WARIOWARE
	);
	TRumbleType = RUMBLE_TYPE;
  

function isRumbleInserted(): cbool; cdecl; external;
procedure setRumble(position: cbool); cdecl; external;
{$endif NDS_INTERFACE}

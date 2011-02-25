{$IFDEF OGC_INTERFACE}
const
  DEVICE_TYPE_WII_SD = ((Ord('W') shl 2) or (Ord('I') shl 16) or (Ord('S') shl 8) or Ord('D'));

var
  __io_wiisd : DISC_INTERFACE; external;
{$ENDIF}



{$IFDEF OGC_INTERFACE}
const
  DEVICE_TYPE_GC_SD = ((Ord('G') shl 2) or (Ord('C') shl 16) or (Ord('S') shl 8) or Ord('D'));

var
  __io_gcsda : DISC_INTERFACE; external;
  __io_gcsdb : DISC_INTERFACE; external;
{$ENDIF}


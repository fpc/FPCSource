{$IFDEF OGC_INTERFACE}
type
  _gx_rmodeobj = record
    viTVMode : cuint32;
    fbWidth : cuint16;
    efbHeight : cuint16;
    xfbHeight : cuint16;
    viXOrigin : cuint16;
    viYOrigin : cuint16;
    viWidth : cuint16;
    viHeight : cuint16;
    xfbMode : cuint32;
    field_rendering : cuint8;
    aa : cuint8;
    sample_pattern : array [0..11, 0..1] of cuint8;
    vfilter : array [0..6] of cuint8;
  end;
  GXRModeObj = _gx_rmodeobj;
  PGXRModeObj = ^GXRModeObj;
{$ENDIF OGC_INTERFACE}




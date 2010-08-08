{$ifdef NDS_INTERFACE}

type
  touchPosition = packed record
    rawx: cuint16;
    rawy: cuint16;
    px: cuint16;
    py: cuint16;
    z1: cuint16;
    z2: cuint16; 
  end;
  PtouchPosition = ^touchPosition; 
  TtouchPosition = touchPosition;
   
{$endif NDS_INTERFACE}

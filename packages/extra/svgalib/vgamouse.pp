unit vgamouse;

  interface

{$linklib vga}
{$linklib c}

  Type 
    PLongint = ^Longint;

  const
     MOUSE_MICROSOFT = 0;
     MOUSE_MOUSESYSTEMS = 1;
     MOUSE_MMSERIES = 2;
     MOUSE_LOGITECH = 3;
     MOUSE_BUSMOUSE = 4;
     MOUSE_PS2 = 5;
     MOUSE_LOGIMAN = 6;
     MOUSE_GPM = 7;
     MOUSE_SPACEBALL = 8;
     MOUSE_ORIENTATION_VERTICAL = 0;
     MOUSE_ORIENTATION_HORIZONTAL = 1;

     MOUSE_CHG_DTR = $80000000;
     MOUSE_DTR_HIGH = $40000000;
     MOUSE_CHG_RTS = $20000000;
     MOUSE_RTS_HIGH = $10000000;
     MOUSE_TYPE_MASK = $ffff;
     MOUSE_LEFTBUTTON = 4;
     MOUSE_MIDDLEBUTTON = 2;
     MOUSE_RIGHTBUTTON = 1;
     MOUSE_FOURTHBUTTON = 8;
     MOUSE_FIFTHBUTTON = 16;
     MOUSE_SIXTHBUTTON = 32;
     MOUSE_RESETBUTTON = 64;
     MOUSE_XDIM = 1;
     MOUSE_YDIM = 2;
     MOUSE_ZDIM = 4;
     MOUSE_RXDIM = 8;
     MOUSE_RYDIM = 16;
     MOUSE_RZDIM = 32;
     MOUSE_2DIM = 3;
     MOUSE_3DIM = 7;
     MOUSE_6DIM = 63;
     MOUSE_DEFAULTSAMPLERATE = 150;

  function mouse_init(dev:pchar; thetype:longint; samplerate:longint):longint;cdecl;
  function mouse_init_return_fd(dev:pchar; thetype:longint; samplerate:longint):longint;cdecl;

  type

     __mouse_handler = procedure (button:longint; dx:longint; dy:longint; dz:longint; drx:longint; 
                   dry:longint; drz:longint);CDECL;

  procedure mouse_seteventhandler(handler:__mouse_handler);cdecl;

  procedure mouse_close;cdecl;

  function mouse_update:longint;cdecl;

  procedure mouse_waitforupdate;cdecl;

  const
     MOUSE_NOWRAP = 0;
     MOUSE_WRAPX = 1;
     MOUSE_WRAPY = 2;
     MOUSE_WRAPZ = 4;
     MOUSE_WRAPRX = 8;
     MOUSE_WRAPRY = 16;
     MOUSE_WRAPRZ = 32;
     MOUSE_WRAP = 63;
     
     MOUSE_ROT_COORDS = 196;
     MOUSE_ROT_INFINITESIMAL = 0;
     MOUSE_ROT_RX_RY_RZ = 64;
     MOUSE_ROT_ZXZ = 128;
     MOUSE_ROT_YPR = 196;

  procedure mouse_setdefaulteventhandler;cdecl;
  procedure mouse_setposition(x:longint; y:longint);cdecl;
  procedure mouse_setposition_6d(x:longint; y:longint; z:longint; rx:longint; ry:longint; 
              rz:longint; dim_mask:longint);cdecl;
  procedure mouse_setxrange(x1:longint; x2:longint);cdecl;
  procedure mouse_setyrange(y1:longint; y2:longint);cdecl;
  procedure mouse_setrange_6d(x1:longint; x2:longint; y1:longint; y2:longint; z1:longint; 
              z2:longint; rx1:longint; rx2:longint; ry1:longint; ry2:longint; 
              rz1:longint; rz2:longint; dim_mask:longint);cdecl;
  procedure mouse_setscale(s:longint);cdecl;
  procedure mouse_setwrap(w:longint);cdecl;
  function mouse_getx:longint;cdecl;
  function mouse_gety:longint;cdecl;
  procedure mouse_getposition_6d(x:plongint; y:plongint; z:plongint; rx:plongint; ry:plongint; 
              rz:plongint);cdecl;
  function mouse_getbutton:longint;cdecl;


  implementation

  function mouse_init(dev:pchar; thetype:longint; samplerate:longint):longint;CDECL; external;
  function mouse_init_return_fd(dev:pchar; thetype:longint; samplerate:longint):longint;CDECL; external;
  procedure mouse_seteventhandler(handler:__mouse_handler);CDECL; external;
  procedure mouse_close;CDECL; external;
  function mouse_update:longint;CDECL; external;
  procedure mouse_waitforupdate;CDECL; external;
  procedure mouse_setdefaulteventhandler;CDECL; external;
  procedure mouse_setposition(x:longint; y:longint);CDECL; external;
  procedure mouse_setposition_6d(x:longint; y:longint; z:longint; rx:longint; ry:longint; 
              rz:longint; dim_mask:longint);CDECL; external;
  procedure mouse_setxrange(x1:longint; x2:longint);CDECL; external;
  procedure mouse_setyrange(y1:longint; y2:longint);CDECL; external;
  procedure mouse_setrange_6d(x1:longint; x2:longint; y1:longint; y2:longint; z1:longint; 
              z2:longint; rx1:longint; rx2:longint; ry1:longint; ry2:longint; 
              rz1:longint; rz2:longint; dim_mask:longint);CDECL; external;
  procedure mouse_setscale(s:longint);CDECL; external;
  procedure mouse_setwrap(w:longint);CDECL; external;
  function mouse_getx:longint;CDECL; external;
  function mouse_gety:longint;CDECL; external;
  procedure mouse_getposition_6d(x:plongint; y:plongint; z:plongint; rx:plongint; ry:plongint; 
              rz:plongint);CDECL; external;
  function mouse_getbutton:longint;CDECL; external;

end.
  $Log$
  Revision 1.1  2002-01-29 17:55:22  peter
    * splitted to base and extra

  Revision 1.2  2000/07/13 11:33:31  michael
  + removed logs
 
}

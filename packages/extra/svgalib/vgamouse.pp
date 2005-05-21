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

  function mouse_init(dev:pchar; thetype:longint; samplerate:longint):longint;cdecl; external;
  function mouse_init_return_fd(dev:pchar; thetype:longint; samplerate:longint):longint;cdecl; external;

  type

     __mouse_handler = procedure (button:longint; dx:longint; dy:longint; dz:longint; drx:longint;
                   dry:longint; drz:longint);cdecl;

  procedure mouse_seteventhandler(handler:__mouse_handler);cdecl; external;

  procedure mouse_close;cdecl; external;

  function mouse_update:longint;cdecl; external;

  procedure mouse_waitforupdate;cdecl; external;

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

  procedure mouse_setdefaulteventhandler;cdecl; external;
  procedure mouse_setposition(x:longint; y:longint);cdecl; external;
  procedure mouse_setposition_6d(x:longint; y:longint; z:longint; rx:longint; ry:longint;
              rz:longint; dim_mask:longint);cdecl; external;
  procedure mouse_setxrange(x1:longint; x2:longint);cdecl; external;
  procedure mouse_setyrange(y1:longint; y2:longint);cdecl; external;
  procedure mouse_setrange_6d(x1:longint; x2:longint; y1:longint; y2:longint; z1:longint;
              z2:longint; rx1:longint; rx2:longint; ry1:longint; ry2:longint;
              rz1:longint; rz2:longint; dim_mask:longint);cdecl; external;
  procedure mouse_setscale(s:longint);cdecl; external;
  procedure mouse_setwrap(w:longint);cdecl; external;
  function mouse_getx:longint;cdecl; external;
  function mouse_gety:longint;cdecl; external;
  procedure mouse_getposition_6d(x:plongint; y:plongint; z:plongint; rx:plongint; ry:plongint;
              rz:plongint);cdecl; external;
  function mouse_getbutton:longint;cdecl; external;


implementation

end.

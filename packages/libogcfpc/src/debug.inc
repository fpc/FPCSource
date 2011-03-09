{$ifdef OGC_INTERFACE}

const
  GDBSTUB_DEVICE_USB = 0;
  GDBSTUB_DEVICE_TCP = 1;

  GDBSTUB_DEF_CHANNEL = 0;
  GDBSTUB_DEF_TCPPORT = 2828;

var
  tcp_localip: pcchar; cvar; external;
  tcp_netmask: pcchar; cvar; external;
  tcp_gateway: pcchar; cvar; external;


procedure _break(); cdecl; external;	


procedure DEBUG_Init(device_type, channel_port: cint32); cdecl; external;

{$endif OGC_INTERFACE}

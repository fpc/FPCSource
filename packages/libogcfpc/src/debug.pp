{$IFNDEF FPC_DOTTEDUNITS}
unit debug;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc} 
{$J+}
{$INLINE ON}
{$MACRO ON}
{$ASSERTIONS ON}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.CTypes, WiiApi.Gctypes;
{$ELSE FPC_DOTTEDUNITS}
uses
  ctypes, gctypes;
{$ENDIF FPC_DOTTEDUNITS}
  
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

implementation

initialization
{$linklib db} 
end.

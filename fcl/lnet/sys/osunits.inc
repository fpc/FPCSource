{$ifdef WINDOWS}
  Winsock2,
{$else}
  BaseUnix, NetDB,
{$endif}
  SysUtils, Sockets;


{$ifdef WINDOWS}
  Winsock2,
{$endif}

{$ifdef UNIX}
  BaseUnix, NetDB,
{$endif}

{$ifdef NETWARE}
  WinSock,
{$endif}

{$ifdef OS2}
  WinSock,
{$endif}

{$ifdef NETWLIBC}
  WinSock,
{$endif}

  SysUtils, Sockets;


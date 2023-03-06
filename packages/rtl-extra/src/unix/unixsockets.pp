{$IFNDEF FPC_DOTTEDUNITS}
unit unixsockets;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}{$H+}

{$packrecords C}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.CTypes, UnixApi.Base;
{$ELSE FPC_DOTTEDUNITS}
uses
  cTypes, BaseUnix;
{$ENDIF FPC_DOTTEDUNITS}
  
{$unixsocketsh.inc}

implementation

{$unixsockets.inc}

end.


{$IFNDEF FPC_DOTTEDUNITS}
unit jackintclient;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$packrecords C}

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.CTypes, Api.Jack;
{$ELSE FPC_DOTTEDUNITS}
  ctypes, jack;
{$ENDIF FPC_DOTTEDUNITS}

{$I intclient.inc}

implementation

end.


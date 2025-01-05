{$IFNDEF FPC_DOTTEDUNITS}
unit jackuuid;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$packrecords C}

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.CTypes, Api.Jack;
{$ELSE FPC_DOTTEDUNITS}
  ctypes, jack;
{$ENDIF FPC_DOTTEDUNITS}

{$I uuid.inc}

implementation

end.


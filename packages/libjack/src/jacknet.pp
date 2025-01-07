{$IFNDEF FPC_DOTTEDUNITS}
unit jacknet;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$packrecords C}

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.CTypes, Api.Jack;
{$ELSE FPC_DOTTEDUNITS}
  ctypes, jack;
{$ENDIF FPC_DOTTEDUNITS}

const
  libjacknet = 'jacknet';

type
  PPcfloat = ^PCfloat;

{$I net.inc}

implementation

end.


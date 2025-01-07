{$IFNDEF FPC_DOTTEDUNITS}
unit jackcontrol;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$packrecords C}

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.CTypes, Api.Jack, Api.Jack.JsList;
{$ELSE FPC_DOTTEDUNITS}
  ctypes, jack, jackjslist;
{$ENDIF FPC_DOTTEDUNITS}

const
  libjackserver = 'jackserver';

type
  cbool = ByteBool;

{$I control.inc}

implementation

end.


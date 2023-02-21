{$IFNDEF FPC_DOTTEDUNITS}
unit winprocs;
{$ENDIF FPC_DOTTEDUNITS}

{$if defined(FPC_MM_COMPACT) or defined(FPC_MM_LARGE) or defined(FPC_MM_HUGE)}
  {$define VAR_PARAMS_ARE_FAR}
{$endif}

{$MODE objfpc}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  WinApi.WinTypes;
{$ELSE FPC_DOTTEDUNITS}
uses
  wintypes;
{$ENDIF FPC_DOTTEDUNITS}

{$I winprocsh.inc}

implementation

{$I winprocs.inc}

end.

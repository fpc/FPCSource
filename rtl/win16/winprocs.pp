unit winprocs;

{$if defined(FPC_MM_COMPACT) or defined(FPC_MM_LARGE) or defined(FPC_MM_HUGE)}
  {$define VAR_PARAMS_ARE_FAR}
{$endif}

{$MODE objfpc}

interface

uses
  wintypes;

{$I winprocsh.inc}

implementation

{$I winprocs.inc}

end.

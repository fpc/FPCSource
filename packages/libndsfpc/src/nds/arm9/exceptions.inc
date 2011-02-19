{$ifdef NDS_INTERFACE}
const
  EXCEPTION_VECTOR : ^VoidFn = pointer($2FFFD9C);

var
  exceptionC: ^VoidFn; cvar; external;
  exceptionStack: cuint32; cvar; external;
  exceptionRegisters: pcint32; cvar; external;

procedure enterException(); cdecl; external;
procedure setExceptionHandler(handler: VoidFn); cdecl; external;
procedure defaultExceptionHandler(); cdecl; external;

function getCPSR(): cuint32; cdecl; external;
{$endif NDS_INTERFACE}

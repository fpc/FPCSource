
{$IFDEF OGC_INTERFACE}
const
  SYS_STATE_BEFORE_INIT = 0;
  SYS_STATE_BEFORE_MT = 1;
  SYS_STATE_BEGIN_MT = 2;
  SYS_STATE_UP = 3;
  SYS_STATE_SHUTDOWN = 4;
  SYS_STATE_FAILED = 5;

var
  _sys_state_curr : cuint32; external;


{$ifdef LIBOGC_INTERNAL}
  {$include libogc/sys_state.inl}
{$endif}
{$ENDIF}



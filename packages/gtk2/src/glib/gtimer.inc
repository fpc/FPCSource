// included by glib2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
  PGTimer = pointer;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}

{ microseconds per second  }
const
   G_USEC_PER_SEC = 1000000;

function g_timer_new:PGTimer; cdecl; external gliblib;
procedure g_timer_destroy(timer:PGTimer); cdecl; external gliblib;
procedure g_timer_start(timer:PGTimer); cdecl; external gliblib;
procedure g_timer_stop(timer:PGTimer); cdecl; external gliblib;
procedure g_timer_reset(timer:PGTimer); cdecl; external gliblib;
function g_timer_elapsed(timer:PGTimer; microseconds:Pgulong):gdouble; cdecl; external gliblib;
procedure g_usleep(microseconds:gulong); cdecl; external gliblib;
procedure g_time_val_add(time:PGTimeVal; microseconds:glong); cdecl; external gliblib;
{$ENDIF read_interface_rest}
// included by glib2.pas


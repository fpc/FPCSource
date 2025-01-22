{%mainunit pipewire}
(*

procedure pw_loop_add_source(loop : Ppw_loop); varargs; cdecl;

begin
  spa_loop_add_source(loop^.loop); __VA_ARGS__)
end;

procedure pw_loop_update_source(loop : Ppw_loop); varargs; cdecl;
//spa_loop_update_source((l)->loop,__VA_ARGS__)
procedure pw_loop_remove_source(loop : Ppw_loop); varargs; cdecl;
// spa_loop_remove_source((l)->loop,__VA_ARGS__)
procedure pw_loop_invoke(loop : Ppw_loop); varargs; cdecl;
// spa_loop_invoke((l)->loop,__VA_ARGS__)
procedure pw_loop_get_fd(l)
// spa_loop_control_get_fd((l)->control)
procedure pw_loop_add_hook(loop : Ppw_loop); varargs; cdecl;
// spa_loop_control_add_hook((l)->controloop : Ppw_loop,__VA_ARGS__)
procedure pw_loop_enter(l);
// 	spa_loop_control_enter((l)->control)
procedure pw_loop_iterate(loop : Ppw_loop); varargs; cdecl;
// spa_loop_control_iterate((l)->controloop : Ppw_loop,__VA_ARGS__)
procedure pw_loop_leave(l);
//		spa_loop_control_leave((l)->control)
procedure pw_loop_add_io(loop : Ppw_loop); varargs; cdecl;
// spa_loop_utils_add_io((l)->utils,__VA_ARGS__)
procedure pw_loop_update_io(loop : Ppw_loop); varargs; cdecl;
// spa_loop_utils_update_io((l)->utils,__VA_ARGS__)
procedure pw_loop_add_idle(loop : Ppw_loop); varargs; cdecl;
// spa_loop_utils_add_idle((l)->utils,__VA_ARGS__)
procedure pw_loop_enable_idle(loop : Ppw_loop); varargs;
// cdecl;	spa_loop_utils_enable_idle((l)->utils,__VA_ARGS__)
procedure pw_loop_add_event(loop : Ppw_loop); varargs; cdecl;
// spa_loop_utils_add_event((l)->utils,__VA_ARGS__)
procedure pw_loop_signal_event(loop : Ppw_loop); varargs; cdecl;
// spa_loop_utils_signal_event((l)->utils,__VA_ARGS__)
procedure pw_loop_add_timer(loop : Ppw_loop); varargs; cdecl;
// spa_loop_utils_add_timer((l)->utils,__VA_ARGS__)
procedure pw_loop_update_timer(loop : Ppw_loop); varargs; cdecl;
// spa_loop_utils_update_timer((l)->utils,__VA_ARGS__)
procedure pw_loop_add_signal(loop : Ppw_loop); varargs; cdecl;
// spa_loop_utils_add_signal((l)->utils,__VA_ARGS__)
procedure pw_loop_destroy_source(loop : Ppw_loop); varargs; cdecl;
// spa_loop_utils_destroy_source((l)->utils,__VA_ARGS__

*)

function pw_loop_signal_event(loop : ppw_loop; aData : pspa_source) : cint;

var
  _o : pspa_loop_utils;
  _f : pspa_loop_utils_methods;

begin
  _o:=loop^.utils;
  _f:=(_o^.iface.cb.funcs);
  Result := _f^.signal_event(_o^.iface.cb.data,aData);
end;

function pw_loop_add_event(loop : Ppw_loop; aEvent : Tspa_source_event_func_t; aData : Pointer) : pspa_source;


var
  _o : pspa_loop_utils;
  _f : pspa_loop_utils_methods;

begin
  _o:=loop^.utils;
  _f:=(_o^.iface.cb.funcs);
  Result := _f^.add_event(_o^.iface.cb.data,aEvent,aData);
end;


type
  Ppw_loop = ^Tpw_loop;

  Tpw_loop = record
    system: Pspa_system;
    loop: Pspa_loop;
    control: Pspa_loop_control;
    utils: Pspa_loop_utils;
  end;



function pw_loop_new(props: Pspa_dict) : Ppw_loop; cdecl; external libpipewirename Name 'pw_loop_new';

procedure pw_loop_destroy(loop: Ppw_loop); cdecl; external libpipewirename Name 'pw_loop_destroy';

function pw_loop_signal_event(loop : Ppw_loop; aData : pspa_source) : cint;

(*

procedure pw_loop_add_source(loop : Ppw_loop); varargs; cdecl;
// spa_loop_add_source((l)->loop,__VA_ARGS__)
procedure pw_loop_update_source(loop : Ppw_loop); varargs; cdecl;
//spa_loop_update_source((l)->loop,__VA_ARGS__)
procedure pw_loop_remove_source(loop : Ppw_loop); varargs; cdecl;
// spa_loop_remove_source((l)->loop,__VA_ARGS__)
procedure pw_loop_invoke(loop : Ppw_loop); varargs; cdecl;
// spa_loop_invoke((l)->loop,__VA_ARGS__)
procedure pw_loop_get_fd(l : ppw_loop); cdecl;
// spa_loop_control_get_fd((l)->control)
procedure pw_loop_add_hook(loop : Ppw_loop); varargs; cdecl;
// spa_loop_control_add_hook((l)->controloop : Ppw_loop,__VA_ARGS__)
procedure pw_loop_enter(l : ppw_loop);cdecl;
// 	spa_loop_control_enter((l)->control)
procedure pw_loop_iterate(loop : Ppw_loop); varargs; cdecl;
// spa_loop_control_iterate((l)->controloop : Ppw_loop,__VA_ARGS__)
procedure pw_loop_leave(l : Ppw_loop); cdecl;
//		spa_loop_control_leave((l)->control)
procedure pw_loop_add_io(loop : Ppw_loop); varargs; cdecl;
// spa_loop_utils_add_io((l)->utils,__VA_ARGS__)
procedure pw_loop_update_io(loop : Ppw_loop); varargs; cdecl;
// spa_loop_utils_update_io((l)->utils,__VA_ARGS__)
procedure pw_loop_add_idle(loop : Ppw_loop); varargs; cdecl;
// spa_loop_utils_add_idle((l)->utils,__VA_ARGS__)
procedure pw_loop_enable_idle(loop : Ppw_loop); varargs; cdecl;
// spa_loop_utils_enable_idle((l)->utils,__VA_ARGS__)
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

function pw_loop_add_event(loop : Ppw_loop; aEvent : Tspa_source_event_func_t; aData : Pointer) : pspa_source;


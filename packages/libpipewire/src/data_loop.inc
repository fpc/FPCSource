
type
  Tpw_data_loop = record
  end;
  Ppw_data_loop = ^Tpw_data_loop;
  Ppw_data_loop_events = ^Tpw_data_loop_events;

  Tpw_data_loop_events = record
    version: Tuint32_t;
    Destroy: procedure(Data: pointer); cdecl;
  end;


function pw_data_loop_new(props: Pspa_dict) : Ppw_data_loop; cdecl; external libpipewirename Name 'pw_data_loop_new';

procedure pw_data_loop_add_listener(loop: Ppw_data_loop; listener: Pspa_hook; events: Ppw_data_loop_events; Data: pointer); cdecl; external libpipewirename Name 'pw_data_loop_add_listener';

function pw_data_loop_wait(loop: Ppw_data_loop; timeout: longint) : longint; cdecl; external libpipewirename Name 'pw_data_loop_wait';

procedure pw_data_loop_exit(loop: Ppw_data_loop); cdecl; external libpipewirename Name 'pw_data_loop_exit';

function pw_data_loop_get_loop(loop: Ppw_data_loop) : Ppw_loop; cdecl; external libpipewirename Name 'pw_data_loop_get_loop';

procedure pw_data_loop_destroy(loop: Ppw_data_loop); cdecl; external libpipewirename Name 'pw_data_loop_destroy';

function pw_data_loop_start(loop: Ppw_data_loop) : longint; cdecl; external libpipewirename Name 'pw_data_loop_start';

function pw_data_loop_stop(loop: Ppw_data_loop) : longint; cdecl; external libpipewirename Name 'pw_data_loop_stop';

function pw_data_loop_in_thread(loop: Ppw_data_loop) : T_Bool; cdecl; external libpipewirename Name 'pw_data_loop_in_thread';

function pw_data_loop_get_thread(loop: Ppw_data_loop) : Pspa_thread; cdecl; external libpipewirename Name 'pw_data_loop_get_thread';

function pw_data_loop_invoke(loop: Ppw_data_loop; func: Tspa_invoke_func_t; seq: Tuint32_t; Data: pointer; size: Tsize_t; block: T_Bool; user_data: pointer) : longint;
  cdecl; external libpipewirename Name 'pw_data_loop_invoke';



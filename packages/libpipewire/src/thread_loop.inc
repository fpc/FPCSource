
Type
  Ppw_thread_loop  = ^Tpw_thread_loop;
  Ppw_thread_loop_events  = ^Tpw_thread_loop_events;
  Ptimespec  = ^Ttimespec;

  tpw_thread_loop = record end;

  Tpw_thread_loop_events = record
    version : Tuint32_t;
    destroy : procedure (data:pointer);cdecl;
  end;


function pw_thread_loop_new(name:Pchar; props:Pspa_dict):Ppw_thread_loop;cdecl;external libpipewirename name 'pw_thread_loop_new';

function pw_thread_loop_new_full(loop:Ppw_loop; name:Pchar; props:Pspa_dict):Ppw_thread_loop;cdecl;external libpipewirename name 'pw_thread_loop_new_full';

procedure pw_thread_loop_destroy(loop:Ppw_thread_loop);cdecl;external libpipewirename name 'pw_thread_loop_destroy';

procedure pw_thread_loop_add_listener(loop:Ppw_thread_loop; listener:Pspa_hook; events:Ppw_thread_loop_events; data:pointer);cdecl;external libpipewirename name 'pw_thread_loop_add_listener';

function pw_thread_loop_get_loop(loop:Ppw_thread_loop):Ppw_loop;cdecl;external libpipewirename name 'pw_thread_loop_get_loop';

function pw_thread_loop_start(loop:Ppw_thread_loop):longint;cdecl;external libpipewirename name 'pw_thread_loop_start';

procedure pw_thread_loop_stop(loop:Ppw_thread_loop);cdecl;external libpipewirename name 'pw_thread_loop_stop';

procedure pw_thread_loop_lock(loop:Ppw_thread_loop);cdecl;external libpipewirename name 'pw_thread_loop_lock';

procedure pw_thread_loop_unlock(loop:Ppw_thread_loop);cdecl;external libpipewirename name 'pw_thread_loop_unlock';

procedure pw_thread_loop_wait(loop:Ppw_thread_loop);cdecl;external libpipewirename name 'pw_thread_loop_wait';

function pw_thread_loop_timed_wait(loop:Ppw_thread_loop; wait_max_sec:longint):longint;cdecl;external libpipewirename name 'pw_thread_loop_timed_wait';

function pw_thread_loop_get_time(loop:Ppw_thread_loop; abstime:Ptimespec; timeout:Tint64_t):longint;cdecl;external libpipewirename name 'pw_thread_loop_get_time';

function pw_thread_loop_timed_wait_full(loop:Ppw_thread_loop; abstime:Ptimespec):longint;cdecl;external libpipewirename name 'pw_thread_loop_timed_wait_full';

procedure pw_thread_loop_signal(loop:Ppw_thread_loop; wait_for_accept:T_Bool);cdecl;external libpipewirename name 'pw_thread_loop_signal';

procedure pw_thread_loop_accept(loop:Ppw_thread_loop);cdecl;external libpipewirename name 'pw_thread_loop_accept';

function pw_thread_loop_in_thread(loop:Ppw_thread_loop):T_Bool;cdecl;external libpipewirename name 'pw_thread_loop_in_thread';



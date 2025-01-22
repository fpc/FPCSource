

  Type
    Tpw_main_loop = record end;
    Ppw_main_loop  = ^Tpw_main_loop;
    Ppw_main_loop_events  = ^Tpw_main_loop_events;

    Tpw_main_loop_events = record
        version : Tuint32_t;
        destroy : procedure (data:pointer);cdecl;
      end;


  function pw_main_loop_new(props:Pspa_dict):Ppw_main_loop;cdecl;external libpipewirename name 'pw_main_loop_new';

  procedure pw_main_loop_add_listener(loop:Ppw_main_loop; listener:Pspa_hook; events:Ppw_main_loop_events; data:pointer);cdecl;external libpipewirename name 'pw_main_loop_add_listener';

  function pw_main_loop_get_loop(loop:Ppw_main_loop):Ppw_loop;cdecl;external libpipewirename name 'pw_main_loop_get_loop';

  procedure pw_main_loop_destroy(loop:Ppw_main_loop);cdecl;external libpipewirename name 'pw_main_loop_destroy';

  function pw_main_loop_run(loop:Ppw_main_loop):longint;cdecl;external libpipewirename name 'pw_main_loop_run';

  function pw_main_loop_quit(loop:Ppw_main_loop):longint;cdecl;external libpipewirename name 'pw_main_loop_quit';



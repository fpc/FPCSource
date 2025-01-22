
const
  PW_VERSION_STREAM_EVENTS = 2;

type
  Ppw_buffer = ^Tpw_buffer;
  Ppw_stream = ^Tpw_stream;
  Ppw_stream_control = ^Tpw_stream_control;
  Ppw_stream_events = ^Tpw_stream_events;
  Ppw_stream_flags = ^Tpw_stream_flags;
  Ppw_time = ^Tpw_time;

  float = double;

  tpw_buffer = record
    buffer: pspa_buffer;
    user_data: Pointer;
    size: tuint64_t;
  end;
  tpw_stream = record
  end;

  tpw_stream_control = record
    Name: PChar;
    flags: tuint32_t;
    def: float;
    min: float;
    max: float;
    values: float;
    n_values: tuint32_t;
    max_values: tuint32_t;
  end;


  tpw_time = record
    now: int64;
    rate: tspa_fraction;
    ticks: tuint64_t;
    delay: int64;
    queued: tuint64_t;
  end;


  tpw_stream_state = (
    PW_STREAM_STATE_ERROR := -1,
    PW_STREAM_STATE_UNCONNECTED := 0,
    PW_STREAM_STATE_CONNECTING := 1,
    PW_STREAM_STATE_PAUSED := 2,
    PW_STREAM_STATE_STREAMING := 3
    );

  Tpw_stream_events = record
    version: Tuint32_t;
    Destroy: procedure(Data: pointer); cdecl;
    state_changed: procedure(Data: pointer; old: Tpw_stream_state; state: Tpw_stream_state; error: PChar); cdecl;
    control_info: procedure(Data: pointer; id: Tuint32_t; control: Ppw_stream_control); cdecl;
    io_changed: procedure(Data: pointer; id: Tuint32_t; area: pointer; size: Tuint32_t); cdecl;
    param_changed: procedure(Data: pointer; id: Tuint32_t; param: Pspa_pod); cdecl;
    add_buffer: procedure(Data: pointer; buffer: Ppw_buffer); cdecl;
    remove_buffer: procedure(Data: pointer; buffer: Ppw_buffer); cdecl;
    process: procedure(Data: pointer); cdecl;
    drained: procedure(Data: pointer); cdecl;
    command: procedure(Data: pointer; command: Pspa_command); cdecl;
    trigger_done: procedure(Data: pointer); cdecl;
  end;


  Tpw_stream_flags = (
    PW_STREAM_FLAG_NONE := 0,
    PW_STREAM_FLAG_AUTOCONNECT := 1 shl 0,
    PW_STREAM_FLAG_INACTIVE := 1 shl 1,
    PW_STREAM_FLAG_MAP_BUFFERS := 1 shl 2,
    PW_STREAM_FLAG_DRIVER := 1 shl 3,
    PW_STREAM_FLAG_RT_PROCESS := 1 shl 4,
    PW_STREAM_FLAG_NO_CONVERT := 1 shl 5,
    PW_STREAM_FLAG_EXCLUSIVE := 1 shl 6,
    PW_STREAM_FLAG_DONT_RECONNECT := 1 shl 7,
    PW_STREAM_FLAG_ALLOC_BUFFERS := 1 shl 8,
    PW_STREAM_FLAG_TRIGGER := 1 shl 9
    );



function pw_stream_state_as_string(state: Tpw_stream_state) : PChar; cdecl; external libpipewirename Name 'pw_stream_state_as_string';


function pw_stream_new(core: Ppw_core; Name: PChar; props: Ppw_properties) : Ppw_stream; cdecl; external libpipewirename Name 'pw_stream_new';

function pw_stream_new_simple(loop: Ppw_loop; Name: PChar; props: Ppw_properties; events: Ppw_stream_events; Data: pointer) : Ppw_stream; cdecl; external libpipewirename Name 'pw_stream_new_simple';

procedure pw_stream_destroy(stream: Ppw_stream); cdecl; external libpipewirename Name 'pw_stream_destroy';

procedure pw_stream_add_listener(stream: Ppw_stream; listener: Pspa_hook; events: Ppw_stream_events; Data: pointer); cdecl; external libpipewirename Name 'pw_stream_add_listener';

function pw_stream_get_state(stream: Ppw_stream; error: PPchar) : Tpw_stream_state; cdecl; external libpipewirename Name 'pw_stream_get_state';

function pw_stream_get_name(stream: Ppw_stream) : PChar; cdecl; external libpipewirename Name 'pw_stream_get_name';

function pw_stream_get_core(stream: Ppw_stream) : Ppw_core; cdecl; external libpipewirename Name 'pw_stream_get_core';

function pw_stream_get_properties(stream: Ppw_stream) : Ppw_properties; cdecl; external libpipewirename Name 'pw_stream_get_properties';

function pw_stream_update_properties(stream: Ppw_stream; dict: Pspa_dict) : longint; cdecl; external libpipewirename Name 'pw_stream_update_properties';

function pw_stream_connect(stream: Ppw_stream; direction: Tspa_direction; target_id: Tuint32_t; flags: Tpw_stream_flags; params: PPspa_pod; n_params: Tuint32_t) : longint;
  cdecl; external libpipewirename Name 'pw_stream_connect';

function pw_stream_get_node_id(stream: Ppw_stream) : Tuint32_t; cdecl; external libpipewirename Name 'pw_stream_get_node_id';

function pw_stream_disconnect(stream: Ppw_stream) : longint; cdecl; external libpipewirename Name 'pw_stream_disconnect';

function pw_stream_set_error(stream: Ppw_stream; res: longint; error: PChar; args: array of const) : longint; cdecl; external libpipewirename Name 'pw_stream_set_error';

function pw_stream_set_error(stream: Ppw_stream; res: longint; error: PChar) : longint; cdecl; external libpipewirename Name 'pw_stream_set_error';

function pw_stream_update_params(stream: Ppw_stream; params: PPspa_pod; n_params: Tuint32_t) : longint; cdecl; external libpipewirename Name 'pw_stream_update_params';

function pw_stream_get_control(stream: Ppw_stream; id: Tuint32_t) : Ppw_stream_control; cdecl; external libpipewirename Name 'pw_stream_get_control';

function pw_stream_set_control(stream: Ppw_stream; id: Tuint32_t; n_values: Tuint32_t; values: Psingle; args: array of const) : longint; cdecl; external libpipewirename Name 'pw_stream_set_control';

function pw_stream_set_control(stream: Ppw_stream; id: Tuint32_t; n_values: Tuint32_t; values: Psingle) : longint; cdecl; external libpipewirename Name 'pw_stream_set_control';

function pw_stream_get_time(stream: Ppw_stream; time: Ppw_time) : longint; cdecl; external libpipewirename Name 'pw_stream_get_time';

function pw_stream_dequeue_buffer(stream: Ppw_stream) : Ppw_buffer; cdecl; external libpipewirename Name 'pw_stream_dequeue_buffer';

function pw_stream_queue_buffer(stream: Ppw_stream; buffer: Ppw_buffer) : longint; cdecl; external libpipewirename Name 'pw_stream_queue_buffer';

function pw_stream_set_active(stream: Ppw_stream; active: T_Bool) : longint; cdecl; external libpipewirename Name 'pw_stream_set_active';

function pw_stream_flush(stream: Ppw_stream; drain: T_Bool) : longint; cdecl; external libpipewirename Name 'pw_stream_flush';

function pw_stream_is_driving(stream: Ppw_stream) : T_Bool; cdecl; external libpipewirename Name 'pw_stream_is_driving';

function pw_stream_trigger_process(stream: Ppw_stream) : longint; cdecl; external libpipewirename Name 'pw_stream_trigger_process';



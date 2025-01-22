
Type
  Ppw_work_queue  = ^Tpw_work_queue;

  Tpw_work_func_t = procedure(obj: pointer; Data: pointer; res: longint; id: Tuint32_t); cdecl;

  tpw_work_queue = record
  end;

procedure pw_work_queue_destroy(queue: Ppw_work_queue); cdecl; external libpipewirename Name 'pw_work_queue_destroy';

function pw_work_queue_add(queue: Ppw_work_queue; obj: pointer; res: longint; func: Tpw_work_func_t; Data: pointer) : Tuint32_t; cdecl; external libpipewirename Name 'pw_work_queue_add';

function pw_work_queue_cancel(queue: Ppw_work_queue; obj: pointer; id: Tuint32_t) : longint; cdecl; external libpipewirename Name 'pw_work_queue_cancel';

function pw_work_queue_complete(queue: Ppw_work_queue; obj: pointer; seq: Tuint32_t; res: longint) : longint; cdecl; external libpipewirename Name 'pw_work_queue_complete';




{$IFDEF OGC_INTERFACE}
const
  LWP_MQ_FIFO = 0;
  LWP_MQ_PRIORITY = 1;
  LWP_MQ_STATUS_SUCCESSFUL = 0;
  LWP_MQ_STATUS_INVALID_SIZE = 1;
  LWP_MQ_STATUS_TOO_MANY = 2;
  LWP_MQ_STATUS_UNSATISFIED = 3;
  LWP_MQ_STATUS_UNSATISFIED_NOWAIT = 4;
  LWP_MQ_STATUS_DELETED = 5;
  LWP_MQ_STATUS_TIMEOUT = 6;
  LWP_MQ_STATUS_UNSATISFIED_WAIT = 7;
  LWP_MQ_SEND_REQUEST = 2147483647;//INT_MAX;
  LWP_MQ_SEND_URGENT = -2147483648;//INT_MIN;

type
  mq_notifyhandler = procedure(par0: pointer); cdecl;

  _mqbuffer = record
    size : cuint32;
    buffer : array [0..0] of cuint32;
  end;
  mq_buffer = _mqbuffer;
  pmq_buffer = ^_mqbuffer;

  _mqbuffercntrl = record
    node : lwp_node;
    prio : cuint32;
    contents : mq_buffer;
  end;
  mq_buffercntrl = _mqbuffercntrl;
  Pmq_buffercntrl = ^mq_buffercntrl;


//the following struct is extensible
  _mqattr = record
    mode : cuint32;
  end;
  mq_attr = _mqattr;

  _mqcntrl = record
    wait_queue : lwp_thrqueue;
    attr : mq_attr;
    max_pendingmsgs : cuint32;
    num_pendingmsgs : cuint32;
    max_msgsize : cuint32;
    pending_msgs : lwp_queue;
    msq_buffers : Pmq_buffer;
    notify_handler : mq_notifyhandler;
    notify_arg : pointer;
    inactive_msgs : lwp_queue;
  end;
  mq_cntrl = _mqcntrl;

  Pmq_attr = ^mq_attr;
  Pmq_cntrl = ^mq_cntrl;

function __lwpmq_initialize(mqueue: Pmq_cntrl; attrs: Pmq_attr;
  max_pendingmsgs, max_msgsize: cuint32): cuint32; cdecl; external;

procedure __lwpmq_close(mqueue: Pmq_cntrl; status: cuint32); cdecl; external;

function __lwpmq_seize(mqueue: Pmq_cntrl; id: cuint32; buffer: pointer;
  size: pcuint32; wait: cuint32; timeout: cuint64): cuint32; cdecl; external;

function __lwpmq_submit(mqueue: Pmq_cntrl; id: cuint32; buffer: pointer;
  size, type_, wait: cuint32; timeout: cuint64): cuint32; cdecl; external;

function __lwpmq_broadcast(mqueue: Pmq_cntrl; buffer: pointer;
  size, id: cuint32; count: pcuint32): cuint32; cdecl; external;

procedure __lwpmq_msg_insert(mqueue: Pmq_cntrl; msg: Pmq_buffercntrl;
  type_: cuint32); cdecl; external;

function __lwpmq_flush(mqueue: Pmq_cntrl): cuint32; cdecl; external;

function __lwpmq_flush_support(mqueue: Pmq_cntrl): cuint32; cdecl; external;

procedure __lwpmq_flush_waitthreads(mqueue: Pmq_cntrl); cdecl; external;


//{$ifdef LIBOGC_INTERNAL}
//{$include libogc/lwp_messages.inl}
//{$endif}
{$ENDIF}



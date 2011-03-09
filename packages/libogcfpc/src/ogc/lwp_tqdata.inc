
{$IFDEF OGC_INTERFACE}
const
  LWP_THREADQ_NUM_PRIOHEADERS = 4;
  LWP_THREADQ_PRIOPERHEADER = 64;
  LWP_THREADQ_REVERSESEARCHMASK = $20;
  LWP_THREADQ_SYNCHRONIZED = 0;
  LWP_THREADQ_NOTHINGHAPPEND = 1;
  LWP_THREADQ_TIMEOUT = 2;
  LWP_THREADQ_SATISFIED = 3;
  LWP_THREADQ_MODEFIFO = 0;
  LWP_THREADQ_MODEPRIORITY = 1;

type
  _lwpthrqueue = record
  	sync_state: cuint32;
  	mode: cuint32;
  	state: cuint32;
  	timeout_state: cuint32;
    case integer of
    0:(//queues: record
         fifo: lwp_queue;
       //end;
      );
		1:(//queues: record
         priority: array [0..LWP_THREADQ_NUM_PRIOHEADERS - 1] of lwp_queue;
       //end;
      );
  end;
  lwp_thrqueue = _lwpthrqueue;
  plwp_thrqueue = ^_lwpthrqueue;
{$ENDIF}



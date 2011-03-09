
{$IFDEF OGC_INTERFACE}
const
  DSPTASK_INIT = 0;  (*!< DSP task is initializing  *)
  DSPTASK_RUN = 1;  (*!< DSP task is running  *)
  DSPTASK_YIELD = 2;  (*!< DSP task has yield  *)
  DSPTASK_DONE = 3;  (*!< DSP task is done/ready  *)
  DSPTASK_CLEARALL = $00000000;  (*!< DSP task emtpy/ready  *)
  DSPTASK_ATTACH = $00000001;  (*!< DSP task attached  *)
  DSPTASK_CANCEL = $00000002;  (*!< DSP task canceled  *)

type
  DSPCallback = procedure; cdecl;
  DSPTaskCallback = procedure(task: pointer); cdecl;


type
  P_dsp_task = ^_dsp_task;
  _dsp_task = record
    state : cuint32;
    prio : cuint32;
    flags : cuint32;
    iram_maddr : pointer;
    iram_len : cuint32;
    iram_addr : cuint32;
    dram_maddr : pointer;
    dram_len : cuint32;
    dram_addr : cuint32;
    init_vec : cuint16;
    resume_vec : cuint16;
    init_cb : DSPTaskCallback;
    res_cb : DSPTaskCallback;
    done_cb : DSPTaskCallback;
    req_cb : DSPTaskCallback;
    next : P_dsp_task;
    prev : P_dsp_task;
  end;
  dsptask_t = _dsp_task;
  Pdsptask_t = ^dsptask_t;
  //DSPTaskCallback = procedure(task: Pdsptask_t); cdecl;


procedure DSP_Init; cdecl; external;


function DSP_CheckMailTo: cuint32; cdecl; external;


function DSP_CheckMailFrom: cuint32; cdecl; external;


function DSP_ReadMailFrom: cuint32; cdecl; external;


procedure DSP_AssertInt; cdecl; external;


procedure DSP_SendMailTo(mail: cuint32); cdecl; external;


function DSP_ReadCPUtoDSP: cuint32; cdecl; external;


function DSP_AddTask(task: Pdsptask_t): Pdsptask_t; cdecl; external;


function DSP_AssertTask(task: Pdsptask_t): Pdsptask_t; cdecl; external;


procedure DSP_CancelTask(task: Pdsptask_t); cdecl; external;


procedure DSP_Reset; cdecl; external;


procedure DSP_Halt; cdecl; external;


procedure DSP_Unhalt; cdecl; external;


function DSP_RegisterCallback(usr_cb: DSPCallback): DSPCallback; cdecl; external;
{$ENDIF}


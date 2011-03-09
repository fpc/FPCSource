
{$IFDEF OGC_INTERFACE}
const
  NUM_EXCEPTIONS = 15;
  EX_SYS_RESET = 0;
  EX_MACH_CHECK = 1;
  EX_DSI = 2;
  EX_ISI = 3;
  EX_INT = 4;
  EX_ALIGN = 5;
  EX_PRG = 6;
  EX_FP = 7;
  EX_DEC = 8;
  EX_SYS_CALL = 9;
  EX_TRACE = 10;
  EX_PERF = 11;
  EX_IABR = 12;
  EX_RESV = 13;
  EX_THERM = 14;

{$ifndef _LANGUAGE_ASSEMBLY}

type
  _excption_frame = record
    EXCPT_Number : cuint32;
    SRR0 : cuint32;
    SRR1 : cuint32;
    GPR : array [0..31] of cuint32;
    GQR : array [0..7] of cuint32;
    CR : cuint32;
    LR : cuint32;
    CTR : cuint32;
    XER : cuint32;
    MSR : cuint32;
    DAR : cuint32;
    state : cuint16;  //used to determine whether to restore the fpu context or not
    mode : cuint16;  //unused
    FPR : array [0..31] of f64;
    FPSCR : cuint64;
    PSFPR : array [0..31] of f64;
  end;
  frame_context = _excption_frame;


{$endif		_LANGUAGE_ASSEMBLY}
{$ENDIF}



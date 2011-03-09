
{$IFDEF OGC_INTERFACE}
const
  CPU_STACK_ALIGNMENT = 8;
  CPU_MINIMUM_STACK_SIZE = 1024 * 8;
  CPU_MINIMUM_STACK_FRAME_SIZE = 16;
  CPU_MODES_INTERRUPT_MASK = $00000001;  (* interrupt level in mode  *)

function __lwp_stack_allocate(par0: Plwp_cntrl; par1: cuint32): cuint32; cdecl; external;

procedure __lwp_stack_free(par0: Plwp_cntrl); cdecl; external;


//#ifdef LIBOGC_INTERNAL
//#include <libogc/lwp_stack.inl>
//#endif
{$ENDIF}



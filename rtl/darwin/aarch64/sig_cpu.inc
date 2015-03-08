
{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}



  type
     __darwin_arm_exception_state64 = record
          __far : cuint64;
          __esr : cuint32;
          __exception : cuint32;
       end;

     __darwin_arm_thread_state64 = record
          __r : array[0..28] of cuint64;
          __fp : cuint64;
          __lr : cuint64;
          __sp : cuint64;
          __pc : cuint64;
          __cpsr : cuint32;
       end;

     __darwin_arm_neon_state64 = record
          { actually an array of cuint128 }
          __r : array[0..31] of record l1,l2: cuint64; end;
          __fpsr : cuint32;
          __fpcr : cuint32;
          { array of cuint128 is aligned/padded to multiple of 16 bytes }
          pad: cuint64;
       end;

     __darwin_arm_debug_state64 = record
          __bvr : array[0..15] of cuint64;
          __bcr : array[0..15] of cuint64;
          __wvr : array[0..15] of cuint64;
          __wcr : array[0..15] of cuint64;
          __mdscr_el1: cuint64;
       end;

     mcontext_t = record
          __es : __darwin_arm_exception_state64;
          __ss : __darwin_arm_thread_state64;
          __fs : __darwin_arm_neon_state64;
       end;



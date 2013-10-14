
{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}



  type
     __darwin_arm_exception_state = record
          __exception : cuint32;
          __fsr : cuint32;
          __far : cuint32;
       end;

     __darwin_arm_thread_state = record
          __r : array[0..12] of cuint32;
          __sp : cuint32;
          __lr : cuint32;
          __pc : cuint32;
          __cpsr : cuint32;
       end;

     __darwin_arm_vfp_state = record
          __r : array[0..63] of cuint32;
          __fpscr : cuint32;
       end;

     __darwin_arm_debug_state = record
          __bvr : array[0..15] of cuint32;
          __bcr : array[0..15] of cuint32;
          __wvr : array[0..15] of cuint32;
          __wcr : array[0..15] of cuint32;
       end;

     mcontext_t = record
          __es : __darwin_arm_exception_state;
          __ss : __darwin_arm_thread_state;
          __fs : __darwin_arm_vfp_state;
       end;



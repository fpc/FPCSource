                        .area _CODE
                        .globl PASCALMAIN
                        .globl FPC_SAVE_IY
                        .globl __fpc_stackarea_start
                        .globl __fpc_stackarea_end

start::
                        ld sp, #__fpc_stackarea_end
                        ld (FPC_SAVE_IY), iy
                        jp PASCALMAIN

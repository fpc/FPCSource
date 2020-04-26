                        .area _CODE
                        .globl PASCALMAIN
                        .globl FPC_SAVE_IY
start::
                        ld sp, #stack_area_end
                        ld (FPC_SAVE_IY), iy
                        jp PASCALMAIN

                        .area _DATA
stack_area_start:       ;.rs 1022
                        .rs 254
stack_area_end:         .rs 2

                        .area _CODE
                        .globl _todo_pascal_main_
                        .globl FPC_SAVE_IY
start::
                        ld sp, #stack_area_end
                        ld (FPC_SAVE_IY), iy
                        jp _todo_pascal_main_

                        .area _DATA
stack_area_start:       .rs 1022
stack_area_end:         .rs 2

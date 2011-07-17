    _start:
        clr r1

        // load stack pointer
        ldi r30,lo8(_stack_top)
        out 0x3d,r30
        ldi r30,hi8(_stack_top)
        out 0x3e,r30
{
        // copy initialized data from flash to ram
        ldr r1,.L_etext
        ldr r2,.L_data
        ldr r3,.L_edata
.Lcopyloop:
        cmp r2,r3
        ldrls r0,[r1],#4
        strls r0,[r2],#4
        bls .Lcopyloop

        // clear onboard ram
        ldr r1,.L_bss_start
        ldr r2,.L_bss_end
        mov r0,#0
.Lzeroloop:
        cmp r1,r2
        strls r0,[r1],#4
        bls .Lzeroloop
 }

        call PASCALMAIN
        call _FPC_haltproc
        .text


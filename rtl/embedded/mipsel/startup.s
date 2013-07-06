/*********************************************************************
 *
 *                  Pascal Runtime Startup
 *
 *********************************************************************
 * Filename:        startup.s
 *
 * Processor:       PIC32
 *
 * Compiler:        MPLAB XC32
 *                  MPLAB X IDE
 * Company:         Microchip Technology Inc.
 *
 * Software License Agreement
 *
 * This software is developed by Microchip Technology Inc. and its
 * subsidiaries ("Microchip").
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1.      Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * 2.      Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * 3.      Microchip's name may not be used to endorse or promote products
 * derived from this software without $specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY MICROCHIP "AS IS" AND ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL
 * MICROCHIP BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING BUT NOT LIMITED TO
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA OR PROFITS;
 * OR BUSINESS INTERRUPTION) HOWSOEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
 * OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 * ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 ********************************************************************/


        ##################################################################
        # Entry point of the entire application
        ##################################################################
	.section .reset,"ax",@progbits
        .align 2
        .set noreorder
        .ent _reset
_reset:
        la      $k0, _startup
        jr      $k0                      # Jump to startup code
        nop

        .end _reset
        .globl _reset

        ##################################################################
        # Startup code
        ##################################################################
        .section .startup,"ax",@progbits
        .align 2
        .set noreorder
        .ent _startup
_startup:
        ##################################################################
        # If entered because of an NMI, jump to the NMI handler.
        ##################################################################
        mfc0    $k0,$12, 0                # _CP0_STATUS
        ext     $k0,$k0,19,1              # Extract NMI bit
        beqz    $k0,_no_nmi
        nop
        la      $k0,_nmi_handler
        jr      $k0
        nop
_no_nmi:
        ##################################################################
        # Initialize Stack Pointer
        #   _stack is initialized by the linker script to point to the
        #    starting location of the stack in DRM
        ##################################################################
        la      $sp,_stack_top

        ##################################################################
        # Initialize Global Pointer
        #   _gp is initialized by the linker script to point to "middle"
        #   of the small variables region
        ##################################################################
        la      $gp,_gp

        ##################################################################
        # Initialize Global Pointer in Shadow Set
        #   The SRSCtl's PSS field must be set to the shadow set in which
        #   to initialize the global pointer.  Since we only have a
        #   single shadow set (besides the normal), we will initialize
        #   SRSCtl<PSS> to SRSCtl<HSS>.  We then write the global pointer
        #   to the previous shadow set to ensure that on interrupt, the
        #   global pointer has been initialized.
        ##################################################################
        mfc0    $t1,$12, 2                 #_CP0_SRSCTL          # Read SRSCtl register
        add     $t3,$t1,$zero              # Save off current SRSCtl
        ext     $t2,$t1,26,4              # to obtain HSS field
        ins     $t1,$t2,6,4               # Put HSS field
        mtc0    $t1,$12, 2              #_CP0_SRSCTL          # into SRSCtl<PSS>
        ehb                             # Clear hazard before using new SRSCTL
        wrpgpr  $gp,$gp                   # Set global pointer in PSS
        mtc0    $t3,$12, 2              #_CP0_SRSCTL          # Restore SRSCtl

        ##################################################################
        # Call the "on reset" procedure
        ##################################################################
#        la      $t0,_on_reset
#        jalr    $t0
#        nop

        ##################################################################
        # Clear uninitialized data sections
        ##################################################################
        la      $t0,_bss_start
        la      $t1,_bss_end
        b       _bss_check
        nop

_bss_init:
        sw      $zero,0x0($t0)
        sw      $zero,0x4($t0)
        sw      $zero,0x8($t0)
        sw      $zero,0xc($t0)
        addu    $t0,16
_bss_check:
        bltu    $t0,$t1,_bss_init
        nop

        ##################################################################
        # Copy initialized data from program flash to data memory
        #   src=_data_image_begin dst=_data_begin stop=_data_end
        ##################################################################
#       TODO
#        la      $t0,_data_image_begin
#        la      $t1,_data
#        la      $t2,_edata
#        b       _init_check
#        nop

_init_data:
#        lw      $t3,($t0)
#        sw      $t3,($t1)
#        addu    $t0,4
#        addu    $t1,4
_init_check:
#        bltu    $t1,$t2,_init_data
#        nop

        ##################################################################
        # If there are no RAM functions, skip the next two sections --
        # copying RAM functions from program flash to data memory and
        # initializing bus matrix registers.
        ##################################################################
#        la      $t1,_ramfunc_length
#        beqz    $t1,_ramfunc_done
#        nop

        ##################################################################
        # Copy RAM functions from program flash to data memory
        #   src=_ramfunc_image_begin dst=_ramfunc_begin stop=_ramfunc_end
        ##################################################################
#        la      $t0,_ramfunc_image_begin
#        la      $t1,_ramfunc_begin
#        la      $t2,_ramfunc_end

#_init_ramfunc:
#        lw      $t3,($t0)
#        sw      $t3,($t1)
#        addu    $t0,4
#        addu    $t1,4
#_ramfunc_check:
#        bltu    $t1,$t2,_init_ramfunc
#        nop

        ##################################################################
        # Initialize bus matrix registers if RAM functions exist in the
        # application
        ##################################################################
#        la      $t1,_bmxdkpba_address
#        la      $t2,BMXDKPBA
#        sw      $t1,0($t2)
#        la      $t1,_bmxdudba_address
#        la      $t2,BMXDUDBA
#        sw      $t1,0($t2)
#        la      $t1,_bmxdupba_address
#        la      $t2,BMXDUPBA
#        sw      $t1,0($t2)
#_ramfunc_done:

        ##################################################################
        # Initialize CP0 registers
        ##################################################################
        # Initialize Count register
        ##################################################################
        mtc0    $zero,$9, 0  #_CP0_COUNT

        ##################################################################
        # Initialize Compare register
        ##################################################################
        li      $t2,-1
        mtc0    $t2,$11, 0 #_CP0_COMPARE

        ##################################################################
        # Initialize EBase register
        ##################################################################
        la      $t1,_ebase_address
        mtc0    $t1,$15, 1  #_CP0_EBASE

        ##################################################################
        # Initialize IntCtl register
        ##################################################################
        la      $t1,_vector_spacing
        li      $t2,0                    # Clear $t2 and
        ins     $t2,$t1,5,5               # shift value to VS field
        mtc0    $t2,$12, 1  #_CP0_INTCTL

        ##################################################################
        # Initialize CAUSE registers
        # - Enable counting of Count register <DC = 0>
        # - Use $special exception vector <IV = 1>
        # - Clear pending software interrupts <IP1:IP0 = 0>
        ##################################################################
        li      $t1,0x00800000
        mtc0    $t1,$13, 0   #_CP0_CAUSE

        ##################################################################
        # Initialize STATUS register
        # - Access to Coprocessor 0 not allowed in user mode <CU0 = 0>
        # - User mode uses configured endianness <RE = 0>
        # - Preserve Bootstrap Exception vectors <BEV>
        # - Preserve soft reset <SR> and non-maskable interrupt <NMI>
        # - CorExtend enabled based on whether CorExtend User Defined
        #   Instructions have been implemented <CEE = Config<UDI>>
        # - Disable any pending interrups <IM7..IM2 = 0, IM1..IM0 = 0>
        # - Disable hardware interrupts <IPL7:IPL2 = 0>
        # - Base mode is Kernel mode <UM = 0>
        # - Error level is normal <ERL = 0>
        # - Exception level is normal <EXL = 0>
        # - Interrupts are disabled <IE = 0>
        ##################################################################
        mfc0    $t0,$16, 0                #_CP0_CONFIG
        ext     $t1,$t0,22,1              # Extract UDI from Config register
        sll     $t1,$t1,17                # Move UDI to Status.CEE location
        mfc0    $t0,$12, 0                #_CP0_STATUS
        and     $t0,$t0,0x00580000        # Preserve SR, NMI, and BEV
        or      $t0,$t1,$t0                # Include Status.CEE (from UDI)
        mtc0    $t0,$12, 0                #_CP0_STATUS

        ##################################################################
        # Call the "on bootstrap" procedure
        ##################################################################
 #       la      $t0,_on_bootstrap
 #       jalr    $t0
 #       nop

        ##################################################################
        # Initialize Status<BEV> for normal exception vectors
        ##################################################################
        mfc0    $t0,$12, 0                # _CP0_STATUS
        and     $t0,$t0,0xffbfffff        # Clear BEV
        mtc0    $t0,$12, 0                # _CP0_STATUS

        ##################################################################
        # Call main. We do this via a thunk in the text section so that
        # a normal jump and link can be used, enabling the startup code
        # to work properly whether main is written in MIPS16 or MIPS32
        # code. I.e., the linker will correctly adjust the JAL to JALX if
        # necessary
        ##################################################################
        and     $a0,$a0,0
        and     $a1,$a1,0
        la      $t0,_main_entry
        jr      $t0
        nop

        .end _startup
        .globl _startup

        ##################################################################
        # Boot Exception Vector Handler
        # Jumps to _bootstrap_exception_handler
        ##################################################################
        .section .bev_handler,"ax",@progbits
        .align 2
        .set noreorder
        .ent _bev_exception
_bev_exception:
        la      $k0,_bootstrap_exception_handler
        jr      $k0
        nop

        .end _bev_exception

        ##################################################################
        # General Exception Vector Handler
        # Jump to _general_exception_handler
        ##################################################################
        .section .gen_handler,"ax",@progbits
        .align 2
        .set noreorder
        .ent _gen_exception
_gen_exception:
        #la      $k0,_general_exception_context
        la      $k0,_DefaultInterrupt
        jr      $k0
        nop

        .end _gen_exception

        .text
        .ent _main_entry
_main_entry:
        and     $a0,$a0,0
        and     $a1,$a1,0

        ##################################################################
        # Call main
        ##################################################################
        jal main
        nop

        ##################################################################
        # Just in case, go into infinite loop
        ##################################################################
        j _haltproc
        nop
        .end _main_entry

        ##################################################################
        # Haltproc
        ##################################################################
        .text
        .ent _haltproc
_haltproc:
        b       _haltproc
        nop
        sdbbp 0

        .set    macro
        .set    reorder
        .globl _haltproc
        .end  _haltproc
        .size   _haltproc, .-_haltproc

        ##################################################################
        # NMI Handler
        ##################################################################
        .text
        .align  2
        .weak   _nmi_handler
        .set    nomips16
        .ent    _nmi_handler
_nmi_handler:
        .frame  $sp,0,$31                # vars= 0, regs= 0/0, args= 0, gp= 0
        .mask   0x00000000,0
        .fmask  0x00000000,0
        .set    noreorder

        mfc0    $k0, $12, 0                          # _CP0_STATUS                   # retrieve STATUS
        lui     $k1, ~(0x00400000 >> 16) & 0xffff                         # _CP0_STATUS_BEV_MASK = 0x00400000
        ori     $k1, $k1, ~0x00400000 & 0xffff       # _CP0_STATUS_BEV_MASK = 0x00400000
        and     $k0, $k0, $k1                        # Clear BEV
        mtc0    $k0, $12, 0                          # _CP0_STATUS                   # store STATUS
        eret

        .set    macro
        .set    reorder
        .end    _nmi_handler
        .size   _nmi_handler, .-_nmi_handler

        ##################################################################
        # _bootstrap_exception_handler
        ##################################################################
        .text
        .align  2
        .weak   _bootstrap_exception_handler
        .set    nomips16
        .ent    _bootstrap_exception_handler
_bootstrap_exception_handler:
        sdbbp 0
        b       _bootstrap_exception_handler
        .set    macro
        .set    reorder
        .end    _bootstrap_exception_handler
        .size   _bootstrap_exception_handler, .-_bootstrap_exception_handler

        ##################################################################
        # _DefaultInterrupt
        ##################################################################
        .text
        .align  2
        .weak   _DefaultInterrupt
        .set    nomips16
        .ent    _DefaultInterrupt
_DefaultInterrupt:
        sdbbp 0
        b       _DefaultInterrupt
        .set    macro
        .set    reorder
        .end    _DefaultInterrupt
        .size   _DefaultInterrupt, .-_DefaultInterrupt

.file "prt0.as"
	.text
   .globl __entry
   .globl _start
   .globl __start
__entry:
_start:
__start:
       movel (sp)+, d0     
       lea   (4,sp,d0*4),a0
       movel a0, U_SYSLINUX_ENVP
       movel sp,U_SYSLINUX_ARGV
       movel d0,U_SYSLINUX_ARGC
/*
   movel d0,U_SYSLINUX_ENVP
   movel 4(sp),d0
   movel d0,U_SYSLINUX_ARGV
   movel (sp),d0
   movel d0,U_SYSLINUX_ARGC */
   jsr   PASCALMAIN

   .globl _haltproc
   .globl _HALTPROC
_haltproc:
_HALTPROC:
   moveq #1,d0
   movew U_SYSLINUX_EXITCODE,d1
   trap  #0
   bra   _haltproc


   .data
	.align  4
        .globl  ___FPC_BRK_ADDR
	.globl  ___fpc_brk_addr
___fpc_brk_addr:
___FPC_BRK_ADDR:
	.long   0


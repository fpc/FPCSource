.file "prt0.as"
	.text
   .globl __entry
__entry:
   movel sp@(8),d0
   movel d0,U_SYSLINUX_ENVP
   movel sp@(4),d0
   movel d0,U_SYSLINUX_ARGV
   movel sp@,d0
   movel d0,U_SYSLINUX_ARGC
   jsr  PASCALMAIN

   .globl _haltproc
_haltproc:
   moveq #1,d0
   movel U_SYSLINUX_EXITCODE,d1
   trap  #0
   bras  _haltproc


   .data
	.align	4
	.globl	___fpc_brk_addr
___fpc_brk_addr:
	.long	0


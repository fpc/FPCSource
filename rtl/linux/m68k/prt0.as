.file "crt0.s"
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



   movel U_SYSLINUX_EXITCODE,d1
done:
   moveq #1,d0
   trap  #0
   bras  done

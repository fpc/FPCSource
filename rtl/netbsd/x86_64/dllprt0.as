	.file	"crt0.c"
	.globl	__progname
	.section	.rodata
.LC0:
	.string	""
	.data
	.align 8
	.type	__progname, @object
	.size	__progname, 8
__progname:
	.quad	.LC0
	.text 
        .p2align 2,0x90

.globl _haltproc
.type _haltproc,@function

_haltproc:
           movq $1,%rax
           movzwq operatingsystem_result(%rip),%rbx
           pushq   %rbx
           call .Lactualsyscall
           addq  $8,%rsp
           jmp   _haltproc

.Lactualsyscall:
         int $0x80
         jb .LErrorcode
         xor %rbx,%rbx
         ret
.LErrorcode:
	movl	%eax, %edi

# This section is needed for NetBSD to recognize a NetBSD binary as such.
# otherwise it will be startup in Linux emulation mode.

.section ".note.netbsd.ident","a"
.p2align 2

.long 7
.long 4
# ELF NOTE TYPE NETBSD TAG
.long 1
.ascii "NetBSD\0\0"
.long 199905

	.comm	environ,8,8
	.comm	__progname_storage,256,32
        .comm   operatingsystem_parameter_envp,8,8
        .comm   operatingsystem_parameter_argc,8,8
        .comm   operatingsystem_parameter_argv,8,8


	.file "inputall.pp"

.text
	.align 4
.globl	_DONE_CB$PFL_OBJECT$LONGINT
	.type	_DONE_CB$PFL_OBJECT$LONGINT,@function
_DONE_CB$PFL_OBJECT$LONGINT:
.globl	done_cb
	.type	done_cb,@function
done_cb:
	pushl	%ebp
	movl	%esp,%ebp
	pushl	$0
	call	_SYSLINUX$$_HALT$BYTE
	leave
	ret	$8
	.align 4
.globl	_INPUT_CB$PFL_OBJECT$LONGINT
	.type	_INPUT_CB$PFL_OBJECT$LONGINT,@function
_INPUT_CB$PFL_OBJECT$LONGINT:
.globl	input_cb
	.type	input_cb,@function
input_cb:
	pushl	%ebp
	movl	%esp,%ebp
	subl	$532,%esp
	leal	-8(%ebp),%eax
	pushl	%eax
	leal	-4(%ebp),%eax
	pushl	%eax
	pushl	8(%ebp)
	call	fl_get_input_cursorpos
	addl	$12,%esp
	movl	%eax,-12(%ebp)
	pushl	$128
	leal	-273(%ebp),%edi
	pushl	%edi
	pushl	$-1
	pushl	-12(%ebp)
	call	FPC_SHORTSTR_LONGINT
	leal	-530(%ebp),%edi
	pushl	%edi
	pushl	$.L23
	pushl	$255
	call	FPC_SHORTSTR_COPY
	leal	-530(%ebp),%edi
	pushl	%edi
	leal	-273(%ebp),%edi
	pushl	%edi
	call	FPC_SHORTSTR_CONCAT
	leal	-530(%ebp),%edi
	pushl	%edi
	pushl	$.L24
	call	FPC_SHORTSTR_CONCAT
	leal	-141(%ebp),%edi
	pushl	%edi
	leal	-530(%ebp),%edi
	pushl	%edi
	pushl	$128
	call	FPC_SHORTSTR_COPY
	pushl	$128
	leal	-273(%ebp),%edi
	pushl	%edi
	pushl	$-1
	pushl	-4(%ebp)
	call	FPC_SHORTSTR_LONGINT
	leal	-530(%ebp),%edi
	pushl	%edi
	leal	-141(%ebp),%edi
	pushl	%edi
	pushl	$255
	call	FPC_SHORTSTR_COPY
	leal	-530(%ebp),%edi
	pushl	%edi
	pushl	$.L31
	call	FPC_SHORTSTR_CONCAT
	leal	-530(%ebp),%edi
	pushl	%edi
	leal	-273(%ebp),%edi
	pushl	%edi
	call	FPC_SHORTSTR_CONCAT
	leal	-530(%ebp),%edi
	pushl	%edi
	pushl	$.L24
	call	FPC_SHORTSTR_CONCAT
	leal	-141(%ebp),%edi
	pushl	%edi
	leal	-530(%ebp),%edi
	pushl	%edi
	pushl	$128
	call	FPC_SHORTSTR_COPY
	pushl	$128
	leal	-273(%ebp),%edi
	pushl	%edi
	pushl	$-1
	pushl	-8(%ebp)
	call	FPC_SHORTSTR_LONGINT
	leal	-530(%ebp),%edi
	pushl	%edi
	leal	-141(%ebp),%edi
	pushl	%edi
	pushl	$255
	call	FPC_SHORTSTR_COPY
	leal	-530(%ebp),%edi
	pushl	%edi
	pushl	$.L38
	call	FPC_SHORTSTR_CONCAT
	leal	-530(%ebp),%edi
	pushl	%edi
	leal	-273(%ebp),%edi
	pushl	%edi
	call	FPC_SHORTSTR_CONCAT
	leal	-530(%ebp),%edi
	pushl	%edi
	pushl	$.L39
	call	FPC_SHORTSTR_CONCAT
	leal	-141(%ebp),%edi
	pushl	%edi
	leal	-530(%ebp),%edi
	pushl	%edi
	pushl	$128
	call	FPC_SHORTSTR_COPY
	leal	-140(%ebp),%eax
	pushl	%eax
	movl	8(%ebp),%eax
	movl	(%eax),%eax
	movl	(%eax),%eax
	pushl	16(%eax)
	call	fl_set_object_label
	addl	$8,%esp
	leave
	ret	$8
	.align 4
.globl	_HIDE_SHOW_CB$PFL_OBJECT$LONGINT
	.type	_HIDE_SHOW_CB$PFL_OBJECT$LONGINT,@function
_HIDE_SHOW_CB$PFL_OBJECT$LONGINT:
.globl	hide_show_cb
	.type	hide_show_cb,@function
hide_show_cb:
	pushl	%ebp
	movl	%esp,%ebp
	subl	$4,%esp
	movl	8(%ebp),%eax
	movl	(%eax),%eax
	movl	(%eax),%edi
	movl	%edi,-4(%ebp)
	movl	-4(%ebp),%eax
	movl	12(%eax),%eax
	movl	188(%eax),%eax
	testl	%eax,%eax
	jne	.L48
	jmp	.L49
.L48:
	movl	-4(%ebp),%eax
	pushl	12(%eax)
	call	fl_hide_object
	popl	%edi
	jmp	.L52
.L49:
	movl	-4(%ebp),%eax
	pushl	12(%eax)
	call	fl_show_object
	popl	%edi
.L52:
	leave
	ret	$8
	.align 4
.globl	_CREATE_FORM_INPUT
	.type	_CREATE_FORM_INPUT,@function
_CREATE_FORM_INPUT:
	pushl	%ebp
	movl	%esp,%ebp
	subl	$12,%esp
	pushl	$20
	leal	-12(%ebp),%edi
	pushl	%edi
	call	FPC_GETMEM
	movl	-12(%ebp),%eax
	pushl	%eax
	pushl	$441
	pushl	$441
	pushl	$0
	call	fl_bgn_form
	addl	$12,%esp
	movl	%eax,%edx
	popl	%eax
	movl	%edx,(%eax)
	leal	.L69+1,%eax
	pushl	%eax
	pushl	$441
	pushl	$441
	pushl	$0
	pushl	$0
	pushl	$1
	call	fl_add_box
	addl	$24,%esp
	movl	%eax,-8(%ebp)
	leal	.L84+1,%eax
	pushl	%eax
	pushl	$30
	pushl	$340
	pushl	$40
	pushl	$40
	pushl	$0
	call	fl_add_input
	addl	$24,%esp
	movl	%eax,-8(%ebp)
	movl	-12(%ebp),%eax
	movl	-8(%ebp),%edi
	movl	%edi,8(%eax)
	pushl	$5
	pushl	-8(%ebp)
	call	fl_set_object_lalign
	addl	$8,%esp
	pushl	$0
	leal	_INPUT_CB$PFL_OBJECT$LONGINT,%eax
	pushl	%eax
	pushl	-8(%ebp)
	call	fl_set_object_callback
	addl	$12,%esp
	leal	.L111+1,%eax
	pushl	%eax
	pushl	$30
	pushl	$160
	pushl	$100
	pushl	$40
	pushl	$2
	call	fl_add_input
	addl	$24,%esp
	movl	%eax,-8(%ebp)
	pushl	$5
	pushl	-8(%ebp)
	call	fl_set_object_lalign
	addl	$8,%esp
	leal	.L130+1,%eax
	pushl	%eax
	pushl	$30
	pushl	$160
	pushl	$100
	pushl	$230
	pushl	$1
	call	fl_add_input
	addl	$24,%esp
	movl	%eax,-8(%ebp)
	pushl	$5
	pushl	-8(%ebp)
	call	fl_set_object_lalign
	addl	$8,%esp
	leal	.L149+1,%eax
	pushl	%eax
	pushl	$30
	pushl	$160
	pushl	$150
	pushl	$40
	pushl	$3
	call	fl_add_input
	addl	$24,%esp
	movl	%eax,-8(%ebp)
	pushl	$5
	pushl	-8(%ebp)
	call	fl_set_object_lalign
	addl	$8,%esp
	leal	.L168+1,%eax
	pushl	%eax
	pushl	$30
	pushl	$160
	pushl	$150
	pushl	$230
	pushl	$6
	call	fl_add_input
	addl	$24,%esp
	movl	%eax,-8(%ebp)
	pushl	$5
	pushl	-8(%ebp)
	call	fl_set_object_lalign
	addl	$8,%esp
	leal	.L187+1,%eax
	pushl	%eax
	pushl	$180
	pushl	$360
	pushl	$210
	pushl	$40
	pushl	$4
	call	fl_add_input
	addl	$24,%esp
	movl	%eax,-8(%ebp)
	movl	-12(%ebp),%eax
	movl	-8(%ebp),%edi
	movl	%edi,12(%eax)
	pushl	$0
	leal	_INPUT_CB$PFL_OBJECT$LONGINT,%eax
	pushl	%eax
	pushl	-8(%ebp)
	call	fl_set_object_callback
	addl	$12,%esp
	leal	.L69+1,%eax
	pushl	%eax
	pushl	$30
	pushl	$210
	pushl	$400
	pushl	$30
	pushl	$0
	call	fl_add_text
	addl	$24,%esp
	movl	%eax,-8(%ebp)
	movl	-12(%ebp),%eax
	movl	-8(%ebp),%edi
	movl	%edi,16(%eax)
	pushl	$8196
	pushl	-8(%ebp)
	call	fl_set_object_lalign
	addl	$8,%esp
	leal	.L230+1,%eax
	pushl	%eax
	pushl	$30
	pushl	$70
	pushl	$400
	pushl	$330
	pushl	$0
	call	fl_add_button
	addl	$24,%esp
	movl	%eax,-8(%ebp)
	pushl	$0
	leal	_DONE_CB$PFL_OBJECT$LONGINT,%eax
	pushl	%eax
	pushl	-8(%ebp)
	call	fl_set_object_callback
	addl	$12,%esp
	leal	.L251+1,%eax
	pushl	%eax
	pushl	$30
	pushl	$70
	pushl	$400
	pushl	$250
	pushl	$0
	call	fl_add_button
	addl	$24,%esp
	movl	%eax,-8(%ebp)
	pushl	$0
	leal	_HIDE_SHOW_CB$PFL_OBJECT$LONGINT,%eax
	pushl	%eax
	pushl	-8(%ebp)
	call	fl_set_object_callback
	addl	$12,%esp
	call	fl_end_form
	movl	-12(%ebp),%eax
	movl	(%eax),%eax
	movl	-12(%ebp),%edi
	movl	%edi,(%eax)
	movl	-12(%ebp),%edi
	movl	%edi,-4(%ebp)
	movl	-4(%ebp),%eax
	leave
	ret
	.align 4
.globl	main
	.type	main,@function
main:
.globl	PASCALMAIN
	.type	PASCALMAIN,@function
PASCALMAIN:
.globl	program_init
	.type	program_init,@function
program_init:
	pushl	%ebp
	movl	%esp,%ebp
	call	FPC_INITIALIZEUNITS
	pushl	$0
	pushl	$0
	leal	.L69+1,%eax
	pushl	%eax
	pushl	U_SYSLINUX_ARGV
	leal	U_SYSLINUX_ARGC,%eax
	pushl	%eax
	call	fl_initialize
	addl	$20,%esp
	call	_CREATE_FORM_INPUT
	movl	%eax,_FD_INPUT
	pushl	$1
	movl	_FD_INPUT,%eax
	pushl	16(%eax)
	call	fl_set_object_dblbuffer
	addl	$8,%esp
	pushl	$3
	movl	_FD_INPUT,%eax
	pushl	12(%eax)
	call	fl_set_object_return
	addl	$8,%esp
	pushl	$3
	movl	_FD_INPUT,%eax
	pushl	8(%eax)
	call	fl_set_object_return
	addl	$8,%esp
	leal	.L298+1,%eax
	pushl	%eax
	pushl	$1
	pushl	$16386
	movl	_FD_INPUT,%eax
	pushl	(%eax)
	call	fl_show_form
	addl	$16,%esp
	jmp	.L306
.L305:
.L306:
	call	fl_do_forms
	cmpl	$0,%eax
	jne	.L305
	jmp	.L307
.L307:
	call	FPC_DO_EXIT
	leave
	ret

.data
	.ascii	"FPC 0.99.11 for i386 - LINUX"
	.align 4
.globl	INITFINAL
	.type	INITFINAL,@object
INITFINAL:
	.long	3,0
	.long	INIT$$SYSLINUX
	.long	0
	.long	INIT$$OBJPAS
	.long	0
	.long	INIT$$XLIB
	.long	0
.globl	HEAPSIZE
	.type	HEAPSIZE,@object
HEAPSIZE:
	.long	2097152

.data
.L23:
	.ascii	"\004P = \000"
.L24:
	.ascii	"\001 \000"
.L31:
	.ascii	"\003x= \000"
.L38:
	.ascii	"\003y= \000"
.L39:
	.ascii	"\001\000\000"
.L69:
	.ascii	"\000\000"
.L84:
	.ascii	"\013NormalInput\000"
.L111:
	.ascii	"\010IntInput\000"
.L130:
	.ascii	"\012FloatInput\000"
.L149:
	.ascii	"\011DateInput\000"
.L168:
	.ascii	"\013Secretinput\000"
.L187:
	.ascii	"\003MMM\000"
.L230:
	.ascii	"\004Done\000"
.L251:
	.ascii	"\011Hide/Show\000"
.L298:
	.ascii	"\005input\000"

.data

.bss
	.lcomm	_FD_INPUT,4
	.comm	HEAP,2097152


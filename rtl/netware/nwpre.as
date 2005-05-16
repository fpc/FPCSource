#
#    $Id: nwpre.as,v 1.3 2003/03/25 18:17:54 armin Exp $
#    This file is part of the Free Pascal run time library.
#    Copyright (c) 1999-2002 by the Free Pascal development team
#    Copyright (c) 2002 Armin Diehl
#		
#    This is the (nwpre-like) startup code for netware
#	    
#    See the file COPYING.FPC, included in this distribution,
#    for details about the copyright.
#				    
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#				
#**********************************************************************

    .file "nwpre.as"
    .text

#
# This is the main program (not loader) Entry-Point that will be called by netware    
# it sets up the argc and argv and calls _nlm_main (in system.pp)
#
_pasStart_:
    pushl	$_nlm_main
    call	_SetupArgV_411
    addl	$4,%esp
    ret
# this is a hack to avoid that FPC_NW_CHECKFUNCTION will be
# eleminated by the linker (with smartlinking)
    call	FPC_NW_CHECKFUNCTION


#
# this will be called by the loader, we pass the address of _pasStart_ and
# _kNLMInfo (needed by clib) and netware is doing the work
#
    .globl	_Prelude
_Prelude:
       	pushl	%ebp
    	movl	%esp,%ebp
       	pushl	%edi
       	pushl	%esi
   	pushl	%ebx
     	movl	0x14(%ebp),%edi
     	movl	0x18(%ebp),%esi
	movl	%esi, __uninitializedDataSize
     	movl	0x1c(%ebp),%ebx
     	movl	0x20(%ebp),%ecx
     	movl	0x28(%ebp),%eax
   	pushl	$_pasStart_
   	pushl	$_kNLMInfo
   	pushl	%eax
     	movl	0x24(%ebp),%edx  # 1b7f6
   	pushl	%edx
       	pushl	%ecx  
   	pushl	%ebx
       	pushl	%esi			# uninitialized data size
       	pushl	%edi
     	movl	0x10(%ebp),%edx
   	pushl	%edx
     	movl	0xc(%ebp),%edx
   	pushl	%edx
     	movl	0x8(%ebp),%edx
 	pushl	%edx
       	call	_StartNLM
	test	%eax,%eax
    	jne	x1
    	xorl	%eax,%eax		# dont know why this is needed ?
x1:
     	lea	0xfffffff4(%ebp),%esp
   	popl	%ebx
       	popl	%esi
       	popl	%edi
    	movl	%ebp,%esp
   	popl	%ebp
   	ret


#
# the global stop-function
#
    .globl	_Stop
_Stop:
	pushl	$0x5			# TERMINATE_BY_UNLOAD=0, TERMINATE_BY_EXTERNAL_THREAD=0
	pushl	$0x0
       	movl	_kNLMInfo,%edx
       	pushl	%edx
       	call	_TerminateNLM
    	addl	$0x0c,%esp
       	ret


.data
# argc is defined in the novell nwpre, i assume it is not needed
#_argc:
#	.long	0

# structure needed by clib
#   kNLMInfoT =
#   PACKED RECORD
#      Signature      : ARRAY [0..3] OF CHAR;	// LONG 'NLMI'
#      Flavor         : LONGINT;		// TRADINIONAL_FLAVOR = 0
#      Version        : LONGINT;		// TRADINIONAL_VERSION = 0, LIBERTY_VERSION = 1
#      LongDoubleSize : LONGINT;		// gcc nwpre defines 12, watcom 8
#      wchar_tSize    : LONGINT;
#    END;
    .globl	_kNLMInfo		# will be used as data start
_kNLMInfo:
	.ascii	"NLMI"
	.long	0,1,8,2
	

.text
.globl	__getTextStart
__getTextStart:
    movl    $.text,%eax
    ret	
    
.text
.globl	__getDataStart
__getDataStart:
    movl    $.data,%eax
    ret

.text
.globl	__getBssStart
__getBssStart:
    movl    $.bss,%eax
    ret
    
.data 
  __uninitializedDataSize:	.long
    


.text
.globl  __getUninitializedDataSize
__getUninitializedDataSize:
    movl   __uninitializedDataSize, %eax
    ret

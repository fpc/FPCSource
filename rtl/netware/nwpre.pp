(*
#    $Id: nwpre.as,v 1.3 2003/03/25 18:17:54 armin Exp $
#    This file is part of the Free Pascal run time library.
#    Copyright (c) 1999-2011 by the Free Pascal development team
#    Copyright (c) 2002-2011 Armin Diehl
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
*)

unit nwpre;

interface

implementation

//    .file "nwpre.as"
//    .text


procedure _SetupArgV_411 (startProc:pointer); cdecl; external 'clib' name '_SetupArgV_411';
procedure _nlm_main; external  name '_nlm_main';
procedure FPC_NW_CHECKFUNCTION; external name 'FPC_NW_CHECKFUNCTION';
//procedure _StartNLM; external name '_StartNLM';

function _StartNLM (NLMHandle              : longint;
                   initErrorScreenID       : longint;
                   cmdLineP                : pchar;
                   loadDirectoryPath       : pchar;
                   uninitializedDataLength : longint;
                   NLMFileHandle           : longint;
                   readRoutineP            : pointer;
                   customDataOffset        : longint;
                   customDataSize          : longint;
                   NLMInformation          : pointer;
                   userStartFunc           : pointer) : longint; cdecl; external '!clib' name '_StartNLM';
                                                                                                                                                                                                                                                          

procedure _TerminateNLM; cdecl; external '!clib' name '_TerminateNLM';

// This is the main program (not loader) Entry-Point that will be called by netware    
// it sets up the argc and argv and calls _nlm_main (in system.pp)
//

procedure _Stop; forward;

procedure _pasStart; assembler; export; [alias:'_pasStart_'];
asm
    pushl	$_nlm_main
    call	_SetupArgV_411
    addl	$4,%esp
    ret
// this is a hack to avoid that FPC_NW_CHECKFUNCTION will be
// eleminated by the linker (with smartlinking)
    call	FPC_NW_CHECKFUNCTION
    call	_Stop
end;

//.data
//# argc is defined in the novell nwpre, i assume it is not needed
//#_argc:
//#	.long	0

// structure needed by clib
type kNLMInfoT =
   packed record
      Signature      : ARRAY [0..3] OF CHAR;	// LONG 'NLMI'
      Flavor         : longint;		// TRADINIONAL_FLAVOR = 0
      Version        : longint;		// TRADINIONAL_VERSION = 0, LIBERTY_VERSION = 1
      LongDoubleSize : longint;		// gcc nwpre defines 12, watcom 8
      wchar_tSize    : longint;
    end;
//    .globl	_kNLMInfo		# will be used as data start
var _kNLMInfo:kNLMInfoT = (Signature:'NLMI';Flavor:0;Version:1;LongDoubleSize:8;wChar_tSize:2);
//	.ascii	"NLMI"
//	.long	0,1,8,2
//	

//.data 
//  __uninitializedDataSize:	.long
var  __uninitializedDataSize:longint;

//
// this will be called by the loader, we pass the address of _pasStart_ and
// _kNLMInfo (needed by clib) and netware is doing the work
//
//    .globl	_Prelude
procedure _Prelude; assembler; export; [alias:'_Prelude'];
asm
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
   	pushl	$_pasStart
   	pushl	$_kNLMInfo
   	pushl	%eax
     	movl	0x24(%ebp),%edx  // 1b7f6
   	pushl	%edx
       	pushl	%ecx  
   	pushl	%ebx
       	pushl	%esi			// uninitialized data size
       	pushl	%edi
     	movl	0x10(%ebp),%edx
   	pushl	%edx
     	movl	0xc(%ebp),%edx
   	pushl	%edx
     	movl	0x8(%ebp),%edx
 	pushl	%edx
       	call	_StartNLM
	test	%eax,%eax
    	jne	.Lx1
    	xorl	%eax,%eax		// dont know why this is needed ?
.Lx1:
     	lea	0xfffffff4(%ebp),%esp
   	popl	%ebx
       	popl	%esi
       	popl	%edi
    	movl	%ebp,%esp
   	popl	%ebp
   	ret
end;

//#
//# the global stop-function
//#
//    .globl	_Stop
procedure _Stop; assembler; [alias:'_Stop'];
asm
	pushl	$0x5			// TERMINATE_BY_UNLOAD=0, TERMINATE_BY_EXTERNAL_THREAD=0
	pushl	$0x0
       	movl	_kNLMInfo,%edx
       	pushl	%edx
       	call	_TerminateNLM
    	addl	$0x0c,%esp
       	ret
end;


//.text
procedure __getTextStart; assembler; export; [alias:'__getTextStart'];
asm
//__getTextStart:
//    movl    $.text,%eax
    movl    $_pasStart,%eax		// should be the start in .text, dont know how to access .text in the internal assembler
    ret
end;

procedure _DataStart; external name '.data';

procedure __getDataStart; assembler; export; [alias:'_getDataStart'];
asm
//__getDataStart:
//    movl    $.data,%eax
    movl  $_kNLMInfo, %eax
    ret
end;


procedure __getBssStart; assembler; export; [alias:'__getBssStart'];
asm
//__getBssStart:
//    movl    $.bss,%eax
    movl    $__uninitializedDataSize,%eax
    ret
end;

//.text
procedure __getUninitializedDataSize; assembler; export; [alias:'__getUninitializedDataSize'];
asm
//__getUninitializedDataSize:
    movl   __uninitializedDataSize, %eax
    ret
end;


end.

// DLL Startup code for WIN32 port of Free Pascal
// Written by P.Ozerski 16.10.1998
     .text
     .globl _mainCRTStartup
_mainCRTStartup:
     movb $1,U_SYSTEM_ISCONSOLE
     jmp .LDLL_Entry
     .globl _WinMainCRTStartup
_WinMainCRTStartup:
     movb $0,U_SYSTEM_ISCONSOLE
.LDLL_Entry:
     pushl    %ebp
     movl     %esp,%ebp
     pushl    %ebx
     pushl    %esi
     pushl    %edi
     movl     8(%ebp),%edi
     movl     %edi,U_SYSTEM_HINSTANCE
     movl     12(%ebp),%edi
     movl     %edi,U_SYSTEM_DLLREASON
     movl     16(%ebp),%edi
     movl     %edi,U_SYSTEM_DLLPARAM
     call     _FPC_DLL_Entry
     popl     %edi
     popl     %esi
     popl     %ebx
     popl     %ebp
     ret      $12
//
// $Log$
// Revision 1.3  2002-07-28 20:43:51  florian
//   * several fixes for linux/powerpc
//   * several fixes to MT
//
//

//Startup code for WIN32 port of Free Pascal
//Written by P.Ozerski 1998
// modified by Pierre Muller
     .text
     .globl _mainCRTStartup
_mainCRTStartup:
     movb   $1,U_SYSTEM_ISCONSOLE
     call   _FPC_EXE_Entry
     .globl _WinMainCRTStartup
_WinMainCRTStartup:
     movb   $0,U_SYSTEM_ISCONSOLE
     call   _FPC_EXE_Entry

//
// $Log$
// Revision 1.3  2002-07-28 20:43:51  florian
//   * several fixes for linux/powerpc
//   * several fixes to MT
//
//

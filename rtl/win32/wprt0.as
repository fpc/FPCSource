//Startup code for WIN32 port of FPK-Pascal 0.9.98
//Written by P.Ozerski
//1998
// modified by Pierre Muller
     .text
     .globl _mainCRTStartup
_mainCRTStartup:
     movb   $1,U_SYSWIN32_ISCONSOLE
     call   _FPC_EXE_Entry
     .globl _WinMainCRTStartup
_WinMainCRTStartup:
     movb   $0,U_SYSWIN32_ISCONSOLE
     call   _FPC_EXE_Entry
 

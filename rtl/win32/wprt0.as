//Startup code for WIN32 port of FPK-Pascal 0.9.98
//Written by P.Ozerski
//1998
// modified by Pierre Muller
     .text
     .GLOBL _mainCRTStartup
_mainCRTStartup:
     movl   $1,U_SYSWIN32_ISCONSOLE
     jmp    FPC_EXE_Entry
.GLOBL _WinMainCRTStartup
_WinMainCRTStartup:
     movl   $0,U_SYSWIN32_ISCONSOLE
     jmp    FPC_EXE_Entry
 

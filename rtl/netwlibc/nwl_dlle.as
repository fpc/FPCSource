//for a non-library we should not have DllMain
//link this only for libraries
     .text
     .globl DllMain
DllMain:
     jmp    _FPC_DLL_Entry

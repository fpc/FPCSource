//for a library we should not have main
//link this only for non libraries
     .text
     .globl main
main:
     jmp    _FPC_NLM_Entry

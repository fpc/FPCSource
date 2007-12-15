{ %CPU=i386 }
{ %OPT=-Cg- }
{ Old file: tbs0228.pp }
{ Asm, wrong warning for size                          OK 0.99.11 (PFV) }

PROGRAM Buggy;

{$ASMMODE ATT}

PROCEDURE XX;  ASSEMBLER;
TYPE
  TabType=ARRAY[0..3] OF BYTE;
CONST
  TabCent   : TabType = (0,6,4,2);
ASM
    movzbl TabCent(,%eax),%ebx
END;

BEGIN
END.

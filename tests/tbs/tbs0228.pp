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

{ Source provided for Free Pascal Bug Report 1996 }
{ Submitted by "Louis Jean-Richard" on  2002-06-03 }
{ e-mail: Ljean_richard@compuserve.com }
PROGRAM RangeDefinition;
TYPE
        codeIndex       = 5 .. 287;
CONST
        noCodeIndex     = HIGH(codeIndex) + 1;
        valuereal      =  14.5 + 13.4;
CONST
        noCodeIndexBis  = SUCC(HIGH(codeIndex));
TYPE
        codePointer     = 0 + LOW(codeIndex) .. noCodeIndex;
TYPE
        codePointerBis  = LOW(codeIndex) .. noCodeIndex;
var
 Code : CodeIndex;
BEGIN
        code     := 1 + LOW(codeIndex);
END
.

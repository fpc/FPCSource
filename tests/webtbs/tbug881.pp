PROGRAM TEST;
TYPE
    byteSet     = SET OF 0..7;
    booleanArray    = ARRAY[0..HIGH(word) DIV 8] OF byteSet;
    booleanArrayPointer = ^booleanArray;

PROCEDURE SetBooleanArray(  CONST p     : booleanArrayPointer;
                            CONST index : word );
BEGIN
    INCLUDE(p^[index DIV 8],index MOD 8)
END;

BEGIN
END.

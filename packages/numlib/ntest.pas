
TYPE ArbFloat=extended; {Any float, sensible values are Real Or extended}
     ArrayByte= ARRAY[0..SIZEOF(ArbFloat)-1] OF BYTE;
     Float10Arb=ArrayByte;

PROCEDURE DisplayBinValue(Flt : ArbFloat);

VAR      FltArr : ArrayByte;
         I      : LONGINT;

BEGIN
 Writeln(Flt);
 Move(Flt,FltArr,SIZEOF(ArbFloat));
 FOR I:=0 TO SIZEOF(ArbFloat)-1 DO
  BEGIN
   Write(FltArr[I]);
   IF I<> (SIZEOF(ArbFloat)-1 ) THEN
    Write(', ');
  END;
 Writeln;
END;

FUNCTION  CreateFloat(Sign:BOOLEAN;Mantissa1:comp;Exponent:LONGINT):ArbFloat;

VAR L : ArrayByte;

BEGIN
 FillChar(L,8,#0);
 IF Sign THEN
  L[SIZEOF(ArbFloat)-1]:=128;

END;
CONST
    ETC1 : Float10Arb = (0,0,$00,$00,$00,$00,0,128,192,63);
    ETC2 : Float10Arb = ($FF,$FF,$FF,$FF,$FF,$FF,$FF,$D6,$FE,127);
    ETC3 : Float10Arb = (1,0,0,0,0,0,0,0,0,0);
    ETC4 : Float10Arb = (0,0,0,0,0,0,0,0,$F0,$7F);
    ETC5 : Float10Arb = (0,0,0,0,0,0,0,$80,$F0,$7F);

VAR L:ArbFloat;

BEGIN
{ writeln(extended(ETC2));
 L:=ln(extended(ETC2));
 DisplayBinValue(L);
 writeln(extended(ETC3));
 L:=ln(extended(ETC3));
 DisplayBinValue(L);}
 L:=extended(ETC4);
 writeln(L);
 DisplayBinValue(L);
 L:=extended(ETC5);
 writeln(L);
 DisplayBinValue(L);

END.

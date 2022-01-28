program tutf8charconst;

{$mode objfpc}{$H+}

const
  FULLWIDTH_RIGHT_PARENTHESIS: UTF8String = #$FF09;

var
  UFFFF: UTF8String;
begin
  if not((Length(FULLWIDTH_RIGHT_PARENTHESIS) = 3) and
         (Ord(FULLWIDTH_RIGHT_PARENTHESIS[1]) = $EF) and
         (Ord(FULLWIDTH_RIGHT_PARENTHESIS[2]) = $BC) and
         (Ord(FULLWIDTH_RIGHT_PARENTHESIS[3]) = $89)) then
    Halt(1);

  UFFFF := #65535;
  if not((Length(UFFFF) = 3) and
         (Ord(UFFFF[1]) = $EF) and
         (Ord(UFFFF[2]) = $BF) and
         (Ord(UFFFF[3]) = $BF)) then
    Halt(2);
end.

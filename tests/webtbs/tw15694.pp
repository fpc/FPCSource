type
 TEn=array [0..63] of boolean;
var
  WlEnb:array [0..511] of TEn;

procedure ReadMask;
var
  I,ICd,J:ptrint;
  b: boolean;
begin
  for ICd:=0 to 10 do
    begin
      J:=icd;
{$R-}
      for I:=0 to 3 do
        begin
          WlEnb[ICd,I]:=J and $01<>0;
          b:=J and $01<>0;
          if WlEnb[ICd,I]<>b then
            halt(1);
          J:=J shr 1;
       end;
{$R+}
    end;
end;

begin
  readmask
end.

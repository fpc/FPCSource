{ test by Graeme Geldenhuys }

{$mode delphi}
uses sysutils;


procedure test;
var
 Result: string;
 e: extended;
 r: double;
begin
 DecimalSeparator:='.';
 e := 234.502;
 Result := FloatToStrF(e, ffGeneral, 15, 0);
// Memo1.Lines.Add(Result);      { prints 234.502  }
 writeln(result);
 if (result <> '234.502') then
   halt(1);

 r := 234.502;
 Result := FloatToStrF(r, ffGeneral, 15, 0);
// Memo1.Lines.Add(Result);  { prints 234.50200000000001 }
 writeln(result);
 if (result <> '234.502') then
   halt(1);

 r := 234.501;
 Result := FloatToStrF(r, ffGeneral, 15, 0);
// Memo1.Lines.Add(Result);  { prints 234.501  Why does this work? }
 writeln(result);
 if (result <> '234.501') then
   halt(1);

 r := 7.502;
 Result := FloatToStrF(r, ffGeneral, 15, 0);
// Memo1.Lines.Add(Result);  { prints 7.502 }
 writeln(result);
 if (result <> '7.502') then
   halt(1);

 r := 8.502;
 Result := FloatToStrF(r, ffGeneral, 15, 0);
// Memo1.Lines.Add(Result);  { prints 8.502000000000001 }
 writeln(result);
 if (result <> '8.502') then
   halt(1);

 e:=0.005;
 str(e:0:2,result);
 writeln(result);
 if (result<>'0.01') then
   halt(1);
end;

begin
  test;
end.

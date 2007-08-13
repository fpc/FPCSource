{$codepage utf8}

{ Source provided for Free Pascal Bug Report 4254 }
{ Submitted by "rimga" on  2005-08-04 }
{ e-mail: rimga@ktl.mii.lt }
//when source encoded in utf8
var
  p: pchar;
begin
  p:= 'abc'#261;
  //constant is internally treated as of wide chars a0b0c0#105 (it is ok)
  //but assigning it to pchar it has to be converted to 8-bit chars
  if strlen(p)= 1 then
    begin
    writeln('problem');
    halt(1);
    end;
end.

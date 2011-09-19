{$CODEPAGE cp850}
program tcptypedconst;

type
  Str_cp = type AnsiString(1251);
  Str_cp850 = type AnsiString(850);
  
procedure printcontent(p : Pointer; l: integer);
var
  i : Integer;
  pb : PByte;
begin
  writeln;
  pb := p;
  for i := 1 to l do begin
    writeln('s[',i,']= ',pb^);
    inc(pb);
  end;
end;  
  
const
  c : Str_cp = #163#165#167#174#181#224; 
  c850 : Str_cp850 = #163#165#167#174#181#224; 
begin
  printcontent(@(c[1]),Length(c));
  printcontent(@(c850[1]),Length(c850));
  WriteLn(c);
  WriteLn(c850);
end.

{$CODEPAGE cp437}
program tcptypedconst2;
   
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
   CFrame      = #1#1#2#2#3;  
   P: String[Length(CFrame)] = CFrame;   
begin   
  printcontent(@(P[1]),Length(p));
end.
program arcrash;

{$mode objfpc}{$H+}

type
  Trec = record
    Signature: array of Integer;
    s: ansistring;
  end;

var
  M: array of Trec;
  s2: ansistring;

begin
  SetLength(M,2);
  SetLength(M[0].Signature,4);
  SetLength(M[1].Signature,4);
  setlength(m[0].s,2);
  s2:=m[0].s;
  WriteLn(Length(M[0].Signature), ' ', Length(M[1].Signature));
  writeln(length(m[0].s));
  M[0].Signature := M[0].Signature;
  m[0].s:=m[0].s;
  WriteLn(Length(M[0].Signature), ' ', Length(M[1].Signature));
  writeln(length(m[0].s));
  s2:='';
  if (Length(M[0].Signature) <> 4) then
    halt(1);
  if (Length(M[0].s) <> 2) then
    halt(2);
end.


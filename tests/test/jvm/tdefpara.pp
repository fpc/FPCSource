program tdefpara;

{$mode delphi}

{$ifdef cpujvm}
uses
  {$ifdef java}jdk15{$else}androidr14{$endif};

{$macro on}
{$define writeln:=jlsystem.fout.println}
{$endif}


type
  tc = class
    fa: longint;
    constructor create(a: longint = 1234);
  end;

  tc2 = class(tc)
  end;

constructor tc.create(a: longint = 1234);
begin
  fa:=a;
end;

var
  c: tc;
begin
  c:=tc2.create;
  if c.fa<>1234 then
    raise jlexception.create('wrong overload');
end.

program tnestedset;

{$mode delphi}

uses
  jdk15;

type
  tnestedfpstructenum = (ea,eb,ec);
  tnestedfpstructenumset = set of tnestedfpstructenum;

procedure test(var s: tnestedfpstructenumset);

 procedure sub;
 begin
   s:=s+[eb];
 end;


begin
  sub
end;

var
  s: tnestedfpstructenumset;
begin
  test(s);
  if s<>[eb] then
    raise jlexception.create;
  jlsystem.fout.println(jlstring(juenumset(@s).toString));
end.

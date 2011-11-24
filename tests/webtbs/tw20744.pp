{ %opt=-gh }

program tt;

type
  pstring = ^string;
var
  s: string;
  ps: pstring;
  as: ansistring;
  us: unicodestring;
  ws: widestring;
begin
  HaltOnNotReleased := true;
  s:='abc';
  ps:=@s;
  writestr(s,ps^,1,s,2,s);
  writeln(s);
  if s<>'abc1abc2abc' then
    halt(1);

  as:='de';
  as:=as+'f';
  writestr(as,as,3,as,4,as);
  writeln(as);
  if as<>'def3def4def' then
    halt(2);


  us:='de';
  us:=us+'f';
  writestr(us,us,3,us,4,us);
  writeln(as);
  if us<>'def3def4def' then
    halt(3);


  ws:='de';
  ws:=ws+'f';
  writestr(ws,ws,3,ws,4,ws);
  writeln(ws);
  if ws<>'def3def4def' then
    halt(4);
end.

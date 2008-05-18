{ %opt=-gl }
{$ifdef unix}
uses
  dl;
{$endif unix}

procedure p(var a : pointer);external 'tlib1a' name 'p';

var
  s1,s2 : string;
  l : longint;
  a : pointer;
begin
  p(a);
  GetLineInfo(PtrUInt(a),s1,s2,l);
  writeln('Func: ',s1,' Source: ',s2,' Line: ',l);
  if (s1<>'P') or (s2<>'tlib1a.pp') or (l<>9) then
    halt(1);
  writeln('ok');
end.

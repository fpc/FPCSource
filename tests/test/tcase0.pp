{$mode objfpc}{$h+}

// A basic test for 'case-of-string' with embedded zeroes
var
  s: string;
  ss: shortstring;
  ws: widestring;
  us: unicodestring;
  i: integer;

begin
  i:=15;

  s:='aa'#0'bb';  
  case s of  
    'aa'#0'aa' .. 'aa'#0'cc': i:=i and (not 1);
  end;

  ss:='aa'#0'bb';
  case ss of
    'aa'#0'aa' .. 'aa'#0'cc': i:=i and (not 2);  
  end;
  
  ws:='aa'#0'bb';
  case ws of
    'aa'#0'aa' .. 'aa'#0'cc': i:=i and (not 4);
  end;
  
  us:='aa'#0'bb';
  case us of
    'aa'#0'aa' .. 'aa'#0'cc': i:=i and (not 8);
  end;

  if i=0 then
    writeln('ok');
  Halt(i);  
end.

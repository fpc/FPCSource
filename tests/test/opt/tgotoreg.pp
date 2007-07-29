{ %OPT=-Ooregvar }

{$goto on}

var
  global: longint;

procedure t1;
begin
  { make sure it's not put in a register in t }
  global := 1;
end;

procedure t;
var
  l: longint;
  s1,s2,s3: ansistring;
label lab;
begin
  t1;
  l := 1;
  s1 := 'ABC';
  s2 := 'DEF';
  s3 := '';
lab:
  inc(l);
  inc(global);
  if global > 10 then
    halt(1);
  if l = 10 then
    s3 := 'ABCDEF';
  if s1+s2 <> s3 then
    goto lab;
end;

begin
  t;
end.
 

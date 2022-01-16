{$mode objfpc}

function comparearr(const a,b: array of byte; len: integer): Boolean;
var
  i : integer;
begin
  for i:=0 to len-1 do
    if a[i]<>b[i] then begin
      Result:=false;
      Exit;
    end;
  Result:=true;
end;

procedure printarr(const a: array of byte);
var
  i : integer;
begin
  for i:=0 to length(a)-1 do write(a[i],' ');
  writeln;
end;

const
  size_cnt = 8;
  size_inc = 20;
  size_dec = 4;

var
  a: array of byte;
  b: array of byte;
  i: integer;
  r: integer;
begin
  SetLength(a, size_cnt);
  for i:=0 to length(a)-1 do a[i]:=$F0+i;

  // test decrease size
  // match, by less size
  b:=a;
  SetLength(b, size_dec);
  if not comparearr(a,b,length(b))  then
    halt(1);

  // test same size/copy
  // full match
  b:=a;
  SetLength(b, length(b));
  if not comparearr(a,b,length(b)) then
    halt(1);

  // test increase size
  // first part must match, last part must be zero
  b:=a;
  SetLength(b, size_inc);
  if not comparearr(a,b,length(a)) then
    halt(1);
  r:=1;
  for i:=length(a) to length(b)-1 do
    if b[i]<>0 then begin r:=0; halt(1) end;
  writeln('ok');
end.

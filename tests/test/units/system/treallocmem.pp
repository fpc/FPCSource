{ ReAllocMem must return the new pointer also if the block is resized in place,
  the string helpers depend on it }
program treallocmem;

procedure error(nr: longint);
  begin
    writeln('error ',nr);
    halt(1);
  end;

var
  p, q, r: pointer;
  i: longint;
  s: ansistring;

begin
  { same size after alignment }
  getmem(p,10);
  q:=reallocmem(p,11);
  if q<>p then
    error(1);
  { shrinking }
  q:=reallocmem(p,4);
  if q<>p then
    error(2);
  { growing, the block after it is free }
  q:=reallocmem(p,100);
  if q<>p then
    error(3);
  { growing, the block after it is used }
  getmem(r,10);
  q:=reallocmem(p,1000);
  if q<>p then
    error(4);
  freemem(r);
  freemem(p);
  { nil and size 0 }
  p:=nil;
  q:=reallocmem(p,10);
  if (q<>p) or (p=nil) then
    error(5);
  q:=reallocmem(p,0);
  if (q<>nil) or (p<>nil) then
    error(6);

  { strings which are resized in place }
  s:='';
  for i:=1 to 100 do
    s:=s+chr(ord('a')+i mod 26);
  for i:=1 to 100 do
    if s[i]<>chr(ord('a')+i mod 26) then
      error(10);
  setlength(s,50);
  setlength(s,60);
  for i:=1 to 50 do
    if s[i]<>chr(ord('a')+i mod 26) then
      error(11);

  writeln('ok');
end.

program tw4080;
{$i+}

{$ifdef unix}
uses
  {$ifdef darwin}iosxwstr{$else}cwstring{$endif};
{$endif unix}

var
  S, S2 : array [1..15] of char;
  f: text;
  f2: file;
  l: longint;
  str: shortstring;
  astr: ansistring;
  wstr: widestring;
begin
  S := 'string1'#0'string2';
  assign(f,'tw4080.out');
  rewrite(f);
  write (f,S);
  close(f);
  assign(f2,'tw4080.out');
  reset(f2,1);
  if (filesize(f2) <> 15) then
    halt(1);
  blockread(f2,s2,sizeof(s2));
  close(f2);
  erase(f2);
  for l := low(s) to high(s) do
    if s[l] <> s2[l] then
      halt(1);

  str := s;
  for l := low(s) to high(s) do
    if s[l] <> str[l] then
      halt(1);

  astr := s;
  for l := low(s) to high(s) do
    if s[l] <> astr[l] then
      halt(1);
  wstr := s;
  for l := low(s) to high(s) do
    if s[l] <> wstr[l] then
      halt(1);
end.



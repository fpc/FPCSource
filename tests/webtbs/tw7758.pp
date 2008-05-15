{$codepage utf8}

uses
{$ifdef unix}
  cwstring,
{$endif}
  sysutils;

const
  cwc=widechar('a');
  c2=widechar('é');
  c3=widestring('é');
var
  c: char;
  wc,wc2: widechar;
  s,s2,a: ansistring;
  w: widestring;
  ss: shortstring;
begin
  c:=#0;
  w:=c;
  if (length(w)<>1) or
     (w[1]<>#0) then
    halt(1);
  s:='é';
  w:=s;
  wc:=w[1];
  s2:=wc;
  if (w <> s2) or
     (s <> s2) then
    halt(2);

  c:=#0;
  wc:=c;
  c:=wc;
  if (c<>#0) or
     (wc<>#0) then
    halt(5);
  ss:=wc;
  wc:=ss[1];
  if (length(ss)<>1) or
     (ss[1]<>#0) or
     (wc<>#0) then
    halt(6);
  a:=wc;
  wc:=a[1];
  if (length(a)<>1) or
     (a[1]<>#0) or
     (wc<>#0) then
    halt(7);

  c:='a';
  wc:=c;
  c:=wc;
  if (c<>'a') or
     (wc<>'a') then
    halt(8);
  ss:=wc;
  wc:=ss[1];
  if (length(ss)<>1) or
     (ss[1]<>'a') or
     (wc<>'a') then
    halt(9);
  a:=wc;
  wc:=a[1];
  if (length(a)<>1) or
     (a[1]<>'a') or
     (wc<>'a') then
    halt(10);

  wc2:=cwc;
  if (wc2<>'a') or
     (wc2<>cwc) then
    halt(3);
  ss:=cwc;
  if (length(ss)<>1) or
     (ss[1] <> 'a') then
    halt(4);
  c:=cwc;
  if (c<>'a') or
     (c<>cwc) then
    halt(13);
  w:=cwc;
  if (length(w)<>1) or
     (w[1] <> 'a') then
    halt(11);
  s:=cwc;
  if (length(s)<>1) or
     (s[1] <> 'a') then
    halt(12);


  wc:=c2;
  c:=c2;
  wc2:=c;
  if ((c<>c2) and
      (c<>'?')) or
     (wc<>c2) or
     ((wc2<>c2) and
      (wc2<>'?')) then
    halt(14);
  ss:=c2;
  w:=ss;
  wc:=w[1];
  if (length(w)<>1) or
     (w[1]<>c2) or
     (wc<>c2) then
    halt(15);
  a:=c2;
  w:=a;
  wc:=w[1];
  if (length(w)<>1) or
     (w[1]<>c2) or
     (wc<>c2) then
    halt(16);

  ss:=c3;
  w:=ss;
  wc:=w[1];
  if (length(w)<>1) or
     (wc <> c2) then
    halt(17);
  c:=c3[1];
  if ((c<>c2) and
      (c<>'?')) then
    halt(18);
  w:=c3;
  if (length(w)<>1) or
     (w[1] <> c2) then
    halt(19);
  s:=c3;
  w:=s;
  if (length(w)<>1) or
     (w[1] <> c2) then
    halt(20);
  ss:=c3;
  w:=ss;
  if (length(w)<>1) or
     (w[1] <> c2) then
    halt(21);

{$ifdef dummy}
  wc:=c2;
  writestr(s,wc);
  w:=s;
  if (length(w)<>1) or
     (w[1]<>c2) then
    halt(22);
{$endif}
end.

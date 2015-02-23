{ %skiptarget=win32,win64,wince,os2,emx }
{ This test is only useful if the local codepage is utf-8 which
  usually not the case on windows (and never can be the case on OS/2)
}
{$codepage utf-8}

{$mode objfpc}

uses
{$ifdef unix}
  {$ifdef darwin}iosxwstr{$else}cwstring{$endif},
{$endif}
  SysUtils;

{$i+}

var
  t: text;
  w: widestring;
  u: unicodestring;
  a: ansistring;
  wc: widechar;

begin
  assign(t,'twide3.txt');
  rewrite(t);
  writeln(t,'łóżka');
  close(t);
  reset(t);
  
  try
    read(t,wc);
    if wc<>'ł' then
      raise Exception.create('wrong widechar read: '+inttostr(ord(wc))+'<>'+inttostr(ord('ł')));
  except
    close(t);
//    erase(t);
    raise;
  end;
    
  reset(t);
  try
    readln(t,a);
    w:=a;
    if (w<>'łóżka') then
      raise Exception.create('wrong ansistring read');
  except
    close(t);
    erase(t);
    raise;
  end;

  reset(t);
  try
    readln(t,w);
    if (w<>'łóżka') then
      raise Exception.create('wrong widestring read');
  except
    close(t);
    erase(t);
    raise;
  end;

  reset(t);
  try
    readln(t,u);
    if (u<>'łóżka') then
      raise Exception.create('wrong unicodestring read');
  finally
    close(t);
    erase(t);
  end;

  readstr(u,a);
  if u<>a then
    raise Exception.create('wrong readstr(u,a)');
  readstr(w,a);
  if w<>u then
    raise Exception.create('wrong readstr(w,a)');
end.

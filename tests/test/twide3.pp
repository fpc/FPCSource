{ %skiptarget=win32,win64,wince }
{ This test is only usefull if the local codepage is utf-8 which
  usually not the case on windows
}
{$codepage utf-8}

{$mode objfpc}
uses
{$ifdef unix}
  cwstring,
{$endif}
  sysutils;

{$i+}

var
  t: text;
  w: widestring;
  a: ansistring;

begin
  assign(t,'twide3.txt');
  rewrite(t);
  writeln(t,'łóżka');
  close(t);
  reset(t);
  try
    readln(t,a);
    w:=a;
    if (w<>'łóżka') then
      raise Exception.create('wrong string read');
  finally
    close(t);
    erase(t);
  end;
end.

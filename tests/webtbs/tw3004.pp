{ Source provided for Free Pascal Bug Report 3004 }
{ Submitted by "Michalis Kamburelis" on  2004-03-04 }
{ e-mail: michalis@camelot.homedns.org }
{$mode OBJFPC}
{$H+}
{ $mode DELPHI}

uses erroru,SysUtils;

  procedure P;
  var s:string;
  begin
   s:='blah';
   raise Exception.Create('foo ' +s);
  end;

procedure p1;
var
  i : sizeint;
begin
 i:=0;
 domem(i);
 try
  P;
 except
 end;
 if domem(i)<>0 then
   begin
     writeln('Memleak');
     halt(1);
   end;
end;

begin
  p1;
end.

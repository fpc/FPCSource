{ Source provided for Free Pascal Bug Report 3004 }
{ Submitted by "Michalis Kamburelis" on  2004-03-04 }
{ e-mail: michalis@camelot.homedns.org }
{$mode OBJFPC}
{$H+}
{ $mode DELPHI}

uses SysUtils;

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
 i:=memavail; 
 try
  P;
 except
 end;
 if i<>memavail then
   begin
     writeln('Memleak');
     halt(1);
   end;
end;

begin
  p1;
end.
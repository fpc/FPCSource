{ %result=217 }

{ Source provided for Free Pascal Bug Report 2229 }
{ Submitted by "Vincent Snijders" on  2002-11-14 }
{ e-mail: vslist@zonnet.nl }
program reraise;

{$mode objfpc}
{$H+}

uses
  SysUtils;

procedure raiseexception;
var
  x: integer;
begin
  try
    x := 1;
    raise Exception.Create('Bug?');
  except
    on E: Exception  do
    begin
      if  x=1 then
      //begin
        raise
      //end
      else writeln('Do nothing')
    end
  end;
end;

begin
  raiseexception;
end.

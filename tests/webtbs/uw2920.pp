{ Source provided for Free Pascal Bug Report 2920 }
{ Submitted by "marco (Gory Bugs Department)" on  2004-01-27 }
{ e-mail:  }
unit uw2920;

interface

{$mode Delphi}
{$H+}

type myclass= class
                function bb:string; virtual;
                end;

implementation

function myclass.bb:string;

begin
  bb:='a';
end;

begin
end.

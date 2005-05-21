{ Source provided for Free Pascal Bug Report 3356 }
{ Submitted by "Vincent Snijders" on  2004-10-12 }
{ e-mail: vslist@zonnet.nl }
program project1;

{$mode objfpc}
uses
  uw3356;

type
  TB = class(TA)
  protected
    procedure DoB;
  end;

procedure TB.DoB;
var
  FA: integer;
begin

end;

begin
end.

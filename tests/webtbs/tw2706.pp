{ Source provided for Free Pascal Bug Report 2706 }
{ Submitted by "Johannes Berg" on  2003-10-01 }
{ e-mail: johannes -at- sipsolutions -dot- de }
program j;

{$mode delphi}

uses
  uw2706b, uw2706a;

type
  TClassB = class(TClassA)
  end;
  TClassC = class
    FB: TClassB;
    procedure Test;
  end;

procedure TClassC.Test;
var
  LX: TX;
begin
  FB.CT(LX);
end;

begin
end.

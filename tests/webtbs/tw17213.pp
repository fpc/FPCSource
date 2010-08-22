program openarrayparam;
{$ifdef FPC}{$mode objfpc}{$h+}{$endif}
{$ifdef mswindows}{$apptype console}{$endif}
uses
 sysutils;
type
 testflagty = (tf_1,tf2);
 testflagsty = set of testflagty;

procedure testproc(const testpar: array of testflagsty);
begin
end;

begin
 testproc([[],[],[]]);
end.

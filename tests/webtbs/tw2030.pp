{ Source provided for Free Pascal Bug Report 2030 }
{ Submitted by "Michalis Kamburelis" on  2002-07-06 }
{ e-mail: mkambi@poczta.onet.pl }
program wrong_delphi_overloads_handling;

{$mode delphi}
{under objfpc mode there will be no bug}

const
  err : boolean = true;

procedure p;            overload; forward;
procedure p(a:integer); overload; forward;

{ it doesn't matter if we add clause "overload"
  to p's definitions below; the error will still
  prevent this code from compiling }

procedure p;
begin
  err:=false;
end;

procedure p(a:integer); overload;
begin end;

begin
  p;
  if err then
   halt(1);
end.

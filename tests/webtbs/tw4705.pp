{ Source provided for Free Pascal Bug Report 4705 }
{ Submitted by "Phil H." on  2006-01-17 }
{ e-mail: pjhess@purdue.edu }
program TestVarCase;

uses
  SysUtils,
  Variants;

var
  AVar  : Variant;
  e : (e1,e2);

begin
  AVar := 1;
  case AVar of
    1 : halt(0);
  end;
  halt(1);
end.


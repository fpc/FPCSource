{ Source provided for Free Pascal Bug Report 4093 }
{ Submitted by "alphax" on  2005-06-16 }
{ e-mail: acmui_2004@163.com }
program test_var_dt;

{$IFDEF FPC}
  {$MODE OBJFPC}
{$ENDIF}

uses
  SysUtils, Variants;

var
  DT: TDateTime;
  S: string;
   V: Variant;
   C: Currency;
begin
  V := '1.0';
  C := V;
  WriteLn(CurrToStr(C));

  S := DateTimeToStr(Now());
  V := S;
  writeln(v);
  DT := V;
  WriteLn(DateTimeToStr(DT));
end.

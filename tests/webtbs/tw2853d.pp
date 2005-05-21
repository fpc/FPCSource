{ OPT= -dloop }
{ bug report variant, check that it works in
  TP mode , added by Pierre }
{ Source provided for Free Pascal Bug Report 2853 }
{ Submitted by "Bj”rn Hendriks" on  2003-12-18 }
{ e-mail: bjoern.hendriks.ext_ese@ts.siemens.de }
program test;

begin
   WriteLn('abc');
{$ifdef LOOP}
   WriteLn('def');
{ {}
  {$endif, this endif should be ignored,
  as it is inside a comment}
  (* *)
}
   WriteLn('ghi');
{$endif LOOP}
   WriteLn('test');
end.

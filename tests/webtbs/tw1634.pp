{ Source provided for Free Pascal Bug Report 1634 }
{ Submitted by "Igor Grigoriev" on  2001-10-06 }
{ e-mail: igor@sch57.msk.ru }
{$DEFINE A}
program test;

{$IFNDEF A}
  procedure p1;
  begin
   (*{$IFDEF B}  {$ENDIF}*)
  end;
{$ENDIF}

BEGIN
END.
// The above program gives error:
// test.pas(13) Fatal: Unexpected end of file
// But comment line changed to:
// (* {$IFDEF B}  {$ENDIF}*)
//   ( space after '(*' added )
//gives successful compilation.

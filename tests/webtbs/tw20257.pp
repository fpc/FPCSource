{$APPTYPE CONSOLE}
{$IFDEF FPC}
 {$MODESWITCH RESULT} // to avoid "BOOL(constant)" typecasts and stay compilable by Delphi & FPC
{$ENDIF}
program fpc_vs_delphi_bool_compatibility;

(**********************************************************
 
  TEST STUB for the real function from Windows API

 **********************************************************)
type  BOOL = longbool; {to avoid linking to WINDOWS unit}
      INT  = longint;  {to avoid linking to WINDOWS unit}
      TExpectedResult=(R_VISIBLE,R_INVISIBLE,R_BAD_PARAM);

function PtVisible(test_return:TExpectedResult):BOOL;
(*

 MSDN definition:
~~~~~~~~~~~~~~~~~~
 The PtVisible function determines whether the specified point is within the clipping region of a device context. 
 
 BOOL PtVisible(
   HDC hdc, // handle to DC
   int X,   // x-coordinate of point
   int Y    // y-coordinate of point
 );

 Return Values:

 If the specified point is within the clipping region of the device context, the return value is TRUE(1).

 If the specified point is not within the clipping region of the device context, the return value is FALSE(0).

 If the hdc is not valid, the return value is (BOOL)-1. 

*)
begin
  case test_return of
    R_VISIBLE   :
      INT(result):= 1;
    R_INVISIBLE :
      INT(result):= 0;
    else
      INT(result):=-1;
  end;
end;

(**********************************************************
 
  Real test

 **********************************************************)
type  TBool = BOOL;
   (* TBool = boolean; {-- doesn't matter, in FPC fails as well..}*)

function test_visible(test_return:TExpectedResult;expected_result:TBool):TBool;
begin
  result:=(PtVisible(test_return)=expected_result);
end;

begin
  if test_visible(R_VISIBLE,true) then
    writeln('pass')
  else
    begin
      writeln('fail');
      halt(1);
    end;
    { Delphi: pass
      FPC:    fail }
  if (PtVisible(R_VISIBLE)>PtVisible(R_BAD_PARAM)) or
     (PtVisible(R_VISIBLE)<PtVisible(R_BAD_PARAM)) then
    begin
      { don't treat two different values for longbool as
        different if both mean "true" }
      writeln('fail 2');
      halt(2);
    end;
end.


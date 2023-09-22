program tw40324;
// This program compiles and runs in Delphi and in FPC. (at least should run in FPC)
// It is intentionally designed this way.
{$ifdef FPC}
{$mode objfpc}{$H+}
{$modeswitch functionreferences}
{$modeswitch anonymousfunctions}
  // {$warn 5036 off}// "Warning: (5036) Local variable "$Capturer" does not seem to be initialized"
{$endif}
// uses
{$IFDEF UNIX}
// cthreads,
{$ENDIF}
  // Classes, Sysutils { you can add units after this };

type
  T_X = String; // Type of Test-variable X
  TfuncS = reference to function: T_X;
  TfuncF = reference to function(s: T_X): TfuncS;

var f_inner: TfuncS;
  f_outer: TfuncF;
//------------------------------------------------------------------------------
procedure caller;
begin
  f_inner();
end;
//------------------------------------------------------------------------------
procedure main;

var X: T_X;
   // str:String;
    f_outer: TfuncF;

begin

  X := '1234';

  f_outer := function(s: T_X): TfuncS // This captures local and persistent copy of "X"
  begin
      Result := function: T_X
      begin
          Writeln(s);
          Result := s;
      end;
      Writeln('Outer function was called');
  end;
  f_inner := f_outer(X); // This instantiates the outer function and f_inner and captures their local context.

  X := '0'; // Erase the T_X content

  Writeln('now calling f_inner');
  caller(); // This line prints the T_X s=1234, which was captured by the outer function.
               // f_inner will be called from an external context, this is just for test and demonstration
end;
//------------------------------------------------------------------------------
begin
  main;
  Writeln('Now the context of "main()" is lost. Can we still print the variable "X"?');
  if f_inner() = '1234' then
    Writeln('Yes! :-)')
  else begin
    Writeln('No! :-(');
    Halt(1);
  end;

  //readln;

end.

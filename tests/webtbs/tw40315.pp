program tw40315;
//This program compiles and runs in Delphi and in FPC. (at least should run in FPC)
//It is intentionally designed this way.
//It compiles without errors or warnings in Delphi and delivers the expected result.

{$ifdef FPC}
  {$mode objfpc}{$H+}
  {$modeswitch functionreferences}
  {$modeswitch anonymousfunctions}
  // {$warn 5036 off}// "Warning: (5036) Local variable "$Capturer" does not seem to be initialized"
{$endif}
uses
  {$IFDEF UNIX}
  //cthreads,
  {$ENDIF}
  Classes,Sysutils { you can add units after this };

type  TfuncS = reference to function:String;
      TfuncF = reference to function(s:String):TfuncS;
var   f_inner: TfuncS;
      f_outer: TfuncF;

procedure caller;
begin
  f_inner();
end;

procedure main;

var str: String;
   // f_outer: TfuncF;  // <---- doesnt compile in FPC when this is uncommented, but compiles and runs ok in Delphi

begin

    str := 'Hello World!';

    f_outer := function(s:String):TfuncS //This captures local and persistent copy of "str"
    begin
      Result := function:String // <---- Access violation here, when Line "Result:=s" is commented out and when it is compiled.
      begin
        Result := s;  // <---- project1.lpr(37,9) Error: Internal error 2011010304
                      // if the line is commented out it compiles, but gives access violation at runtime

        Writeln(s);
      end;
      Writeln('Outer function was called');
    end;
    f_inner := f_outer(str);   //This instantiates the outer function and f_inner and captures their local context.

    SetLength(str,0); //Erase the string content

    Writeln('now calling f_inner');
    caller();  //This line prints the string s="Hello World!", which was captured by the outer function.
               //f_inner will be called from an external context, this is just for test and demonstration
end;

begin
  main;
  Writeln('Now the context of "main()" is lost. Can we still print the string "str"?');
  //if f_inner()='Hello World!' then writeln('Yes! :-)') else writeln ('No! :-(');
  if f_inner()<>'Hello World!' then
    Halt(1);

  //readln;
end.

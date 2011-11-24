{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2011 by the Free Pascal development team.

    Console i/o for the FPC embedded target

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
Unit consoleio;

  interface

  implementation

    procedure SysInitStdIO;
      begin
        // OpenStdIO(Input,fmInput,0);
        // OpenStdIO(Output,fmOutput,0);
        // OpenStdIO(ErrOutput,fmOutput,0);
        // OpenStdIO(StdOut,fmOutput,0);
        // OpenStdIO(StdErr,fmOutput,0);
      end;

   procedure SysFlushStdIO;
     begin
       { Make sure that all output is written to the redirected file }
{!!!!!!!!       if Textrec(Output).Mode=fmOutput then
         Flush(Output);
       if Textrec(ErrOutput).Mode=fmOutput then
         Flush(ErrOutput);
       if Textrec(stdout).Mode=fmOutput then
         Flush(stdout);
       if Textrec(StdErr).Mode=fmOutput then
         Flush(StdErr);  }
     end;

var
  ErrorBase : Pointer;external name 'FPC_ERRORBASE';

var
  pstdout : ^Text;

initialization
  { Setup stdin, stdout and stderr }
  SysInitStdIO;
finalization
  { Show runtime error and exit }
  pstdout:=@stdout;
  If erroraddr<>nil Then
   Begin
     Writeln(pstdout^,'Runtime error ',Errorcode,' at $',hexstr(erroraddr));
     { to get a nice symify }
     Writeln(pstdout^,BackTraceStrFunc(Erroraddr));
     dump_stack(pstdout^,ErrorBase);
     Writeln(pstdout^,'');
   End;
  SysFlushStdIO;
end.



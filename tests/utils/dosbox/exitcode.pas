{
  Compile with Turbo Pascal 7
  Runs a DOS program and saves its exit code in file EXITCODE.TXT
}

{$M $4000,0,0} { 16K stack, no heap }
uses
  Dos;
var
  ProgramName, CmdLine: string;
  I: Integer;
  ExitCode: LongInt;
  Txt: Text;
begin
  ProgramName := ParamStr(1);

  CmdLine := '';
  for I := 2 to ParamCount do
    CmdLine := CmdLine + ' ' + ParamStr(I);

  SwapVectors;
  Exec(ProgramName, CmdLine);
  SwapVectors;

  if DosError <> 0 then
    ExitCode := DosError shl 16
  else
    ExitCode := DosExitCode;

  Assign(Txt, 'EXITCODE.TXT');
  Rewrite(Txt);
  Write(Txt, ExitCode);
  Close(Txt);
end.

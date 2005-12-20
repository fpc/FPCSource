Uses dos;

{ Declared EXPLICITLY with Ansistring, so NO mistaking is possible }

{
  This function is VERY inefficient, but the downsize would be to
  have initialization/finalization code to get/free the environment
  settings.
}

Function Getenv (Var EnvVar  : AnsiString): AnsiString;
begin
 Getenv:=dos.GetEnv(EnvVar);
end;

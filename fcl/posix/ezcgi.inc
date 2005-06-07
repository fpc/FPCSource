Uses Dos;

{ Declared EXPLICITLY with Ansistring, so NO mistaking is possible }

Function Getenv (Var EnvVar  : AnsiString): AnsiString;

begin
  Result:=Dos.Getenv(EnvVar);
end;

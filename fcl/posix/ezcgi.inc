Uses Dos;

{ Declared EXPLICITLY with Ansistring, so NO mistaking is possible }

Function Getenv (Var EnvVar  : AnsiString): AnsiString;

begin
  Result:=Dos.Getenv(EnvVar);
end;
{
  $Log$
  Revision 1.1  2001-12-10 02:31:45  carl
  + initial version for POSIX (with thread extensions)


}

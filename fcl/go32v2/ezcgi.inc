Uses Dos;

{ Declared EXPLICITLY with Ansistring, so NO mistaking is possible }

Function Getenv (Var EnvVar  : AnsiString): AnsiString;

begin
  Result:=Dos.Getenv(EnvVar);
end;
{
  $Log$
  Revision 1.3  2000-07-22 12:27:48  jonas
    * added missing "open comment braces" for the log section

  Revision 1.2  2000/07/13 11:32:58  michael
  + removed logs

}

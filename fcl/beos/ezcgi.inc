Uses Dos;

{ Declared EXPLICITLY with Ansistring, so NO mistaking is possible }

Function Getenv (Var EnvVar  : AnsiString): AnsiString;

begin
  Result:=Dos.Getenv(EnvVar);
end;
{
  $Log$
  Revision 1.1  2001-09-28 02:19:01  carl
  + beos target


}

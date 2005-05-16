Uses Dos;

{ Declared EXPLICITLY with Ansistring, so NO mistaking is possible }

Function Getenv (Var EnvVar  : AnsiString): AnsiString;

begin
  Result:=Dos.Getenv(EnvVar);
end;
{
  $Log: ezcgi.inc,v $
  Revision 1.5  2005/02/14 17:13:12  peter
    * truncate log

}

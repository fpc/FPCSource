Uses Dos;

{ Declared EXPLICITLY with Ansistring, so NO mistaking is possible }

Function Getenv (Var EnvVar  : AnsiString): AnsiString;

begin
  Result:=Dos.Getenv(EnvVar); 
end;
  $Log$
  Revision 1.2  2000-07-13 11:33:01  michael
  + removed logs
 
}

Uses Linux;

{ Declared EXPLICITLY with Ansistring, so NO mistaking is possible }

Function Getenv (Var EnvVar  : AnsiString): AnsiString;

Var P : Pchar;

begin
   // Linux version returns pchar.
   p:=linux.getenv(EnvVar);
   if P<>nil then
     getenv:=strpas(p)
   else
     getenv:='';
end;

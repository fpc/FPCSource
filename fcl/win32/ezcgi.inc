Uses Windows;

{ Declared EXPLICITLY with Ansistring, so NO mistaking is possible }

{
  This function is VERY inefficient, but the downsize would be to
  have initialization/finalization code to get/free the environment
  settings.
}

Function Getenv (Var EnvVar  : AnsiString): AnsiString;

var
   s : string;
   i : longint;
   hp,p : pchar;
begin
   getenv:='';
   p:=GetEnvironmentStrings;
   hp:=p;
   while hp^<>#0 do
     begin
        s:=StrPas(hp);
        i:=pos('=',s);
        if upcase(copy(s,1,i-1))=upcase(envvar) then
          begin
             getenv:=copy(s,i+1,length(s)-i);
             break;
          end;
        { next string entry}
        hp:=hp+strlen(hp)+1;
     end;
   FreeEnvironmentStrings(p);
end;

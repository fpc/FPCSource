{ %version=1.1 }

{$ifdef fpc}
{$MODE DELPHI}
{$endif}

const Inf=1/0;
      NaN=0/0;
      MinusInf=-Inf;
var
  s : string;
  error : boolean;
begin
  error:=false;
  str(Inf,s);
  writeln('Inf: "',s,'"');
  if s<>'                     +Inf' then
   error:=true;
  str(NaN,s);
  writeln('Nan: "',s,'"');
  if s<>'                      Nan' then
   error:=true;
  str(MinusInf,s);
  writeln('MinusInf: "',s,'"');
  if s<>'                     -Inf' then
   error:=true;
  if error then
   begin
     writeln('ERROR!');
     halt(1);
   end;
end.

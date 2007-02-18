{ %version=1.1 }

{$ifdef fpc}
{$MODE DELPHI}
{$endif}

{ Range and overflow checks need to be off }

{$Q-}
{$R-}

const Inf=1/0;
      NaN=0/0;
      MinusInf=-Inf;

var
  s : string;
  error : boolean;
  s1, s2, s3 : string;
begin
  if sizeof(extended) > 8 then
    begin
      s1 := '                     +Inf';
      s2 := '                      Nan';
      s3 := '                     -Inf';
   end
  else
   begin
      s1 := '                  +Inf';
      s2 := '                   Nan';
      s3 := '                  -Inf';
   end;
  error:=false;
  str(Inf,s);
  writeln('Inf: "',s,'"');
  if s<>s1 then
   error:=true;
  str(NaN,s);
  writeln('Nan: "',s,'"');
  if s<>s2 then
   error:=true;
  str(MinusInf,s);
  writeln('MinusInf: "',s,'"');
  if s<>s3 then
   error:=true;
  if error then
   begin
     writeln('ERROR!');
     halt(1);
   end;
end.

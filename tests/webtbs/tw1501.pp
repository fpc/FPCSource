{$mode objfpc}

function Uper1(const Pwd:AnsiString):AnsiString;
begin
        Result := UpCase(Pwd);
        Exit;
end;

function Uper2(const Pwd:AnsiString):AnsiString;
begin
        Exit(UpCase(Pwd));
end;

begin
  writeln('test', Uper1('test'));
  writeln('test', Uper2('test'));
  if Uper1('test')<>'TEST' then
   begin
     writeln('ERROR in Uper1!');
     halt(1);
   end;
  if Uper2('test')<>'TEST' then
   begin
     writeln('ERROR in Uper2!');
     halt(1);
   end;
end.

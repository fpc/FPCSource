{$mode fpc}

var
  err : boolean;

procedure check(const s:string;b:boolean);
begin
  writeln(s,b);
  if not b then
    begin
      err:=true;
      writeln('error!');
    end;
end;

Var SS : ShortString;
    AS : AnsiString;

Begin
  SS := 'asdf';
  AS := 'asdf';
  Check('SS         > '''': ', SS > '');
  Check('Length(SS) >  0: '  , Length(SS) > 0);
  Check('AS         > '''': ', AS > '');
  Check('Length(AS) >  0: '  , Length(AS) > 0);
  if err then
    halt(1);
End.

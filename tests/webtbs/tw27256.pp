program Test;

type
  FullType = (Unknown,Stiletto,Vanguard);
  SubType = Stiletto..Vanguard;

const
  full_choices: array[FullType] of String = ('U','S','V');
  sub_choices: array[SubType] of String = ('S', 'V');

var
  x : longint;

procedure abc(choices: array of String);
begin
  inc(x,high(choices));
end;

begin
  abc(full_choices);
  abc(sub_choices);
  if x<>3 then
    halt(1);
  writeln('ok');
end.

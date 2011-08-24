{ %OPT=-gl -gh }
{$mode objfpc}{$H+}
uses Classes, SysUtils, FGL;

type
  TMessages = specialize TFPGList<string>;

var
  Messages: TMessages;

procedure WritelnMessages(const S: string);
var
  I: Integer;
begin
  Writeln('Messages ', S, ' : ', Messages.Count);
  for i := 0 to Messages.Count - 1 do
    Writeln('  Messages[', I, ']: ', PtrUInt(Pointer(Messages[I])), ' ', Length(Messages[I]), ' ', Messages[I]);
end;

procedure Show(S: string);
var
  NewS: string;
begin
  WritelnMessages('before Add');
  NewS := Copy(S, 1, 10) + Copy(S, 11, MaxInt);
  Messages.Add(NewS);
  WritelnMessages('after Add');
end;

begin
  Messages := TMessages.Create;

  Show('Loaded level "Castle Hall"');
  Show('You pick "Sword"');
  Show('You''re using weapon "Sword" now');
  Show('Hint: press "Escape" for game menu');
  Messages.Delete(0);
  Show('You pick "Potion Of Life"');

  FreeAndNil(Messages);
end.

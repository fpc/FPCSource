program BugFPC;

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF FPC}

uses
  SysUtils;

type
  TBug = record

  strict private

    class constructor Bugger();

  public
  var
    a: Int32;
    st: string;
    arr: TBytes;

    class var

      skim: TBug;

    constructor Create(b: Int32);

  end;

  { TBug }

class constructor TBug.Bugger;
begin
  skim := Default (TBug);
  skim.a := 5;
  skim.st := 'fish';
  skim.arr := TBytes.Create(1, 2, 3);

  Writeln(skim.a); // should be 5
  Writeln(skim.st); // should be 'fish'
  Writeln(Length(skim.arr)); // should be 3
  if skim.a<>5 then
    halt(1);
  if skim.st<>'fish' then
    halt(2);
  if length(skim.arr)<>3 then
    halt(3);
end;

constructor TBug.Create(b: Int32);
var
  temp: TBug;
begin
  temp := skim;
  Writeln(temp.a); // should be 5
  Writeln(temp.st); // should be 'fish' but got ''
  Writeln(Length(skim.arr)); // should be 3 but got 0
  if skim.a<>5 then
    halt(4);
  if skim.st<>'fish' then
    halt(5);
  if length(skim.arr)<>3 then
    halt(6);
end;

begin
  try
    TBug.Create(9);
  except
    on E: Exception do
      begin
        Writeln(E.ClassName, ': ', E.Message);
        halt(7);
      end;
  end;

end.

{ %interactive }

{ see tw13480d.pp for test instructions }

{$mode objfpc}

unit tw13840a;

interface

type
  ta = class
    procedure test({$ifdef changed}a: longint = 4{$endif}); virtual;
    procedure mymy(var a);virtual;
  end;

implementation

procedure ta.test({$ifdef changed}a: longint = 4{$endif});
begin
  writeln('ta.test');
end;

procedure ta.mymy(var a);
begin
  writeln('ta.mymy');
end;

end.

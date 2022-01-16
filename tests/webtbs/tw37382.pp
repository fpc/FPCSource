{$mode objfpc}
{$h+}

uses
  GDeque;

type
  TIntQueue = specialize TDeque<Integer>;

var
  Q: TIntQueue;

begin
  Q := TIntQueue.Create;
  Q.Insert(0, 12345);
  writeln('Size=',Q.Size);
  Q.Erase(0);
  writeln('Size=',Q.Size);
  Q.Free;
end.

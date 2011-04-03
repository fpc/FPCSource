{$mode objfpc}

uses gpriorityqueue;

type
  lesslli = class
    public
    class function c(a,b: longint):boolean;inline;
  end;

class function lesslli.c(a,b: longint):boolean;inline;
begin
  c:=a<b;
end;

type priorityqueuelli = specialize TPriorityQueue<longint, lesslli>;

var data:priorityqueuelli; i:longint;

begin
  data:=priorityqueuelli.Create;
  for i:=1 to 10 do
    data.Push(random(1000));
  while not data.IsEmpty do begin
    writeln(data.Top);
    data.Pop;
  end;

  data.Destroy;
end.

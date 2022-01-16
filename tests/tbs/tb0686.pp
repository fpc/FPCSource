{$mode objfpc}

uses math;

type
  generic TLazFifoQueue<T> = class
  private
    FList: array of T;
    FQueueSize: integer;
  protected
    FTotalItemsPopped: QWord;
    FTotalItemsPushed: QWord;
  public
    procedure Grow(ADelta: integer);
  end;

procedure TLazFifoQueue.Grow(ADelta: integer);
var
  NewList: array of integer;
  c: Integer;
  i: QWord;
begin
  c:=Max(FQueueSize + ADelta, Integer(FTotalItemsPushed - FTotalItemsPopped));
  setlength(NewList{%H-}, c);
  i:=FTotalItemsPopped;
  while i < FTotalItemsPushed do begin
    NewList[i mod c] := FList[i mod FQueueSize];
    inc(i);
  end;

  FList := NewList;
  FQueueSize:=c;
end;

type
  TIntQ = specialize TLazFifoQueue<integer>;

begin
  with TIntQ.Create do begin
    Grow(123);
    if FQueueSize <> 123 then begin
      writeln('FAILED');
      halt(1);
    end;
    Free;
  end;
  writeln('OK');
end.

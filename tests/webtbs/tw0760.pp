type TElement = object
      constructor Init;
      {something}
      destructor Free; virtual;
      destructor Done; virtual;
     end;

constructor TElement.Init;
begin
  Writeln('Init called');
end;

destructor TElement.free;
begin
  Writeln('Free used');
end;

destructor TElement.Done;
begin
  Writeln('Done used');
end;

var
  E : TElement;
  PE : ^TElement;

begin
  E.init;
  E.Free;
  new(PE,init);
  dispose(PE,Done);
end.

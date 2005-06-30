{ %recompile }
unit tw4140;

interface

implementation

uses uw4140;

procedure Foo;
var Enum: TMySubEnum;
begin
  { Any of two lines below causes "Internal error 309993" }
  Writeln(Ord(Low(TMySubEnum)));
  Writeln(Ord(Low(Enum)));
end;

end.

{$inline on}

unit uw17493;

interface

procedure test; inline;

implementation

procedure test; inline;
var
  a: boolean;
begin
  a:=paramcount>100;
  while a do
    halt(1);
end;

end.

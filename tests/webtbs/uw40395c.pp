unit uw40395c;

interface

type
  trec = record
    a,b,c,d: longint;
  end;

procedure test; inline;

implementation

procedure test; inline;
var
  r: trec;
begin
  r:=default(trec);
end;

end.

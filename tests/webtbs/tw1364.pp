{$X-}

type
  o=object
    constructor init(s:string);
  end;

constructor o.init(s:string);
begin
end;

var
  s:o;
begin
  s.init('test');
end.

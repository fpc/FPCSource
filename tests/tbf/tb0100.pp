{ %FAIL }

type
  enum1 = (one,two,three);
  enum2 = (four,five,six);

  enumset1 = set of enum1;

var
  s1 : enumset1;
begin
  s1:=[one,two]+[four];
end.

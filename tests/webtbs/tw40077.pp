program tw40077;
uses
  Generics.Collections, Generics.Defaults;

type
  TRec = record
    X: Int64;
    Y: Int64;
  end;
  TDict = specialize TObjectDictionary<TRec, TObject>;

var
  Dict: TDict;
  R: TRec;
begin
  Dict := TDict.Create([doOwnsValues]);
  R.X := 1;
  R.Y := 1;
  if not Dict.ContainsKey(R) then // <---------- SIGSEGV here
    Dict.Add(R, TObject.Create);
  Dict.Free;
end.


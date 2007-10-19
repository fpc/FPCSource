uses
  fgl;

type
  TEnum = (ta, tb, tc);

  TMyMap = specialize TFPGMap<string, TEnum>;

var
  map : TMyMap;
  c : TEnum;
  i : Longint;

begin
  map := TMymap.Create();

  map.Add('Hello', ta);

  map.Find('Hello', i);
end.

program tw40331;
{$mode delphi}
uses Generics.Defaults;
type
  SomeEnum = (zero = 0, two = 2);
var
  z: IEqualityComparer<SomeEnum>;
begin
  z:= TEqualityComparer<SomeEnum>.Default;
  WriteLn('Done');
  //ReadLn;
end.


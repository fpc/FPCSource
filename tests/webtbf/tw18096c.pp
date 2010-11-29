{ %fail }
{$mode objfpc}

type
  generic G<_T> = class
  end;
  
  generic TGen<_T> = class
  public
    function Check(ASource: TObject): Boolean;
  end;

  TSpec = specialize TGen<Integer>;

function TGen.Check(ASource: TObject): Boolean;
begin
  Result := not (ASource is G) // we are testing this: usage of another generic is not allowed
  and (ASource is TGen) // this should work...
  and (ASource is ClassType);   // ...and it should be equivelent to this line
end;

var
  f:  TSpec;
  o: TObject;
begin
  f := TSpec.Create;
  o := TObject.Create;
  if not(f.Check(f)) or f.Check(o) then
    halt(1);
  writeln('ok');
end.

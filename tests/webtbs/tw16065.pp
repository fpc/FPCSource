{$mode objfpc}

type
  generic TGen<_T> = class
  public
    function Check(ASource: TObject): Boolean;
  end;

  TSpec = specialize TGen<Integer>;

function TGen<_T>.Check(ASource: TObject): Boolean;
begin
  Result := (ASource is specialize TGen<_T>)   // this line breaks the compiler...
  and (ASource is ClassType);   // ...it should be equivelent to this line
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

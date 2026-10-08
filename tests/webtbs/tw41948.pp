{ %NORUN }

program tw41948;

{$mode ObjFPC}

type
  generic TA<T> = object
  type
    TB = specialize TA<Integer>;
  end;

  //TA_Integer = specialize TA<Integer>;
  TA_Single = specialize TA<Single>;

begin
end.

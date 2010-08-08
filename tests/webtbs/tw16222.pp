{ %norun } 
program tw16222;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TOuterClass = class
  public
    type 
      TInnerClass = class
      end;
  end;

function fn(P: Pointer): TOuterClass.TInnerClass;
begin
  Result := TOuterClass.TInnerClass(P);
end;

begin
end.
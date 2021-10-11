unit uw38827;
{$mode Delphi}
interface
type
  TRec = record
    C: TArray<single>;
    function GetArr<T>: TArray<T>; inline;
  end;
implementation
function TRec.GetArr<T>: TArray<T>;
begin
  result := nil;
  case GetTypeKind(T) of
    tkFloat:
      if SizeOf(T) = SizeOf(Single) then result := C;
  end;
end;
end.

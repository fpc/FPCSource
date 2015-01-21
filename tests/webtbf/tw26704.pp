{ %fail }

type
  TTest = record
    Value: Byte;
    case Integer of
      0: (Value: Word);
  end;

begin
end.

type
  TListEntry = record
    Next: ^TListEntry;                      (*<-- Error message here*)
    Data: Integer;
  end;

begin
end.

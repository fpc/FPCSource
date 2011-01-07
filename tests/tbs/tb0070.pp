{ Old file: tbf0343.pp }

{$mode delphi}
type
  TListEntry = record
    Next: ^TListEntry;                    // delphi and fpc allows this now
    Data: Integer;
  end;

begin
end.

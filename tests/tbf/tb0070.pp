{ %FAIL }
{ Old file: tbf0343.pp }
{  }

{$mode delphi}
type
  TListEntry = record
    Next: ^TListEntry;                      (*<-- Error message here*)
    Data: Integer;
  end;

begin
end.

{$mode delphi}

const
  CSV_Internal = 10;

type
  PTypeRec = ^TTypeRec;
  TTypeRec = record
    atypeid: Word;
  end;


function ChangeType(newtype: PTypeRec): Pointer;

begin
  if NewType.AtypeID = CSV_Internal then
  begin
  end;
end;

begin
end.

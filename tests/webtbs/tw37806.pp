program tw37806;

{$mode delphi}

procedure TurnSetElem<TSet, TElem>(var aSet: TSet; aElem: TElem; aOn: Boolean);
begin
  if aOn then
    Include(aSet, aElem)
  else
    Exclude(aSet, aElem);
end;

type
  TElem = (One, Two, Three, Four, Five);
  TSet = set of TElem;

var
  s: TSet = [];

begin
  TurnSetElem<TSet, TElem>(s, Two, True);
  TurnSetElem<TSet, TElem>(s, Five, True);
  if not((Two in s) and (Five in s)) then
    Halt(1);
    //WriteLn('does not work');
end.

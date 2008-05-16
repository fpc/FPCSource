program project1;

{$mode delphi}

type
  TEnum1 = (en1, en2);
  TEnum2 = (en3, en4);

  TSet1 = set of TEnum1;
  TSet2 = set of TEnum2;

procedure DoSomethingWithSet(ASet: TSet1); overload;
begin

end;

procedure DoSomethingWithSet(ASet: TSet2); overload;
begin

end;

begin
  DoSomethingWithSet([en1]);
end.

program Example111;

{ Program to demonstrate the Include/Exclude functions }

Type
  TEnumA = (aOne,aTwo,aThree);
  TEnumAs = Set of TEnumA;

Var
  SA : TEnumAs;

  Procedure PrintSet(S : TEnumAs);

  var
    B : Boolean;

    procedure DoEl(A : TEnumA; Desc : String);

    begin
      If A in S then
        begin
        If B then
          Write(',');
        B:=True;
        Write(Desc);
        end;
    end;

  begin
    Write('[');
    B:=False;
    DoEl(aOne,'aOne');
    DoEl(aTwo,'aTwo');
    DoEl(aThree,'aThree');
    Writeln(']')
  end;

begin
  SA:=[];
  Include(SA,aOne);
  PrintSet(SA);
  Include(SA,aThree);
  PrintSet(SA);
  Exclude(SA,aOne);
  PrintSet(SA);
  Exclude(SA,aTwo);
  PrintSet(SA);
  Exclude(SA,aThree);
  PrintSet(SA);
end.
    
program test;
{$ZEROBASEDSTRINGS ON}
{$H+} // make sure it is AnsiString, not ShortString
{$R+}

Var
  I: Integer;
  S: String;
Begin
  S := 'abc';
  WriteLn(Length(S));
  For I := High(S) Downto Low(S) Do
    WriteLn(I, ' ---> ', S[I]);
End.


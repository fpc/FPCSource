Program Example72;

{ This program demonstrates the FormatBuf function }

Uses sysutils;

Var
  S : ShortString;

Const
  Fmt : ShortString =  'For some nice examples of fomatting see %s.';

Begin
  S:='';
  SetLength(S,FormatBuf (S[1],255,Fmt[1],Length(Fmt),['Format']));
  Writeln (S);
End.
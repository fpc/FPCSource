Program Example46;

{ This program demonstrates the StrBufSize function }
{$H+}

Uses sysutils;

Const S  = 'Some nice string';

Var P : Pchar;

Begin
   P:=StrAlloc(Length(S)+1);
   StrPCopy(P,S);
   Write (P, ' has length ',length(S));
   Writeln (' and  buffer size ',StrBufSize(P));
   StrDispose(P);
End.
{ %version=1.1}
{$mode objfpc}
Program tests;

Var
  P : PWideChar;
  S : AnsiString;
  W,WP : WideString;
  PO : Pointer;

Const
  Buffer : Array[0..10] of Widechar
         = ('A','n','s','i','S','t','r','i','n','g',#0);
{
  procedure OleStrToStrVar(Source: PWideChar; var Dest: string);
  begin
    WideCharLenToStrVar(Source, Length(WideString(Pointer(Source))), Dest);
  end;


  Function OleStrToString(Source : PWideChar) : String;

  begin
    OleStrToStrVar(Source, Result);
  end;
}

  Function PWideCharLen(P: PWideChar) : Integer;

  Var
    W : PWord;

  begin
    Result:=0;
    If P=Nil then
      Exit;
    W:=PWord(P);
    While W[Result]<>0 do
      Inc(Result);
  end;

  Function PWideCharToWideString(P : PWideChar) : WideString;

  Var
    L : integer;

  begin
    L:=PWideCharLen(P);
    SetLength(Result,L);
    If P<>Nil then
      Move(P^,Result[1],L*SizeOf(WideChar));
  end;

begin
  S:='AnsiString';
  W:=S;
  Writeln(W);
  P:=PWideChar(W);
  Writeln('len : ',Length(WideString(Pointer(P))));
//  P:=PWideChar(@Buffer[0]);
  P:=Buffer;
  Writeln('Len array : ',PWideCharLen(P));
  WP:=PWideCharToWideString(P);
  Writeln('WP Len : ',Length(WP),' : ',WP);
  PO:=Nil;
  WP:=WideString(PO);
//  Writeln('len buffer: ',Length(P)); //WideString(Pointer(P))));
//  Writeln(P);
end.

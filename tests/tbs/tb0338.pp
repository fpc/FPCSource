{$h+}

Type
  TMyRec = Record
    AString : AnsiString;
  end;
  PMyRec = ^TMyRec;

Var
  M : PMyRec;

begin
  M:=New(PmyRec);
end.

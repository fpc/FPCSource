Procedure DoProperties(widget : pGtkWidget ; Window : PMainWindow); cdecl;

Var i : longint;
    S : string;
    Dlg : PFilePropertiesDialog;

begin
  With Window^ do
    S:=AddTrailingSeparator(FDir)+GetFileFirstSelection(FileList);
  Dlg:=NewFilePropertiesDialog(S);
  ShowFilePropertiesDialog(Dlg);
end;

Procedure DeleteFile(Widget : PGtkWidget; Window : PMainWindow); cdecl;

Var i : longint;
    S : TStringList;

begin
  S:=TStringList.Create;
  Try
    GetFileSelection(Window^.FileList,S);
    For I:=0 to S.Count-1 do
      begin
      For I:=0 to S.Count-1 do
        SysUtils.DeleteFile(Window^.FDir+S[i]);
      end;
  Finally
    If S.Count>0 then
      RefreshFileView(Window);
    S.Free;
  end;
end;
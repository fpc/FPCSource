program testuser;

uses users,classes;

var
  Ulist,GList : TStringlist;
  i : longint;

begin
  Ulist:=TStringList.Create;
  Glist:=TStringList.Create;
  Try
    GetUserList(Ulist,True);
    GetGroupList(Glist,True);
    Writeln('Users: ');
    For I:=0 to Ulist.Count-1 do
      Writeln('User ',Ulist[i]);
    Writeln('Groups: ');
    For I:=0 to Glist.Count-1 do
      Writeln('Group ',Glist[i]);
  finally
    Ulist.Free;
    Glist.Free;
  end;
end.

program testpp;
{$mode objfpc}
{$H+}
uses sysutils, sqldb, ibconnection, tsamytable;

Procedure DoTest;

Var
  C : TIBConnection;
  T : TSQLTransaction;
  A : IMyTypeSafeAccess;

begin
  C:=TIBConnection.Create(Nil);
  try
    C.HostName:='localhost';
    C.DatabaseName:='/home/firebird/testdb.fdb';
    C.UserName:='WISASOFT';
    C.Password:='SysteemD';
    T:=TSQLTransaction.Create(C);
    C.Transaction:=T;
    T.Database:=C;
    A:=TMyTypeSafeAccess.GetQuery(C,T);
    A.Open;
    A.Append;
    A.MyBoolean:=True;
    A.MyInteger:=StrToIntDef(Paramstr(1),1);
    A.MyWideString:='a';
    A.MyUnicodeString:='B';
    A.MyByteInteger:=123;
    A.MyInt64:=4564654;
    A.MyQWordLargeInt:=6566564564;
    A.MySmallintInteger:=2345;
    A.MyShortIntInteger:=5;
    A.MyCardinalInteger:=6;
    A.MybLob.Write(C.DatabaseName[1],Length(C.DatabaseName));
    A.MyFixedChar:='fa';
    A.MyFixedWideString:='fu';
    A.Post;
    A.ApplyUpdates;
    T.Commit;
  finally
//    A.Free;
    C.Free;
  end;
end;

begin
  Try
    DoTest;
  except
    On E : Exception do
      writeln('Exception ',E.ClassName,' with message : ',E.Message);
  end;
end.


{ %OPT=-gh }

program tw28271;

{$mode delphi}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes
  { you can add units after this };

type
  TMyMsgDlg=class
  private
    class procedure SyncFree;
    class procedure SyncCreate;
  public
    class procedure StaticCreate;
    class procedure StaticFree;
  end;

var
  Dlg:TMyMsgDlg;

  class procedure TMyMsgDlg.SyncCreate;
  begin
    Dlg:=TMyMsgDlg.Create;
  end;

  class procedure TmyMsgDlg.SyncFree;
  begin
    if Assigned(Dlg) then
    	Dlg.free;
    Dlg:=nil;
  end;

  class procedure TMyMsgDlg.StaticCreate;
  begin
    if IsLibrary then
      SyncCreate
    else
      TThread.Synchronize(nil,SyncCreate);
  end;

  class procedure TMyMsgDlg.StaticFree;
  begin
    if IsLibrary then
      SyncFree
    else
    begin
      TThread.Synchronize(nil,SyncFree)
    end;
  end;

begin
  HaltOnNotReleased := True;
  //writeln('Create');
  TMyMsgDlg.StaticCreate;
  //writeln('Free');
  TMyMsgDlg.StaticFree;
  //writeln('Done');
end.


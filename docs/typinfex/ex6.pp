program example6;

{ This program demonstrates the GetMethodProp function }

{$mode objfpc}

uses rttiobj,typinfo,sysutils;

Type
  TNotifyObject = Class(TObject)
    Procedure Notification1(Sender : TObject);
    Procedure Notification2(Sender : TObject);
  end;

Procedure TNotifyObject.Notification1(Sender : TObject);

begin
  Write('Received notification 1 of object with class: ');
  Writeln(Sender.ClassName);
end;

Procedure TNotifyObject.Notification2(Sender : TObject);

begin
  Write('Received notification 2 of object with class: ');
  Writeln(Sender.ClassName);
end;

Var
  O : TMyTestObject;
  PI : PPropInfo;
  NO : TNotifyObject;
  M : TMethod;

Procedure PrintMethod (Const M : TMethod);

begin
  If (M.Data=Pointer(NO)) Then
    If (M.Code=Pointer(@TNotifyObject.Notification1)) then
      Writeln('Notification1')
    else If (M.Code=Pointer(@TNotifyObject.Notification2)) then
      Writeln('Notification2')
    else
      begin
      Write('Unknown method adress (data:');
      Write(hexStr(Longint(M.data),8));
      Writeln(',code:',hexstr(Longint(M.Code),8),')');
      end;
end;


begin
  O:=TMyTestObject.Create;
  NO:=TNotifyObject.Create;
  O.NotifyEvent:=@NO.Notification1;
  PI:=GetPropInfo(O,'NotifyEvent');
  Writeln('Method property : ');
  Write('Notifying                    : ');
  O.Notify;
  Write('Get (name)                   : ');
  M:=GetMethodProp(O,'NotifyEvent');
  PrintMethod(M);
  Write('Notifying                    : ');
  O.Notify;
  Write('Get (propinfo)               : ');
  M:=GetMethodProp(O,PI);
  PrintMethod(M);
  M:=TMethod(@NO.Notification2);
  SetMethodProp(O,'NotifyEvent',M);
  Write('Set (name,Notification2)     : ');
  M:=GetMethodProp(O,PI);
  PrintMethod(M);
  Write('Notifying                    : ');
  O.Notify;
  Write('Set (propinfo,Notification1) : ');
  M:=TMethod(@NO.Notification1);
  SetMethodProp(O,PI,M);
  M:=GetMethodProp(O,PI);
  PrintMethod(M);
  Write('Notifying                    : ');
  O.Notify;
  O.Free;
end.
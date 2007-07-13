{$mode objfpc}{$H+}

uses classes,typinfo;
type
  TA = class(TPersistent)
  private
    FOnTest: TNotifyEvent;
    procedure SetOnTest(value: TNotifyEvent);
  public
    procedure CallTest;
  published
    property OnTest: TNotifyEvent read FOnTest Write SetOnTest;
  end;  

  TB = class
  public
    procedure Test(Sender: TObject);
  end;

procedure TA.SetOnTest(value: TNotifyEvent);
begin
  FOnTest := Value
end;

procedure TA.CallTest;
begin
  if Assigned(FOnTest) then 
    OnTest(self)
  else
    WriteLn('OnTest no set');
end;

procedure TB.Test(Sender: TObject);
begin
  WriteLn('Test Called');
end;

var
  A: TA;
  B: TB;
  PropInfo: PPropInfo;
  Method: TMethod;
begin
  A := TA.Create;
  B := TB.Create;

  Method:=TMethod(@B.Test);

  PropInfo:=GetPropInfo(A.ClassInfo, 'OnTest');
  if Assigned(PropInfo) then begin
    SetMethodProp(A, PropInfo, Method);
    WriteLn('Testing SetMethodProp method');
    A.CallTest;
  end 
  else begin
    WriteLn('PropInfo for ''OnTest'' not found');
    Halt(1);
  end;
end.


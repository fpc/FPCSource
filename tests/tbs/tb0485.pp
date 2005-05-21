{$mode objfpc}
program test05;

uses
  SysUtils;


type
         QObjectH = class(TObject) end;
                QWidgetH = class(QObjectH) end;


IQbase = interface(IUnknown)
end;

TQBase = class(TInterfacedObject,IQBase)
protected
  fQHandle : TObject;
  function GetQHandle : TObject;
  procedure SetQHandle(Value : TObject);
public
  property QHandle : TObject read GetQHandle write SetQHandle;
end;



IQObject = interface(IQBase)
  function GetQHandle : QObjectH;
  property QHandle : QObjectH read GetQHandle;
end;


TQObject = class(TQBase, IQObject)
protected
  function GetQHandle : QObjectH; overload;
  procedure SetQHandle(Value:QObjectH);

public
  property QHandle : QObjectH read GetQHandle write SetQHandle;
  constructor CreateWrapper;
  Constructor Create(name: PAnsiChar); overload;
end;


IQWidget = interface(IQObject)
  function GetQHandle : QWidgetH;
  property QHandle : QWidgetH read GetQHandle;
  function Width: Integer;
end;


TQWidget = class(TQObject, IQWidget)
protected
  function GetQHandle : QWidgetH; overload;
  procedure SetQHandle(Value:QWidgetH);
public
  property QHandle : QWidgetH read GetQHandle write SetQHandle;
  constructor CreateWrapper;
  Constructor Create(name: PAnsiChar); overload;
  function Width: Integer;
end;


function TQObject.GetQHandle : QObjectH;
begin
  if Self <> nil then Result := QObjectH(fQHandle)
  else Result := nil;
end;

procedure TQObject.SetQHandle(Value : QObjectH);
begin
  fQHandle := TObject(Value);
end;

constructor TQObject.CreateWrapper;
begin
  inherited Create;
end;



Constructor TQObject.Create(name: PAnsiChar);
begin
   CreateWrapper;
end;



function TQBase.GetQHandle : TObject;
begin
  Result := fQHandle
end;


procedure TQBase.SetQHandle(Value : TObject);
begin
  fQHandle:=Value;
end;



function TQWidget.GetQHandle : QWidgetH;
begin
  write('    entering TQWidget.GetQHandle ...');
  if Self <> nil then Result := QWidgetH(fQHandle)
  else Result := nil;
  writeln('...leaving entering TQWidget.GetQHandle');
end;

procedure TQWidget.SetQHandle(Value : QWidgetH);
begin
  fQHandle := TObject(Value);
end;

constructor TQWidget.CreateWrapper;
begin
  write('    entering TQWidget.CreateWrapper ...');
  inherited Create;
  writeln('...leaving TQWidget.CreateWrapper');
end;



Constructor TQWidget.Create(name: PAnsiChar);
begin
  write('entering TQWidget.Create ...');
  CreateWrapper;
  writeln('... leaving TQWidget.Create');
end;



function TQWidget.Width: Integer;
begin
  write('    entering TQWidget.Width...');
  Result:=123;
  writeln('...leaving TQWidget.Width');
end;



function GetWidget : IQWidget;
begin
Result := TQWidget.CreateWrapper;
end;


begin
writeln('GetWidget.Width (123)?:',GetWidget.Width);
if GetWidget.Width<>123 then
  begin
        writeln('error');
        halt(1);
  end;
end.

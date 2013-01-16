{ This demo is very basic, but shows the Observer support in the RTL }
program dobserver;

{$mode objfpc}{$h+}
{$ifdef mswindows}{$apptype console}{$endif}

uses
 Classes, SysUtils, typinfo;

type
  TMyObserver = class(TObject, IFPObserver)
  private
    procedure FPOObservedChanged(ASender : TObject; Operation : TFPObservedOperation; Data : Pointer);
  end; 
  
{ TMyObserver }

procedure TMyObserver.FPOObservedChanged(ASender: TObject;
               Operation: TFPObservedOperation; Data: Pointer);

  function OperationToString(AOperation: TFPObservedOperation): string;
  begin
    result := GetEnumName(TypeInfo(TFPObservedOperation),
                         Ord(AOperation));
  end;
var
  intf: IFPObserved;
begin
  if Operation = ooFree then
  begin
    writeln('[ooFree] detected so we should detach ourselves');
    if Supports(ASender, IFPObserved, intf) then
      intf.FPODetachObserver(self);
  end
  else
  begin
    writeln(ASender.ClassName + ' has changed ['+
      OperationToString(Operation) + ']');
  end;
end;
  
var
  sl: TStringList;
  observer: TMyObserver;
  intf: IFPObserved;
begin
  { This stringlist will be the subject (observed) }
  sl := TStringList.Create;
  { this instance will be the observer - notified when StringList changes }
  observer := TMyObserver.Create;

  { attach observer }  
  if Supports(sl, IFPObserved, intf) then
  begin
    intf.FPOAttachObserver(observer);
  end;
  
  { Do something to the stringlist }
  sl.Add('Item one');
  sl.Add('Item two');
  sl.Delete(0);
  
  { Clean-up code - also shows ooFree operation }
  sl.Free;
  observer.Free;
end.


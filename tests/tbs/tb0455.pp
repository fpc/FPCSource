{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}
uses classes;


type
  HDC = Cardinal;

    TNotifyEventA    = procedure (Sender:TObject) of object;

   TwolBrushes = class
   public
     FOnChange  :TNotifyEventA;
     procedure Wol_Changed;
     property OnChange  :TNotifyEventA      read FOnChange      Write FOnChange;
   end;


   TWOLBetaObject = class
   public
     mylocalvar : integer;
     constructor Create(AOwner:TOBject);
   protected
     procedure DoBrushChange(Sender:TObject);
   private
     FBrush : TWolBrushes;
   end;


  procedure TWOLBetaObject.DoBrushChange(Sender:TObject);
  var DC:HDC;
  begin
    mylocalvar:=12;
    WriteLn('OK!');
  end;


  procedure TwolBrushes.WOL_Changed;
  begin
    if Assigned(FOnChange) then FOnChange(Self);
  end;




constructor TWOLBetaObject.Create(AOwner:TOBject);
   begin
     Inherited Create;
     FBrush         :=TWOLBrushes.Create;
     FBrush.OnChange:=DoBrushChange;
   end;


var
 cla1: TWolbetaObject;
begin
 cla1:=TWolBetaObject.create(nil);
 cla1.FBrush.WOL_Changed;
end.

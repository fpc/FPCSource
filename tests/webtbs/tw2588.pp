{$MODE DELPHI}
uses classes;


type
    TNotifyEventA    = procedure (Sender:TObject) of object;
   TWOLBetaObject = class;

   TwolBrushes = class(TPersistent)
   private
     FOnChange  :TNotifyEventA;
   public
     property OnChange  :TNotifyEventA      read FOnChange      Write FOnChange;
   end;


   TWOLBetaObject = class(TComponent)
   public
     constructor Create(AOwner:TComponent);
   protected
     procedure DoBrushChange(Sender:TObject);
   private
     FBrush : TWolBrushes;
   end;


  procedure TWOLBetaObject.DoBrushChange(Sender:TObject);
  var DC:longint;
  begin
  end;

constructor TWOLBetaObject.Create(AOwner:TComponent);
   begin
     FBrush         :=TWOLBrushes.Create;
     FBrush.OnChange:=DoBrushChange;
   end;


begin
end.

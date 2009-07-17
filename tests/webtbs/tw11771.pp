{$MODE delphi}

unit tw11771;

interface


type
  
   TEvent = procedure(A : TObject) of object;


   TTest = class
   private
     FProc: TEvent;
     procedure Proc1(A : TObject);
     procedure Proc2(A : TObject = nil);

   public 
     constructor Create;
     procedure B(A : TEvent);
     property A	: TEvent read FProc write FProc;
     end;	

implementation

     constructor TTest.Create;
     begin
//    FProc := Proc1;
//    FProc := Proc2;
    A := Proc1;
    A := Proc2;
//    B(Proc1);
    B(Proc2);
    
     end;


procedure ttest.Proc1(A : TObject);
begin
end;
    

procedure ttest.Proc2(A : TObject = nil);
begin
end;


procedure ttest.B(A : TEvent);
begin
end;


end.

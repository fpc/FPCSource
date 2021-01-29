{ %NORUN }

program tb0679;

{$mode objfpc}

type
  TA = class
  public
    class destructor Destroy; 
    destructor Destroy; override;
  end;
  
class destructor TA.Destroy; 
begin
end;
    
destructor TA.Destroy;
begin
  inherited;
end;
 
var
  A: TA;
begin
  A := TA.Create;
  A.Free;
end.

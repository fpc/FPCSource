
type
 TObj =   object
  constructor Init0;
  constructor Init;
  procedure   Show;
  function    GetStr:string; virtual;
  destructor  Done;
 end;

 TChild = object (TObj)
   function GetStr:string; virtual;
 end;

var
  Err : boolean;

constructor TObj.Init0;
begin
end;

constructor TObj.Init;
begin
  Init0;
end;

function   TObj.GetStr:string;
begin
  GetStr:='Bad';
  Err:=true;
end;

procedure  TObj.Show;
begin
  writeln(GetStr);
end;

destructor TObj.Done;
begin
end;

function TChild.GetStr:string;
begin
  GetStr:='Good'
end;

var
  Obj:TChild;
begin
 Obj.Init;
 Obj.Show;
 Obj.Done;
 if Err then
  halt(1);
end.

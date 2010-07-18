program project_static2;
type
   Etyp=(t1,t2,t3);

type
   ProxyObject=object
     function IsInSubrange(const typ:Etyp):boolean;static;
   end;

   RealObject=object
     mytyp:Etyp;
     function IsInSubrange:boolean;
   end;

function RealObject.IsInSubrange: boolean;
begin
   IsInSubrange:=ProxyObject.IsInSubrange(mytyp);  
   // ^-- Error: Class isn't a parent class of the current class
   // and AV of compiler
end;

function ProxyObject.IsInSubrange(const typ: Etyp): boolean;
begin
   IsInSubrange:=typ<=t2;
end;

var o:RealObject;

begin
  if ProxyObject.IsInSubrange(t3) then
    halt(1);
  if not ProxyObject.IsInSubrange(t2) then
    halt(2);
  o.mytyp:=t3;
  if o.isInSubRange then
    halt(3);
  o.mytyp:=t1;
  if not o.isInSubRange then
    halt(4);
end.

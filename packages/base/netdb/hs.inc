
function HostAddrToStr (Entry : THostAddr) : String;

Var Dummy : String[4];
    I : Longint;

begin
  HostAddrToStr:='';
  For I:=1 to 4 do
   begin
   Str(Entry[I],Dummy);
   HostAddrToStr:=HostAddrToStr+Dummy;
   If I<4 Then 
     HostAddrToStr:=HostAddrToStr+'.';
   end;
end;

function StrToHostAddr(IP : String) : THostAddr ;

Var 
    Dummy : String;
    I     : Longint;
    J     : Integer;
    Temp : THostAddr;

begin
  Result:=NoAddress;
  For I:=1 to 4 do
   begin
   If I<4 Then
     begin
     J:=Pos('.',IP);
     If J=0 then 
       exit;
     Dummy:=Copy(IP,1,J-1);
     Delete (IP,1,J);
     end
   else
     Dummy:=IP;
   Val (Dummy,Temp[I],J);
   If J<>0 then Exit;
   end;
 Result:=Temp;
end;

function NetAddrToStr (Entry : TNetAddr) : String;

Var Dummy : String[4];
    I : Longint;

begin
  NetAddrToStr:='';
  For I:=4 downto 1 do
   begin
   Str(Entry[I],Dummy);
   NetAddrToStr:=NetAddrToStr+Dummy;
   If I>1 Then 
     NetAddrToStr:=NetAddrToStr+'.';
   end;
end;

function StrToNetAddr(IP : String) : TNetAddr;

begin
  StrToNetAddr:=TNetAddr(StrToHostAddr(IP));
end;

Function HostToNet (Host : ThostAddr) : THostAddr;

begin
  Result[1]:=Host[4];
  Result[2]:=Host[3];
  Result[3]:=Host[2];
  Result[4]:=Host[1];
end;

Function NetToHost (Net : TNetAddr) : TNetAddr;

begin
  Result[1]:=Net[4];
  Result[2]:=Net[3];
  Result[3]:=Net[2];
  Result[4]:=Net[1];
end;

Function HostToNet (Host : Longint) : Longint;

begin
  Result:=Longint(HostToNet(THostAddr(host)));
end;

Function NetToHost (Net : Longint) : Longint;

begin
  Result:=Longint(NetToHost(TNetAddr(Net)));
end;

Function ShortHostToNet (Host : Word) : Word;

begin
  ShortHostToNet:=lo(host)*256+Hi(Host);
end;

Function ShortNetToHost (Net : Word) : Word;

begin
  ShortNetToHost:=lo(Net)*256+Hi(Net);
end;


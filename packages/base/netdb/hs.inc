
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
{$Ifdef ENDIAN_LITTLE}
  Result[1]:=Host[4];
  Result[2]:=Host[3];
  Result[3]:=Host[2];
  Result[4]:=Host[1];
{$ELSE}
  result:=host;
{$endif}
end;

Function NetToHost (Net : TNetAddr) : TNetAddr;

begin
{$Ifdef ENDIAN_LITTLE}
  Result[1]:=Net[4];
  Result[2]:=Net[3];
  Result[3]:=Net[2];
  Result[4]:=Net[1];
{$ELSE}
  result:=net;
{$endif}
end;

Function HostToNet (Host : Longint) : Longint;

begin
{$Ifdef ENDIAN_LITTLE}
  Result:=Longint(HostToNet(THostAddr(host)));
{$ELSE}
  result:=host;
{$endif}
end;

Function NetToHost (Net : Longint) : Longint;

begin
{$Ifdef ENDIAN_LITTLE}
  Result:=Longint(NetToHost(TNetAddr(Net)));
{$ELSE}
  result:=net;
{$endif}
end;

Function ShortHostToNet (Host : Word) : Word;

begin
{$Ifdef ENDIAN_LITTLE}
  ShortHostToNet:=lo(host)*256+Hi(Host);
{$ELSE}
  result:=host;
{$endif}

end;

Function ShortNetToHost (Net : Word) : Word;

begin
{$Ifdef ENDIAN_LITTLE}
  ShortNetToHost:=lo(Net)*256+Hi(Net);
{$ELSE}
  result:=net;
{$endif}
end;



function HostAddrToStr6 (Entry : THostAddr6) : String;
var
  i: byte;
  zr1,zr2: set of byte;
  zc1,zc2: byte;
  have_skipped: boolean;
begin
  zr1 := [];
  zr2 := [];
  zc1 := 0;
  zc2 := 0;
  for i := 0 to 7 do begin
    if Entry[i] = 0 then begin
      include(zr2, i);
      inc(zc2);
    end else begin
      if zc1 < zc2 then begin
        zc1 := zc2;
        zr1 := zr2;
        zc2 := 0; zr2 := [];
      end;
    end;
  end;
  if zc1 < zc2 then begin
    zc1 := zc2;
    zr1 := zr2;
  end;
  SetLength(HostAddrToStr6, 8*5-1);
  SetLength(HostAddrToStr6, 0);
  have_skipped := false;
  for i := 0 to 7 do begin
    if not (i in zr1) then begin
      if have_skipped then begin
        if HostAddrToStr6 = ''
          then HostAddrToStr6 := '::'
          else HostAddrToStr6 := HostAddrToStr6 + ':';
        have_skipped := false;
      end;
      // FIXME: is that shortnettohost really proper there? I wouldn't be too sure...
      HostAddrToStr6 := HostAddrToStr6 + IntToHex(ShortNetToHost(Entry[i]), 1) + ':';
    end else begin
      have_skipped := true;
    end;
  end;
  if have_skipped then
    if HostAddrToStr6 = ''
      then HostAddrToStr6 := '::'
      else HostAddrToStr6 := HostAddrToStr6 + ':';

  if HostAddrToStr6 = '' then HostAddrToStr6 := '::';
  if not (7 in zr1) then
    SetLength(HostAddrToStr6, Length(HostAddrToStr6)-1);
end;

function StrToHostAddr6(IP : String) : THostAddr6;
begin
end;

function NetAddrToStr6 (Entry : TNetAddr6) : String;
begin
  Result := HostAddrToStr6(Entry);
end;

function StrToNetAddr6(IP : String) : TNetAddr6;
begin
  Result := StrToHostAddr6(IP);
end;

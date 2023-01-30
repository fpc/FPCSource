uses
  Classes, BaseUnix;

function SpawnFCGIProcess(App, Enviro: string; const aPort: Word): Integer;
var
  TheSocket: TLSocket;
  i: Integer;
  SL: TStringList;
  aNil: Pointer = nil;
  Apps : AnsiString {$IF SIZEOF(CHAR)=1} absolute App{$endif};
  EnviroS : AnsiString {$IF SIZEOF(CHAR)=1} absolute Enviro{$endif};
  ppEnv, ppArgs: ppAnsiChar;
  Env : Array of AnsiString;
  Len : Integer;
  
begin
  {$IF SIZEOF(CHAR)=2}   
  Apps:=UTF8Encode(App);
  Enviros:=UTF8Encode(EnviroS);
  {$ENDIF}
  Result:=FpFork;

  if Result = 0 then begin
    ppArgs:=@aNil;

    for i:=3 to 10000 do
      CloseSocket(i);

    if CloseSocket(StdInputHandle) <> 0 then
      Exit(LSocketError);

    TheSocket:=TLSocket.Create;
    TheSocket.SetState(ssBlocking);

    if not TheSocket.Listen(aPort) then
      Exit(LSocketError);

    ppEnv:=@aNil;

    if Length(Enviro) > 0 then begin
      SL:=TStringList.Create;
      repeat
        i:=Pos(':', Enviro);
        if i > 0 then begin
          SL.Add(Copy(Enviro, 1, i - 1));
          Delete(Enviro, 1, i);
        end else
          SL.Add(Enviro);
      until i = 0;
      Len:=SL.Count;
      SetLength(Env,Len);
      For I:=0 to Len-1 do
        begin
         {$IF SIZEOF(CHAR)=2}
        Env[i]:=UTF8Encode(SL[I]);
        {$ELSE}
        Env[i]:=SL[i];
        {$ENDIF}
        end;
        
      GetMem(ppEnv, SizeOf(pChar) * (Len+1));
      for i:=0 to Len-1 do
        ppEnv[i]:=pAnsiChar(Env[i]);
      ppEnv[Len]:=nil;
    end;
    
    FpExecve(pAnsiChar(App), ppArgs, ppEnv);
  end else if Result > 0 then
    Result:=0; // it went ok
end;


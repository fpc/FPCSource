uses
  baseunix,
  unix,
  lincd;

Function ReadCDTOC(Device : String; Var CDTOC : Array of TTocEntry) : Integer;

Var
  I,Drive    : Integer;
  tochdr   : Tcdrom_tochdr;
  tocentry : tcdrom_tocentry;

begin
  drive:=fpOpen(Device, Open_RDONLY or Open_NONBLOCK);
  if drive<0 then
    begin
    Result:=-1;
    Exit;
    end;
  if fpioctl(drive, CDROMREADTOCHDR, @tochdr)<>0 then
    begin
    Result:=-1;
    Exit;
    end;
  If (tochdr.cdth_trk1-tochdr.cdth_trk0)>High(CDToc) then
    Result:=-2
  else
    begin
    Result:=0;
    for i := tochdr.cdth_trk0 to tochdr.cdth_trk1 do
      begin
      tocentry.cdte_track := i;
      tocentry.cdte_format := CDROM_MSF;
      fpIOCtl(drive, CDROMREADTOCENTRY, @tocentry);
      // We should do some error checking here actually.
      With cdtoc[result] do
        begin
        min := tocentry.cdte_addr.msf.minute;
        sec := tocentry.cdte_addr.msf.second;
        frame := tocentry.cdte_addr.msf.frame;
        inc(frame,min*60*75);
        inc(frame,sec*75);
        end;
      Inc(result);
      end;
    tocentry.cdte_track := $AA;
    tocentry.cdte_format := CDROM_MSF;
    fpIOCtl(drive, CDROMREADTOCENTRY, @tocentry);
    With cdtoc[Result] do
      begin
      Min := tocentry.cdte_addr.msf.minute;
      sec := tocentry.cdte_addr.msf.second;
      frame := tocentry.cdte_addr.msf.frame;
      inc(frame, min*60*75);
      inc(frame, sec*75);
      end;
    end;
  fpClose(drive);
end;

{ ---------------------------------------------------------------------
    /etc/fstab scanning.
  ---------------------------------------------------------------------}

Function ExtractDevice(S : String) : String;

Var
  P,L : Integer;

begin
  Result:='';
  P:=Pos('#',S);
  If P<>0 then
    S:=Copy(S,1,P-1);
  If Length(S)>0 then
    begin
    P:=1;
    While (P<=Length(S)) and (S[P] in [#9,' ']) do
      Inc(p);
    L:=P;
    While (L<=Length(S)) and (Not (S[L] in [#9,' '])) do
      Inc(L);
    If L>P then
      Result:=Copy(S,P,L-P);
    end;
end;

Function TestFSTab(var Devices : Array of String) : Integer;

Var
  fstab : text;
  Line : String;

begin
  Result:=0;
  Assign(FSTab,'/etc/fstab');
  {$push}{$i-}
  Reset(fstab);
  {$pop}
  If IOResult=0 then
    begin
    While Not EOF(fstab) do
      begin
      ReadLn(fsTab,Line);
      Line:=ExtractDevice(Line);
      If IsCdDevice(Line) and (Result<=High(Devices)) then
        begin
        Devices[Result]:=Line;
        inc(Result);
        end;
      end;
    Close(fstab);
    end
  else
    Result:=-1;
end;

Function GetCDRomDevices(Var Devices : Array of string) : Integer;


  Function AlreadyAdded(AName: String; AMax: Integer): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 0 to AMax do
      if Devices[I] = AName then
        Exit(True);
  end;

  // Resolves name if it's a symlink and adds it ensuring no dups
  Function AddCdrom(ACDRom: String; I: Integer): Integer;
  var
    SInfo : stat;
    RealName: String;
  begin
    Result := I;
    if fpStat(PChar(ACDRom), SInfo) <> -1 then
    begin
      RealName := ACDRom;
      if SInfo.st_mode and S_IFMT = S_IFLNK then
        RealName := fpReadLink(ACDRom);

      if not AlreadyAdded(RealName, I-1) then
      begin
        Devices[I] := RealName;
        Result := I+1;
      end;
    end;
  end;

var
  I,J: Integer;
  CDRec: TCDSearchRec;
  FSTab: array[0..10] of String;

begin
  I := 0;
  // First Add Entries From FSTab
  for J := 0 to TestFSTab(FSTab)-1 do
    I := AddCdrom(FSTab[J], I);

  //Now Do A Search
  if FindFirstCD(CDRec) then
  repeat
    I := AddCdrom(CDRec.Name, I);
  until FindNextCD(CDRec) = False;
  Result := I;
end;



{%MainUnit sysutils.pp}
var
  lOldPat,lSrch: SRstring; // lSrch and lOldPat can contain uppercase versions of S,OldPattern
  lSLength,lPatLength,lNewPatLength,lPos,lCnt,lMatchIndex,lMatchesAlloc,lMatchesLimit: SizeInt;
  lDest: SRPChar;
  lMatches: PSizeInt;
  lMatchesStatic: array[0..63] of SizeInt;

begin
  aCount:=0;
  Result:='';

  lPatLength:=Length(OldPattern);
  if lPatLength=0 then
    exit(S);

  if rfIgnoreCase in Flags then begin
    lSrch:=SRUpperCase(S);
    lOldPat:=SRUpperCase(OldPattern);
  end else begin
    lSrch:=S;
    lOldPat:=OldPattern;
  end;

  lSLength:=Length(S);
  lPatLength:=Length(lOldPat);
  lNewPatLength:=Length(NewPattern);
  if lNewPatLength=lPatLength then begin
    //Result length will not change
    Result:=S;
    lPos:=1;
    repeat
      lPos:=Pos(lOldPat,lSrch,lPos);
      if lPos=0 then
        break;
      inc(aCount);
      move(NewPattern[1],Result[lPos],lPatLength*SizeOf(SRChar));
      inc(lPos,lPatLength);
    until not (rfReplaceAll in Flags);
  end else begin
    //Different pattern length: remember match positions in one scan, then build the result.
    //At most lMatchesLimit positions are stored, the matches after those are searched again.
    lMatches:=@lMatchesStatic[0];
    lMatchesAlloc:=Length(lMatchesStatic);
    try
      lPos:=1;
      repeat
        lPos:=Pos(lOldPat,lSrch,lPos);
        if lPos=0 then
          break;
        if aCount=lMatchesAlloc then
          begin
          lMatchesAlloc:=4+lMatchesAlloc+lMatchesAlloc div 2;
          lMatchesLimit:=256+lSLength*SizeOf(SRChar) div 8 div SizeOf(SizeInt);
          if lMatchesAlloc>lMatchesLimit then
            lMatchesAlloc:=lMatchesLimit;
          if lMatches=@lMatchesStatic[0] then
            begin
            lMatches:=GetMem(lMatchesAlloc*SizeOf(SizeInt));
            Move(lMatchesStatic,lMatches^,aCount*SizeOf(SizeInt));
            end
          else
            ReallocMem(lMatches,lMatchesAlloc*SizeOf(SizeInt));
          end;
        if aCount<lMatchesAlloc then
          lMatches[aCount]:=lPos;
        inc(lPos,lPatLength);
        inc(aCount);
      until not (rfReplaceAll in Flags);
      if aCount=0 then
        exit(S);
      SetLength(Result,lSLength+aCount*(lNewPatLength-lPatLength));
      lPos:=1;
      lDest:=SRPChar(Pointer(Result));
      lMatchIndex:=0;
      repeat
        if lMatchIndex<lMatchesAlloc then
          lCnt:=lMatches[lMatchIndex]-lPos
        else
          lCnt:=Pos(lOldPat,lSrch,lPos)-lPos;
        inc(lMatchIndex);
        Move(SRPChar(Pointer(S))[lPos-1],lDest^,lCnt*SizeOf(SRChar));
        Move(SRPChar(Pointer(NewPattern))^,lDest[lCnt],lNewPatLength*SizeOf(SRChar));
        inc(lDest,lCnt+lNewPatLength);
        inc(lPos,lCnt+lPatLength);
      until lMatchIndex=aCount;
      Move(SRPChar(Pointer(S))[lPos-1],lDest^,(lSLength-lPos+1)*SizeOf(SRChar));
    finally
      if lMatches<>@lMatchesStatic[0] then
        System.FreeMem(lMatches);
    end;
  end;
end;

(*
var
  Srch,OldP,RemS: SRString; // Srch and Oldp can contain uppercase versions of S,OldPattern
  P : Integer;
begin
  Srch:=S;
  OldP:=OldPattern;
  if rfIgnoreCase in Flags then
    begin
    Srch:=SRUpperCase(Srch);
    OldP:=SRUpperCase(OldP);
    end;
  RemS:=S;
  Result:='';
  while (Length(Srch)<>0) do
    begin
    P:=AnsiPos(OldP, Srch);
    if P=0 then
      begin
      Result:=Result+RemS;
      Srch:='';
      end
    else
      begin
      Result:=Result+Copy(RemS,1,P-1)+NewPattern;
      P:=P+Length(OldP);
      RemS:=Copy(RemS,P,Length(RemS)-P+1);
      if not (rfReplaceAll in Flags) then
        begin
        Result:=Result+RemS;
        Srch:='';
        end
      else
         Srch:=Copy(Srch,P,Length(Srch)-P+1);
      end;
    end;
end;
*)

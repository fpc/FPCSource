var
  OldPat,Srch: SRstring; // Srch and Oldp can contain uppercase versions of S,OldPattern
  PatLength,NewPatLength,P,Cnt,PrevP: Integer;
  c,d: SRPChar ;
  
begin
  aCount:=0;
  Result:='';
  c:= NIL; d:=NIL;
  OldPat:='';
  Srch:='';

  PatLength:=Length(OldPattern);
  if PatLength=0 then begin
    Result:=S;
    exit;
  end;

  if rfIgnoreCase in Flags then begin
    Srch:=SRUpperCase(S);
    OldPat:=SRUpperCase(OldPattern);
  end else begin
    Srch:=S;
    OldPat:=OldPattern;
  end;

  PatLength:=Length(OldPat);
  if Length(NewPattern)=PatLength then begin
    //Result length will not change
    Result:=S;
    P:=1;
    repeat
      P:=Pos(OldPat,Srch,P);
      if P>0 then begin
        inc(aCount);
        move(NewPattern[1],Result[P],PatLength*SizeOf(SRChar));
        if not (rfReplaceAll in Flags) then exit;
        inc(P,PatLength);
      end;
    until p=0;
  end else begin
    //Different pattern length -> Result length will change
    //To avoid creating a lot of temporary strings, we count how many
    //replacements we're going to make.
    P:=1;
    repeat
      P:=Pos(OldPat,Srch,P);
      if P>0 then begin
        inc(P,PatLength);
        inc(aCount);
        if not (rfReplaceAll in Flags) then break;
      end;
    until p=0;
    if aCount=0 then begin
      Result:=S;
      exit;
    end;
    NewPatLength:=Length(NewPattern);
    SetLength(Result,Length(S)+aCount*(NewPatLength-PatLength));
    P:=1; PrevP:=0;
    c:=SRPChar(Result); d:=SRPChar(S);
    repeat
      P:=Pos(OldPat,Srch,P);
      if P>0 then begin
        Cnt:=P-PrevP-1;
        if Cnt>0 then begin
          Move(d^,c^,Cnt*SizeOf(SRChar));
          inc(c,Cnt);
          inc(d,Cnt);
        end;
        if NewPatLength>0 then begin
          Move(NewPattern[1],c^,NewPatLength*SizeOf(SRChar));
          inc(c,NewPatLength);
        end;
        inc(P,PatLength);
        inc(d,PatLength);
        PrevP:=P-1;
        if not (rfReplaceAll in Flags) then break;
      end;
    until p=0;
    Cnt:=Length(S)-PrevP;
    if Cnt>0 then Move(d^,c^,Cnt*SizeOf(SRChar));
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

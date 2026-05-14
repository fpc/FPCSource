
function IsCJK_Unified_Ideographs(ACodePoint : Cardinal) : Boolean;inline;
begin
  Result := (ACodePoint >= $4E00) and (ACodePoint <= $9FFF); // $9FCC, $9FFF
end;

function IsCJK_Compatibility_Ideographs(ACodePoint : Cardinal) : Boolean;inline;
begin
  Result := (ACodePoint >= $F900) and (ACodePoint <= $FAFF);
end;

function IsTangut(ACodePoint : Cardinal) : Boolean;inline;
begin
  Result := (ACodePoint >= $17000) and (ACodePoint <= $187FF);
end;

function IsTangutComponent(ACodePoint : Cardinal) : Boolean;inline;
begin
  Result := (ACodePoint >= $18800) and (ACodePoint <= $18AFF);
end; 

function IsTangutSupplement(ACodePoint : Cardinal) : Boolean;inline;
begin
  Result := (ACodePoint >= $18D00) and (ACodePoint <= $18D7F);
end;

procedure DeriveWeightTangut(const ACodePoint : Cardinal; AResult : PUCA_PropWeights);inline;
begin
  AResult[0].Weights[0] := Word($FB00);
  AResult[0].Weights[1] := $20;
  AResult[0].Weights[2] := $2;

  AResult[1].Weights[0] := (ACodePoint - $17000) or $8000;
  AResult[1].Weights[1] := 0;
  AResult[1].Weights[2] := 0;
end;    

function IsNushu(ACodePoint : Cardinal) : Boolean;inline;
begin
  Result := (ACodePoint >= $1B170) and (ACodePoint <= $1B2FF);
end;

function IsKhitanSmallScript(ACodePoint : Cardinal) : Boolean;inline;
begin
  Result := (ACodePoint >= $18B00) and (ACodePoint <= $18CFF);
end;

procedure DeriveWeightNushu(const ACodePoint : Cardinal; AResult : PUCA_PropWeights);inline;
begin
  AResult[0].Weights[0] := Word($FB01);
  AResult[0].Weights[1] := $20;
  AResult[0].Weights[2] := $2;

  AResult[1].Weights[0] := (ACodePoint - $1B170) or $8000;
  AResult[1].Weights[1] := 0;
  AResult[1].Weights[2] := 0;
end; 

procedure DeriveWeightKhitanSmallScript(const ACodePoint : Cardinal; AResult : PUCA_PropWeights);inline;
begin
  AResult[0].Weights[0] := Word($FB02);
  AResult[0].Weights[1] := $20;
  AResult[0].Weights[2] := $2;

  AResult[1].Weights[0] := (ACodePoint - $18B00) or $8000;
  AResult[1].Weights[1] := 0;
  AResult[1].Weights[2] := 0;
end;

{$IFDEF UNI_BUILD_TIME} 
function isUnifiedIdeograph(const ACodePoint : Cardinal; const AUnifiedIdeographs : TCodePointRecArray) : boolean;
begin
  Result := IsIncluded(ACodePoint,AUnifiedIdeographs);
end;
{$ENDIF UNI_BUILD_TIME}

{$IFNDEF UNI_BUILD_TIME} 
function isUnifiedIdeograph(const ACodePoint : Cardinal) : boolean;
var
  p : PUC_Prop;
begin
  p := GetProps(ACodePoint);
  Result := (p <> nil) and p^.UnifiedIdeograph;
end;
{$ENDIF UNI_BUILD_TIME}

procedure DeriveWeight(
  const ACodePoint : Cardinal;
        AResult    : PUCA_PropWeights
{$IFDEF UNI_BUILD_TIME}
 ;const AUnifiedIdeographs : TCodePointRecArray
{$ENDIF UNI_BUILD_TIME}
);
const
  BASE_1 = Word($FB40);
  BASE_2 = Word($FB80);
  BASE_3 = Word($FBC0);
var
  base : Word;
  ui : boolean;
begin
  if IsTangut(ACodePoint) or
     IsTangutComponent(ACodePoint) or
     IsTangutSupplement(ACodePoint)
  then begin
    DeriveWeightTangut(ACodePoint,AResult);
  end else if IsNushu(ACodePoint) then begin                         
    DeriveWeightNushu(ACodePoint,AResult);   
  end else if IsKhitanSmallScript(ACodePoint) then begin
    DeriveWeightKhitanSmallScript(ACodePoint,AResult);
  end else begin
    ui := isUnifiedIdeograph(ACodePoint{$IFDEF UNI_BUILD_TIME},AUnifiedIdeographs{$ENDIF UNI_BUILD_TIME});
    if ui and
       (IsCJK_Unified_Ideographs(ACodePoint) or IsCJK_Compatibility_Ideographs(ACodePoint))
    then begin
      base := BASE_1
    end else if ui and
             not(IsCJK_Unified_Ideographs(ACodePoint) or IsCJK_Compatibility_Ideographs(ACodePoint))
    then begin
      base := BASE_2 
    end else begin
      base := BASE_3;
    end;

    AResult[0].Weights[0] := base + (ACodePoint shr 15);
    AResult[0].Weights[1] := $20;
    AResult[0].Weights[2] := $2;

    AResult[1].Weights[0] := (ACodePoint and $7FFF) or $8000;
    AResult[1].Weights[1] := 0;
    AResult[1].Weights[2] := 0;
  end;
end;

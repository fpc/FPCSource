
function IsCJK_Unified_Ideographs(ACodePoint : Cardinal) : Boolean;inline;
begin
  Result := (ACodePoint >= $4E00) and (ACodePoint <= $9FCC); // $9FFF
end;

function IsCJK_Compatibility_Ideographs(ACodePoint : Cardinal) : Boolean;inline;
begin
  Result := (ACodePoint >= $F900) and (ACodePoint <= $FAFF);
end;

function IsCJK_Unified_Ideographs_Extension_A(ACodePoint : Cardinal) : Boolean;inline;
begin
  Result := (ACodePoint >= $3400) and (ACodePoint <= $4DB5);  // $4DBF
end;

function IsCJK_Unified_Ideographs_Extension_B(ACodePoint : Cardinal) : Boolean;inline;
begin
  Result := (ACodePoint >= $20000) and (ACodePoint <= $2A6D6); // $2A6DF
end;

function IsCJK_Unified_Ideographs_Extension_C(ACodePoint : Cardinal) : Boolean;inline;
begin
  Result := (ACodePoint >= $2A700) and (ACodePoint <= $2B734); // $2B73F
end;

function IsCJK_Unified_Ideographs_Extension_D(ACodePoint : Cardinal) : Boolean;inline;
begin
  Result := (ACodePoint >= $2B740) and (ACodePoint <= $2B81D); // $2B81F
end;

function IsCJK_Compatibility_Ideographs_Supplement(ACodePoint : Cardinal) : Boolean;inline;
begin
  Result := (ACodePoint >= $2F800) and (ACodePoint <= $2FA1F);
end;

procedure DeriveWeight(const ACodePoint : Cardinal; AResult : PUCA_PropWeights);
const
  BASE_1 = Word($FB40);
  BASE_2 = Word($FB80);
  BASE_3 = Word($FBC0);
var
  base : Word;
begin
  if IsCJK_Unified_Ideographs(ACodePoint) or IsCJK_Compatibility_Ideographs(ACodePoint) then
    base := BASE_1
  else if IsCJK_Unified_Ideographs_Extension_A(ACodePoint) or
          IsCJK_Unified_Ideographs_Extension_B(ACodePoint) or
          IsCJK_Unified_Ideographs_Extension_C(ACodePoint) or
          IsCJK_Unified_Ideographs_Extension_D(ACodePoint) or
          IsCJK_Compatibility_Ideographs_Supplement(ACodePoint)
  then begin
    base := BASE_2;
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

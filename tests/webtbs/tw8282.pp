{ %opt=-O2 -Sew }

{$inline on}

type
  Int96 = packed record
    case Integer of
      0:
        (
          {$IFDEF ENDIAN_LITTLE}
          Lo32 : DWord;
          case Integer of
            0:
              (
                Mid32 : DWord;
                Hi32 : LongInt;
              );
            1:
              ( Hi64: Int64; );
          {$ELSE ENDIAN_LITTLE}
          Hi32 : LongInt;
          case Integer of
            0:
              (
                Mid32 : DWord;
                Lo32 : DWord;
              );
            1:
              ( Lo64: QWord; );
          {$ENDIF ENDIAN_LITTLE}
        );
      1:
        (
          {$IFDEF ENDIAN_LITTLE}
          Lo64 : QWord;
          {$ELSE ENDIAN_LITTLE}
          Hi64 : Int64;
          {$ENDIF ENDIAN_LITTLE}
        );
  end;

operator shl (const Left: Int96; const Right: LongInt) Result : Int96; forward;

operator shr (const Left: Int96; const Right: LongInt) Result : Int96; inline;
begin
  if Right >= 0 then
    if Right = 32 then begin
      Result.Lo32 := Left.Mid32;
      Result.Mid32 := Left.Hi32;
      Result.Hi32 := 0;
    end else if Right = 0 then begin
      Result.Lo32 := Left.Lo32;
      Result.Mid32 := Left.Mid32;
      Result.Hi32 := Left.Hi32;
    end else if Right = 64 then begin
      Result.Lo32 := Left.Hi32;
      Result.Mid32 := 0;
      Result.Hi32 := 0;
    end else if Right < 32 then begin
      Result.Hi32 := Left.Hi32 shr Right;
      Result.Mid32 := (Left.Mid32 shr Right) or (Left.Hi32 shl (32 - Right));
      Result.Lo32 := (Left.Lo32 shr Right) or (Left.Mid32 shl (32 - Right));
    end else if Right < 64 then begin
      Result.Hi32 := 0;
      Result.Mid32 := Left.Hi32 shr (Right-32);
      Result.Lo32 := (Left.Mid32 shr (Right-32)) or (Left.Hi32 shl (64 - Right));
    end else if Right < 96 then begin
      Result.Hi32 := 0;
      Result.Mid32 := 0;
      Result.Lo32 := Left.Hi32 shr (Right-64);
    end else begin
      Result.Lo32 := 0;
      Result.Mid32 := 0;
      Result.Hi32 := 0;
    end
  else
    Result := Left shl (-Right);
end;

operator shl (const Left: Int96; const Right: LongInt) Result : Int96; inline;
begin
  { ToDo: optimized code for 64bit cpu's }
  if Right >= 0 then
    if Right = 32 then begin
      Result.Lo32 := 0;
      Result.Mid32 := Left.Lo32;
      Result.Hi32 := Left.Mid32;
    end else if Right = 0 then begin
      Result.Lo32 := Left.Lo32;
      Result.Mid32 := Left.Mid32;
      Result.Hi32 := Left.Hi32;
    end else if Right = 64 then begin
      Result.Lo32 := 0;
      Result.Mid32 := 0;
      Result.Hi32 := Left.Lo32;
    end else if Right < 32 then begin
      Result.Lo32 := Left.Lo32 shl Right;
      Result.Mid32 := (Left.Mid32 shl Right) or (Left.Lo32 shr (32 - Right));
      Result.Hi32 := (Left.Hi32 shl Right) or (Left.Mid32 shr (32 - Right));
    end else if Right < 64 then begin
      Result.Lo32 := 0;
      Result.Mid32 := Left.Lo32 shl (Right-32);
      Result.Hi32 := (Left.Mid32 shl (Right-32)) or (Left.Lo32 shr (64 - Right));
    end else if Right < 96 then begin
      Result.Lo32 := 0;
      Result.Mid32 := 0;
      Result.Hi32 := Left.Lo32 shl (Right-64);
    end else begin
      Result.Lo32 := 0;
      Result.Mid32 := 0;
      Result.Hi32 := 0;
    end
  else
    Result := Left shr (-Right);
end;

operator := (const Right: QWord) Result : Int96; inline;
begin
  Result.Lo64  := Right;
  Result.Hi32  := 0;
end;


procedure t;
var
  a: int96;
begin
  a := 500000000000000;
  a := a shr 1;
  if (a.lo64 <> (500000000000000 shr 1)) or
     (a.hi32 <> 0) then
    halt(1);
end;

begin
  t;
end.

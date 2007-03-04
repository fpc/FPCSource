{ %opt=-O2 }

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

function f: int96;
begin
end;

operator := (const Right: QWord) Result : Int96; inline;
var
  c: int96;
begin
  c := f;
  Result.Lo64 := Right;
  Result.Hi32 := 0;
end;

procedure Test;
var
  a : Int96;
begin
  a := 500000000000000;
  if (a.lo64 <> 500000000000000) then
    halt(1);
end;

begin
  test
end.

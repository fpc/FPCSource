{
    Correctly rounded conversion of decimal strings to single and double:
    Eisel-Lemire on the first 19 significant digits, with exact decimal
    shifting for the cases it cannot decide.
}

{$push}
{$Q-}{$R-}

{$i flt_pow10.inc}

const
    C_FLT_DEC_MAX_DIGITS = 800;
    C_FLT_DEC_MAX_SHIFT  = 60;
    C_FLT_DEC_SLACK      = 20;

type
    // Decimal number 0.Digits[0]Digits[1].. * 10^Point, digits as values 0..9
    TFltDecimal = record
        Digits: array [ 0 .. C_FLT_DEC_MAX_DIGITS + C_FLT_DEC_SLACK - 1 ] of byte;
        Count, Point: longint;
        Truncated: boolean;
    end;

// Parses aSrc in val_real syntax into aDec; False for special values and syntax errors
function flt_parse_decimal(const aSrc: shortstring; out aDec: TFltDecimal; out aNeg: boolean): boolean;

var
    lPos, lLen, lExp: longint;
    lExpNeg: boolean;

begin
    Result := false;
    aNeg := false;
    aDec.Count := 0;
    aDec.Point := 0;
    aDec.Truncated := false;
    lLen := length(aSrc);
    lPos := 1;
    while (lPos <= lLen) and (aSrc[lPos] = ' ') do
        inc(lPos);
    if (lPos <= lLen) and (aSrc[lPos] in ['+', '-']) then
    begin
        aNeg := aSrc[lPos] = '-';
        inc(lPos);
    end;
    if (lPos > lLen) or not (aSrc[lPos] in ['0' .. '9', '.', 'E', 'e']) then
        exit;
    while (lPos <= lLen) and (aSrc[lPos] = '0') do
        inc(lPos);
    while (lPos <= lLen) and (aSrc[lPos] in ['0' .. '9']) do
    begin
        aDec.Digits[aDec.Count] := ord(aSrc[lPos]) - ord('0');
        inc(aDec.Count);
        inc(aDec.Point);
        inc(lPos);
    end;
    if (lPos <= lLen) and (aSrc[lPos] = '.') then
    begin
        inc(lPos);
        while (lPos <= lLen) and (aSrc[lPos] in ['0' .. '9']) do
        begin
            if (aDec.Count = 0) and (aSrc[lPos] = '0') then
                dec(aDec.Point)
            else
            begin
                aDec.Digits[aDec.Count] := ord(aSrc[lPos]) - ord('0');
                inc(aDec.Count);
            end;
            inc(lPos);
        end;
    end;
    lExp := 0;
    if lPos <= lLen then
    begin
        if not (aSrc[lPos] in ['E', 'e']) then
            exit;
        inc(lPos);
        if lPos > lLen then
            exit;
        lExpNeg := false;
        case aSrc[lPos] of
            '+':
                inc(lPos);
            '-':
                begin
                    lExpNeg := true;
                    inc(lPos);
                end;
        end;
        while lPos <= lLen do
        begin
            if not (aSrc[lPos] in ['0' .. '9']) then
                exit;
            if lExp < 100000 then
                lExp := lExp * 10 + ord(aSrc[lPos]) - ord('0');
            inc(lPos);
        end;
        if lExpNeg then
            lExp := -lExp;
    end;
    if aDec.Count > 0 then
        inc(aDec.Point, lExp)
    else
        aDec.Point := 0;
    Result := true;
end;


// Converts aMant * 10^aExp10 to IEEE bits; False when the result is not decided by 128 bits
function flt_eisel_lemire(aMant: qword; aExp10, aMantBits, aExpBits: longint; out aBits: qword): boolean;

var
    lClz, lShift, lExp2: longint;
    lMask, lXHi, lXLo, lYHi, lYLo, lMergedHi, lMergedLo, lMsb, lMant: qword;

begin
    Result := false;
    aBits := 0;
    if (aMant = 0) or (aExp10 < C_FLT_POW10_MIN) or (aExp10 > C_FLT_POW10_MAX) then
        exit;
    lClz := 63 - BsrQWord(aMant);
    aMant := aMant shl lClz;
    // floor(aExp10 * log2(10)) + 64 + bias - lClz
    lExp2 := SarLongint(217706 * aExp10, 16) + 64 + ((1 shl (aExpBits - 1)) - 1) - lClz;
    lShift := 64 - aMantBits - 3;
    lMask := (qword(1) shl lShift) - 1;
    lXLo := UMul64x64_128(aMant, flt_pow10_table[aExp10, 1], lXHi);
    if (lXHi and lMask = lMask) and (lXLo + aMant < aMant) then
    begin
        lYLo := UMul64x64_128(aMant, flt_pow10_table[aExp10, 0], lYHi);
        lMergedHi := lXHi;
        lMergedLo := lXLo + lYHi;
        if lMergedLo < lXLo then
            inc(lMergedHi);
        if (lMergedHi and lMask = lMask) and (lMergedLo = high(qword)) and (lYLo + aMant < aMant) then
            exit;
        lXHi := lMergedHi;
        lXLo := lMergedLo;
    end;
    lMsb := lXHi shr 63;
    lMant := lXHi shr (lMsb + lShift);
    dec(lExp2, longint(1 xor lMsb));
    // exactly halfway
    if (lXLo = 0) and (lXHi and lMask = 0) and (lMant and 3 = 1) then
        exit;
    lMant := (lMant + (lMant and 1)) shr 1;
    if lMant shr (aMantBits + 1) <> 0 then
    begin
        lMant := lMant shr 1;
        inc(lExp2);
    end;
    // subnormal, infinite or out of range
    if (lExp2 <= 0) or (lExp2 >= (1 shl aExpBits) - 1) then
        exit;
    aBits := (qword(lExp2) shl aMantBits) or (lMant and ((qword(1) shl aMantBits) - 1));
    Result := true;
end;


// Removes trailing zero digits from aDec
procedure flt_decimal_trim(var aDec: TFltDecimal);

begin
    while (aDec.Count > 0) and (aDec.Digits[aDec.Count - 1] = 0) do
        dec(aDec.Count);
    if aDec.Count = 0 then
        aDec.Point := 0;
end;


// Divides aDec by 2^aShift, with aShift <= C_FLT_DEC_MAX_SHIFT
procedure flt_decimal_right_shift(var aDec: TFltDecimal; aShift: longint);

var
    lRead, lWrite: longint;
    lNum, lMask, lDigit: qword;
    lNext: byte;

begin
    lRead := 0;
    lWrite := 0;
    lNum := 0;
    while lNum shr aShift = 0 do
    begin
        if lRead >= aDec.Count then
        begin
            if lNum = 0 then
            begin
                aDec.Count := 0;
                exit;
            end;
            while lNum shr aShift = 0 do
            begin
                lNum := lNum * 10;
                inc(lRead);
            end;
            break;
        end;
        lNum := lNum * 10 + aDec.Digits[lRead];
        inc(lRead);
    end;
    dec(aDec.Point, lRead - 1);
    lMask := (qword(1) shl aShift) - 1;
    while lRead < aDec.Count do
    begin
        lNext := aDec.Digits[lRead];
        lDigit := lNum shr aShift;
        lNum := lNum and lMask;
        aDec.Digits[lWrite] := lDigit;
        inc(lWrite);
        lNum := lNum * 10 + lNext;
        inc(lRead);
    end;
    while lNum > 0 do
    begin
        lDigit := lNum shr aShift;
        lNum := lNum and lMask;
        if lWrite < C_FLT_DEC_MAX_DIGITS then
        begin
            aDec.Digits[lWrite] := lDigit;
            inc(lWrite);
        end
        else if lDigit > 0 then
            aDec.Truncated := true;
        lNum := lNum * 10;
    end;
    aDec.Count := lWrite;
    flt_decimal_trim(aDec);
end;


// Multiplies aDec by 2^aShift, with aShift <= C_FLT_DEC_MAX_SHIFT
procedure flt_decimal_left_shift(var aDec: TFltDecimal; aShift: longint);

var
    lRead, lWrite, lCount, lDelta, i: longint;
    lNum, lQuo: qword;

begin
    Move(aDec.Digits[0], aDec.Digits[C_FLT_DEC_SLACK], aDec.Count);
    lRead := aDec.Count + C_FLT_DEC_SLACK;
    lWrite := lRead;
    lNum := 0;
    while lRead > C_FLT_DEC_SLACK do
    begin
        dec(lRead);
        inc(lNum, qword(aDec.Digits[lRead]) shl aShift);
        lQuo := lNum div 10;
        dec(lWrite);
        aDec.Digits[lWrite] := lNum - 10 * lQuo;
        lNum := lQuo;
    end;
    while lNum > 0 do
    begin
        lQuo := lNum div 10;
        dec(lWrite);
        aDec.Digits[lWrite] := lNum - 10 * lQuo;
        lNum := lQuo;
    end;
    lDelta := C_FLT_DEC_SLACK - lWrite;
    lCount := aDec.Count + lDelta;
    if lCount > C_FLT_DEC_MAX_DIGITS then
    begin
        for i := lWrite + C_FLT_DEC_MAX_DIGITS to lWrite + lCount - 1 do
            if aDec.Digits[i] <> 0 then
                aDec.Truncated := true;
        lCount := C_FLT_DEC_MAX_DIGITS;
    end;
    Move(aDec.Digits[lWrite], aDec.Digits[0], lCount);
    aDec.Count := lCount;
    inc(aDec.Point, lDelta);
    flt_decimal_trim(aDec);
end;


// Multiplies aDec by 2^aShift; a negative aShift divides
procedure flt_decimal_shift(var aDec: TFltDecimal; aShift: longint);

begin
    if aDec.Count = 0 then
        exit;
    if aShift > 0 then
    begin
        while aShift > C_FLT_DEC_MAX_SHIFT do
        begin
            flt_decimal_left_shift(aDec, C_FLT_DEC_MAX_SHIFT);
            dec(aShift, C_FLT_DEC_MAX_SHIFT);
        end;
        flt_decimal_left_shift(aDec, aShift);
    end
    else if aShift < 0 then
    begin
        while aShift < -C_FLT_DEC_MAX_SHIFT do
        begin
            flt_decimal_right_shift(aDec, C_FLT_DEC_MAX_SHIFT);
            inc(aShift, C_FLT_DEC_MAX_SHIFT);
        end;
        flt_decimal_right_shift(aDec, -aShift);
    end;
end;


// Returns the integer part of aDec rounded to nearest, ties to even
function flt_decimal_rounded_integer(const aDec: TFltDecimal): qword;

var
    i: longint;
    lUp: boolean;

begin
    Result := 0;
    i := 0;
    while (i < aDec.Point) and (i < aDec.Count) do
    begin
        Result := Result * 10 + aDec.Digits[i];
        inc(i);
    end;
    while i < aDec.Point do
    begin
        Result := Result * 10;
        inc(i);
    end;
    lUp := false;
    if (aDec.Point >= 0) and (aDec.Point < aDec.Count) then
        if (aDec.Digits[aDec.Point] = 5) and (aDec.Point + 1 = aDec.Count) then
            lUp := aDec.Truncated or ((aDec.Point > 0) and odd(aDec.Digits[aDec.Point - 1]))
        else
            lUp := aDec.Digits[aDec.Point] >= 5;
    if lUp then
        inc(Result);
end;


// Converts aDec to IEEE bits without sign, rounding to nearest, ties to even
function flt_decimal_to_bits(var aDec: TFltDecimal; aMantBits, aExpBits: longint): qword;

const
    CPowTab: array [ 0 .. 8 ] of byte = ( 1, 3, 6, 9, 13, 16, 19, 23, 26 );

var
    lBias, lExp, lShift, lExpLimit: longint;
    lMant: qword;
    lOverflow: boolean;

begin
    lBias := -((1 shl (aExpBits - 1)) - 1);
    lExpLimit := (1 shl aExpBits) - 1;
    lMant := 0;
    lExp := lBias;
    lOverflow := false;
    if (aDec.Count > 0) and (aDec.Point > 310) then
        lOverflow := true
    else if (aDec.Count > 0) and (aDec.Point >= -330) then
    begin
        // scale to [0.5, 1)
        lExp := 0;
        while aDec.Point > 0 do
        begin
            if aDec.Point > high(CPowTab) then
                lShift := 27
            else
                lShift := CPowTab[aDec.Point];
            flt_decimal_shift(aDec, -lShift);
            inc(lExp, lShift);
        end;
        while (aDec.Point < 0) or ((aDec.Point = 0) and (aDec.Digits[0] < 5)) do
        begin
            if -aDec.Point > high(CPowTab) then
                lShift := 27
            else
                lShift := CPowTab[-aDec.Point];
            flt_decimal_shift(aDec, lShift);
            dec(lExp, lShift);
        end;
        dec(lExp);
        if lExp < lBias + 1 then
        begin
            lShift := lBias + 1 - lExp;
            flt_decimal_shift(aDec, -lShift);
            inc(lExp, lShift);
        end;
        if lExp - lBias >= lExpLimit then
            lOverflow := true
        else
        begin
            flt_decimal_shift(aDec, 1 + aMantBits);
            lMant := flt_decimal_rounded_integer(aDec);
            if lMant = qword(2) shl aMantBits then
            begin
                lMant := lMant shr 1;
                inc(lExp);
                if lExp - lBias >= lExpLimit then
                    lOverflow := true;
            end;
            // subnormal
            if lMant and (qword(1) shl aMantBits) = 0 then
                lExp := lBias;
        end;
    end;
    if lOverflow then
    begin
        lMant := 0;
        lExp := lExpLimit + lBias;
    end;
    Result := (lMant and ((qword(1) shl aMantBits) - 1)) or (qword((lExp - lBias) and lExpLimit) shl aMantBits);
end;


// Converts aSrc to IEEE bits; False for special values and syntax errors
function flt_str_to_bits(const aSrc: shortstring; aMantBits, aExpBits: longint; out aBits: qword): boolean;

var
    lDec: TFltDecimal;
    lNeg, lTrunc: boolean;
    lMant, lUpBits: qword;
    lCount, lExp10, i: longint;

begin
    aBits := 0;
    Result := flt_parse_decimal(aSrc, lDec, lNeg);
    if not Result then
        exit;
    if lDec.Count > 0 then
    begin
        lCount := lDec.Count;
        if lCount > 19 then
            lCount := 19;
        lMant := 0;
        for i := 0 to lCount - 1 do
            lMant := lMant * 10 + lDec.Digits[i];
        lTrunc := false;
        for i := lCount to lDec.Count - 1 do
            if lDec.Digits[i] <> 0 then
                lTrunc := true;
        lExp10 := lDec.Point - lCount;
        if not flt_eisel_lemire(lMant, lExp10, aMantBits, aExpBits, aBits)
           or (lTrunc and not (flt_eisel_lemire(lMant + 1, lExp10, aMantBits, aExpBits, lUpBits) and (lUpBits = aBits))) then
            aBits := flt_decimal_to_bits(lDec, aMantBits, aExpBits);
    end;
    if lNeg then
        aBits := aBits or (qword(1) shl (aMantBits + aExpBits));
end;


// Converts aSrc to the nearest double; aCode as for Val
function val_double(const aSrc: shortstring; out aCode: ValSInt): double;

type
    TSplitDouble = packed record
        case byte of
            0: ( d: double );
            1: ( q: qword );
            2: ( l: array [ 0 .. 1 ] of dword );
    end;

var
    lBits: qword;
    lSplit: TSplitDouble;

begin
    if not flt_str_to_bits(aSrc, 52, 11, lBits) then
    begin
        Result := val_real(aSrc, aCode);
        exit;
    end;
{$ifdef FPC_DOUBLE_HILO_SWAPPED}
    lSplit.l[0] := hi(lBits);
    lSplit.l[1] := lo(lBits);
{$else}
    lSplit.q := lBits;
{$endif}
    aCode := 0;
    Result := lSplit.d;
end;


// Converts aSrc to the nearest single; aCode as for Val
function val_single(const aSrc: shortstring; out aCode: ValSInt): single;

type
    TSplitSingle = packed record
        case byte of
            0: ( s: single );
            1: ( d: dword );
    end;

var
    lBits: qword;
    lSplit: TSplitSingle;

begin
    if not flt_str_to_bits(aSrc, 23, 8, lBits) then
    begin
        Result := val_real(aSrc, aCode);
        exit;
    end;
    lSplit.d := lo(lBits);
    aCode := 0;
    Result := lSplit.s;
end;

{$pop}

function IntFloatToTextFmt(Buf: FPChar; const Value; ValueType: TFloatValue; Format: FPChar; const AFormatSettings: TFormatSettings): Integer;

Type
  TPosArray = Array[0..3] of Integer;

const
  MaxPrecision = 18;  // Extended precision

var
  // Input in usable format
  E : Extended;              // Value as extended.
  FV: TFloatRec;             // Value as floatrec.
  Section : String;          // Format can contain 3 sections, semicolon separated: Pos;Neg;Zero. This is the one to use.
  SectionLength : Integer;   // Length of section.
  // Calculated based on section. Static during output
  ThousandSep: Boolean;      // Thousands separator detected in format ?
  IsScientific: Boolean;     // Use Scientific notation ? (E detected in format)
  DecimalPos: Integer;       // Position of decimal point in pattern.
  FirstDigit: Integer;       // First actual digit in input (# or 0), relative to decimal point
  LastDigit: Integer;        // Last required (0) digit, relative to decimal point
  RequestedDigits: Integer;  // Number of requested digits, # and 0 alike
  ExpSize : Integer;         // Number of digits in exponent
  Available: Integer;        // Available digits in FV.
  // These change during output loop
  Current: Integer;          // Current digit in available digits
  PadZeroes: Integer;        // Difference in requested digits before comma and exponent, needs to be padded with zeroes.
  DistToDecimal: Integer;    // Place of current digit, relative to decimal point taking in account PadZeroes!

  Procedure InitVars;

  begin
    if ValueType = fvCurrency then
      E:=Currency(Value)
    else
      E:=Extended(Value);
    Section:='';
    SectionLength:=0;
    ThousandSep:=false;
    IsScientific:=false;
    DecimalPos:=0;
    FirstDigit:=MaxInt;
    LastDigit:=0;
    RequestedDigits:=0;
    ExpSize:=0;
    Available:=-1;
  end;

  procedure ToResult(const AChar: FChar); inline;
  begin
    Buf[Result]:=AChar;
    Inc(Result);
    // Writeln('->',AChar,'(',Ord(AChar),') : ',Result);
  end;


  procedure AddToResult(const AStr: FString);
  var
    I : Integer;
  begin
    For I:=1 to Length(AStr) do
      ToResult(AStr[I]);
  end;

  procedure WriteDigit(ADigit: FChar);

  // Write a digit to result, prepend with decimalseparator or append with 1000 separator

  begin
    if ADigit=#0 then exit;
    // Writeln('WriteDigit: ',ADigit,', DistToDecimal: ',DistToDecimal);
    Dec(DistToDecimal);
    // -1 -> we've arrived behind the decimal
    if (DistToDecimal=-1) then
      begin
      ToResult(AFormatSettings.DecimalSeparator);
      ToResult(ADigit);
      end
    else
      begin
      // We're still before the decimal.
      ToResult(ADigit);
      if ThousandSep and ((DistToDecimal mod 3)=0) and (DistToDecimal>1) then
        ToResult(AFormatSettings.ThousandSeparator);
      end;
  end;

  Function GetDigit : FChar;

  // Return next digit from available digits.
  // May return #0 if none available.
  // Will return '0' if applicable.

  begin
    // Writeln(' DistToDecimal <= LastDigit : ',DistToDecimal,' <  ',LastDigit,' have digit: ',Current<=Available, '(',Current,')');
    Result:=#0;
    if (Current<=Available) then
      begin
      Result:=FV.Digits[Current];
      Inc(Current);
      end
    else if (DistToDecimal <= LastDigit) then
      Dec(DistToDecimal)
    else
      Result:='0';
    //  Writeln('GetDigit ->: ',Result);
  end;

  procedure CopyDigit;

  // Copy a digit (#, 0) to the output with the correct value

  begin
    // Writeln('CopyDigit ');
    if (PadZeroes=0) then
      WriteDigit(GetDigit) // No shift needed, just copy what is available.
    else if (PadZeroes<0) then
      begin
      // We must prepend zeroes
      Inc(PadZeroes);
      if (DistToDecimal<=FirstDigit) then
        WriteDigit('0')
      else
        Dec(DistToDecimal);
      end
    else
      begin
      // We must append zeroes
      while PadZeroes > 0 do
        begin
        WriteDigit(GetDigit);
        Dec(PadZeroes);
        end;
      WriteDigit(GetDigit);
      end;
  end;

  Function GetSections(Var SP : TPosArray) : Integer;

  var
    FL : Integer;
    i : Integer;
    C,Q : FChar;
    inQuote : Boolean;

  begin
    Result:=1;
    SP[1]:=-1;
    SP[2]:=-1;
    SP[3]:=-1;
    inQuote:=False;
    Q:=#0;
    I:=0;
    FL:=StrLen(Format);
    while (I<FL) do
      begin
      C:=Format[I];
      case C of
      ';':
        begin
        if not InQuote then
          begin
          if Result>3 then
            Raise Exception.Create('Invalid float format');
          SP[Result]:=I+1;
          Inc(Result);
          end;
        end;
      '"','''':
        begin
        if InQuote then
          InQuote:=C<>Q
        else
          begin
          InQuote:=True;
          Q:=C;
          end;
        end;
      end;
      Inc(I);
      end;
    if SP[Result]=-1 then
      SP[Result]:=FL+1;
  end;

  Procedure AnalyzeFormat;

  var
    I,Len: Integer;
    Q,C: FChar;
    InQuote : Boolean;

  begin
    Len:=Length(Section);
    I:=1;
    InQuote:=False;
    Q:=#0;
    while (I<=Len) do
      begin
      C:=Section[i];
      if C in ['"',''''] then
        begin
        if InQuote then
          InQuote:=C<>Q
        else
          begin
          InQuote:=True;
          Q:=C;
          end;
        end
      else if not InQuote then
        case C of
        '.':
          if (DecimalPos=0) then
            DecimalPos:=RequestedDigits+1;
        ',':
            ThousandSep:=AFormatSettings.ThousandSeparator<>#0;
        'e', 'E':
            begin
            Inc(I);
            if (I<Len) then
              begin
              C:=Section[i];
              IsScientific:=C in ['-','+'];
              if IsScientific then
                while (I<Len) and (Section[i+1]='0') do
                  begin
                  Inc(ExpSize);
                  Inc(I);
                  end;
              if ExpSize>4 then
                ExpSize:=4;
              end;
            end;
        '#':
            Inc(RequestedDigits);
        '0':
            begin
            if RequestedDigits<FirstDigit then
              FirstDigit:=RequestedDigits+1;
            Inc(RequestedDigits);
            LastDigit:=RequestedDigits+1;
            end;
        end;
      Inc(I);
      end;
    if DecimalPos=0 then
      DecimalPos:=RequestedDigits+1;
    // Writeln('LastDigit: ',DecimalPos,'-',LastDigit);
    LastDigit:=DecimalPos-LastDigit;
    if LastDigit>0 then
      LastDigit:=0;
    // Writeln('FirstDigit: ',DecimalPos,'-',FirstDigit);
    FirstDigit:=DecimalPos-FirstDigit;
    if FirstDigit<0 then
      FirstDigit:=0;
  end;

  Function ValueOutSideScope : Boolean;
  begin
    With FV do
     Result:=((Exponent >= 18) and (not IsScientific)) or (Exponent = $7FF) or (Exponent = $800)
  end;

  Procedure CalcRunVars;

  Var
    D,P: Integer;

  begin
    if IsScientific then
      begin
      P:=RequestedDigits;
      D:=9999;
      end
    else
      begin
      P:=MaxPrecision;
      D:=RequestedDigits-DecimalPos+1;
      end;
    FloatToDecimal(FV,Value,ValueType,P,D);
    DistToDecimal:=DecimalPos-1;
    if IsScientific then
      PadZeroes:=0 // No padding.
    else
      begin
      PadZeroes:=FV.Exponent-(DecimalPos-1);
      if (PadZeroes>=0) then
        DistToDecimal:=FV.Exponent
      end;
    // Writeln('PadZeroes : ',PadZeroes, ', DistToDecimal : ',DistToDecimal);
    Available:=-1;
    while (Available<High(FV.Digits)) and (FV.Digits[Available+1]<>#0) do
      Inc(Available);
    // Writeln('Available: ',Available);
  end;


  Function FormatExponent(ASign: FChar; aExponent: Integer) : FString;

  begin
    if E = 0 then
      aExponent := 0;
    Result:=IntToStr(Abs(aExponent));
    Result:=StringOfChar('0',ExpSize-Length(Result))+Result;
    if (aExponent<0) then
      Result:='-'+Result
    else if (aExponent>=0) and (aSign='+') then
      Result:=aSign+Result;
  end;

var
  I,S : Integer;
  C,Q : FChar;
  PA : TPosArray;
  InLiteral : Boolean;

begin
  Result:=0;
  Initvars;
  // What section to use ?
  if (E>0) then
    S:=1
  else if (E<0) then
    S:=2
  else
    S:=3;
  PA[0]:=0;
  I:=GetSections(PA);
  if (I<S) or (PA[S]-PA[S-1]=0) then
    S:=1;
  // Extract correct section
  SectionLength:=PA[S]-PA[S-1]-1;
  SetLength(Section,SectionLength);
  Move(Format[PA[S-1]],Section[1],SizeOf(FChar)*SectionLength);
  // Writeln('Section ',I,' : "',Section,'" ',SectionLength);
  AnalyzeFormat;
  // Writeln('RequestedDigits: ',RequestedDigits,', DecimalPos : ',DecimalPos,', LastDigit: ',LastDigit,', FirstDigit: ',FirstDigit);
  CalcRunVars;
  // If we cannot process value using current settings, fallback
  if (SectionLength=0) or ValueOutSideScope then
    Exit(FloatToText(FPChar(Buf), E, ffGeneral, 15, 0, AFormatSettings));
  // Get Started
  I:=1;
  Current:=0;
  Q:=' ';
  InLiteral:=False;
  if (FV.Negative) and (S=1) then
    ToResult('-');
  while (I<=SectionLength) do
    begin
    C:=Section[i];
    // Writeln('Analyzing pos ',I,': "',C,'"');
    If (C in ['"', '''']) then
      begin
      if InLiteral then
        InLiteral:=C<>Q
      else
        begin
        inLiteral:=True;
        Q:=C;
        end;
      end
    else if InLiteral then
      ToResult(C)
    else
      case C of
      '0', '#':
        CopyDigit;
      '.', ',':
        ; // Do nothing, handled by CopyDigit
      'e', 'E':
        begin
        ToResult(C); // Always needed
        if IsScientific then
          begin
          Inc(I);
          if I<=Section.Length then
            begin
            C:=Section[I];
            if (C in ['+','-']) then
              begin
              AddToResult(FormatExponent(C,FV.Exponent-DecimalPos+1));
              // Skip rest
              while (I<SectionLength) and (Section[i+1]='0') do
                Inc(I);
              end;
            end;
          end
        else if I< SectionLength Then
          begin
          inc(I);
          ToResult(Section[i]);
          end;
        end;
      else
        ToResult(C);
      end;
    Inc(i);
    end;
//  Writeln('Result  ',Result);
end;


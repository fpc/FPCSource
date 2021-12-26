type
  { TUInt16Bit1Test }

  TUInt16Bit1Test = class(TUInt16DivTest)
    protected
      function GetDivisor: Word; override;
      procedure DoTestIteration(Iteration: Integer); override;
    public
      function TestTitle: shortstring; override;
  end;

  { TUInt16Bit1ModTest }

  TUInt16Bit1ModTest = class(TUInt16ModTest)
    protected
      function GetDivisor: Word; override;
      procedure DoTestIteration(Iteration: Integer); override;
    public
      function TestTitle: shortstring; override;
  end;

  { TUInt16Bit2Test }

  TUInt16Bit2Test = class(TUInt16DivTest)
    protected
      function GetDivisor: Word; override;
      procedure DoTestIteration(Iteration: Integer); override;
    public
      function TestTitle: shortstring; override;
  end;

  { TUInt16Bit2ModTest }

  TUInt16Bit2ModTest = class(TUInt16ModTest)
    protected
      function GetDivisor: Word; override;
      procedure DoTestIteration(Iteration: Integer); override;
    public
      function TestTitle: shortstring; override;
  end;

  { TUInt16Bit3Test }

  TUInt16Bit3Test = class(TUInt16DivTest)
    protected
      function GetDivisor: Word; override;
      procedure DoTestIteration(Iteration: Integer); override;
    public
      function TestTitle: shortstring; override;
  end;

  { TUInt16Bit3ModTest }

  TUInt16Bit3ModTest = class(TUInt16ModTest)
    protected
      function GetDivisor: Word; override;
      procedure DoTestIteration(Iteration: Integer); override;
    public
      function TestTitle: shortstring; override;
  end;

  { TUInt16Bit7Test }

  TUInt16Bit7Test = class(TUInt16DivTest)
    protected
      function GetDivisor: Word; override;
      procedure DoTestIteration(Iteration: Integer); override;
    public
      function TestTitle: shortstring; override;
  end;

  { TUInt16Bit7ModTest }

  TUInt16Bit7ModTest = class(TUInt16ModTest)
    protected
      function GetDivisor: Word; override;
      procedure DoTestIteration(Iteration: Integer); override;
    public
      function TestTitle: shortstring; override;
  end;

  { TUInt16Bit10Test }

  TUInt16Bit10Test = class(TUInt16DivTest)
    protected
      function GetDivisor: Word; override;
      procedure DoTestIteration(Iteration: Integer); override;
    public
      function TestTitle: shortstring; override;
  end;

  { TUInt16Bit10ModTest }

  TUInt16Bit10ModTest = class(TUInt16ModTest)
    protected
      function GetDivisor: Word; override;
      procedure DoTestIteration(Iteration: Integer); override;
    public
      function TestTitle: shortstring; override;
  end;

  { TUInt16Bit100Test }

  TUInt16Bit100Test = class(TUInt16DivTest)
    protected
      function GetDivisor: Word; override;
      procedure DoTestIteration(Iteration: Integer); override;
    public
      function TestTitle: shortstring; override;
  end;

  { TUInt16Bit100ModTest }

  TUInt16Bit100ModTest = class(TUInt16ModTest)
    protected
      function GetDivisor: Word; override;
      procedure DoTestIteration(Iteration: Integer); override;
    public
      function TestTitle: shortstring; override;
  end;

  { TUInt16Bit1000Test }
const
  FU16_1000Input: array[$0..$F] of Word =
    (0, 1, 999, 1000, 1001, 1999, 2000, 2001,
    64999, 65000, 65001,
    $7FFE, $7FFF, $8000, $8001, $FFFF);

type
  TUInt16Bit1000Test = class(TUInt16DivTest)
    protected
      function GetDivisor: Word; override;
      procedure DoTestIteration(Iteration: Integer); override;
    public
      function TestTitle: shortstring; override;
  end;

  { TUInt16Bit1000ModTest }

  TUInt16Bit1000ModTest = class(TUInt16ModTest)
    protected
      function GetDivisor: Word; override;
      procedure DoTestIteration(Iteration: Integer); override;
    public
      function TestTitle: shortstring; override;
  end;


{ TUInt16Bit1Test }

function TUInt16Bit1Test.TestTitle: shortstring;
  begin
    Result := 'Unsigned 16-bit division by 1';
  end;

function TUInt16Bit1Test.GetDivisor: Word;
  begin
    Result := 1;
  end;

procedure TUInt16Bit1Test.DoTestIteration(Iteration: Integer);
  var
    Numerator, Answer: Word;
    Index, X: Integer;
  begin
    Index := Iteration and $FF;
    case Index of
      253:
        Numerator := 65533;
      254:
        Numerator := 65534;
      255:
        Numerator := 65535;
      else
        Numerator := Word(Index);
    end;

    FInputArray[Index] := Numerator;
    for X := 0 to INTERNAL_LOOPS - 1 do
      Answer := Numerator div 1;

    FResultArray[Index] := Answer;
  end;

{ TUInt16Bit1Test }

function TUInt16Bit1ModTest.TestTitle: shortstring;
  begin
    Result := 'Unsigned 16-bit modulus by 1';
  end;

function TUInt16Bit1ModTest.GetDivisor: Word;
  begin
    Result := 1;
  end;

procedure TUInt16Bit1ModTest.DoTestIteration(Iteration: Integer);
  var
    Numerator, Answer: Word;
    Index, X: Integer;
  begin
    Index := Iteration and $FF;
    case Index of
      253:
        Numerator := 65533;
      254:
        Numerator := 65534;
      255:
        Numerator := 65535;
      else
        Numerator := Word(Index);
    end;

    FInputArray[Index] := Numerator;
    for X := 0 to INTERNAL_LOOPS - 1 do
      Answer := Numerator mod 1;

    FResultArray[Index] := Answer;
  end;

{ TUInt16Bit2Test }

function TUInt16Bit2Test.TestTitle: shortstring;
  begin
    Result := 'Unsigned 16-bit division by 2';
  end;

function TUInt16Bit2Test.GetDivisor: Word;
  begin
    Result := 2;
  end;

procedure TUInt16Bit2Test.DoTestIteration(Iteration: Integer);
  var
    Numerator, Answer: Word;
    Index, X: Integer;
  begin
    Index := Iteration and $FF;
    case Index of
      253:
        Numerator := 65533;
      254:
        Numerator := 65534;
      255:
        Numerator := 65535;
      else
        Numerator := Word(Index);
    end;

    FInputArray[Index] := Numerator;
    for X := 0 to INTERNAL_LOOPS - 1 do
      Answer := Numerator div 2;

    FResultArray[Index] := Answer;
  end;

{ TUInt16Bit2ModTest }

function TUInt16Bit2ModTest.TestTitle: shortstring;
  begin
    Result := 'Unsigned 16-bit modulus by 2';
  end;

function TUInt16Bit2ModTest.GetDivisor: Word;
  begin
    Result := 2;
  end;

procedure TUInt16Bit2ModTest.DoTestIteration(Iteration: Integer);
  var
    Numerator, Answer: Word;
    Index, X: Integer;
  begin
    Index := Iteration and $FF;
    case Index of
      253:
        Numerator := 65533;
      254:
        Numerator := 65534;
      255:
        Numerator := 65535;
      else
        Numerator := Word(Index);
    end;

    FInputArray[Index] := Numerator;
    for X := 0 to INTERNAL_LOOPS - 1 do
      Answer := Numerator mod 2;

    FResultArray[Index] := Answer;
  end;

{ TUInt16Bit3Test }

function TUInt16Bit3Test.TestTitle: shortstring;
  begin
    Result := 'Unsigned 16-bit division by 3';
  end;

function TUInt16Bit3Test.GetDivisor: Word;
  begin
    Result := 3;
  end;

procedure TUInt16Bit3Test.DoTestIteration(Iteration: Integer);
  var
    Numerator, Answer: Word;
    Index, X: Integer;
  begin
    Index := Iteration and $FF;
    case Index of
      253:
        Numerator := 65533;
      254:
        Numerator := 65534;
      255:
        Numerator := 65535;
      else
        Numerator := Word(Index);
    end;

    FInputArray[Index] := Numerator;
    for X := 0 to INTERNAL_LOOPS - 1 do
      Answer := Numerator div 3;

    FResultArray[Index] := Answer;
  end;

{ TUInt16Bit3ModTest }

function TUInt16Bit3ModTest.TestTitle: shortstring;
  begin
    Result := 'Unsigned 16-bit modulus by 3';
  end;

function TUInt16Bit3ModTest.GetDivisor: Word;
  begin
    Result := 3;
  end;

procedure TUInt16Bit3ModTest.DoTestIteration(Iteration: Integer);
  var
    Numerator, Answer: Word;
    Index, X: Integer;
  begin
    Index := Iteration and $FF;
    case Index of
      253:
        Numerator := 65533;
      254:
        Numerator := 65534;
      255:
        Numerator := 65535;
      else
        Numerator := Word(Index);
    end;

    FInputArray[Index] := Numerator;
    for X := 0 to INTERNAL_LOOPS - 1 do
      Answer := Numerator mod 3;

    FResultArray[Index] := Answer;
  end;

{ TUInt16Bit7Test }

function TUInt16Bit7Test.TestTitle: shortstring;
  begin
    Result := 'Unsigned 16-bit division by 7';
  end;

function TUInt16Bit7Test.GetDivisor: Word;
  begin
    Result := 7;
  end;

procedure TUInt16Bit7Test.DoTestIteration(Iteration: Integer);
  var
    Numerator, Answer: Word;
    Index, X: Integer;
  begin
    Index := Iteration and $FF;
    case Index of
      253:
        Numerator := 65533;
      254:
        Numerator := 65534;
      255:
        Numerator := 65535;
      else
        Numerator := Word(Index);
    end;

    FInputArray[Index] := Numerator;
    for X := 0 to INTERNAL_LOOPS - 1 do
      Answer := Numerator div 7;

    FResultArray[Index] := Answer;
  end;

{ TUInt16Bit7ModTest }

function TUInt16Bit7ModTest.TestTitle: shortstring;
  begin
    Result := 'Unsigned 16-bit modulus by 7';
  end;

function TUInt16Bit7ModTest.GetDivisor: Word;
  begin
    Result := 7;
  end;

procedure TUInt16Bit7ModTest.DoTestIteration(Iteration: Integer);
  var
    Numerator, Answer: Word;
    Index, X: Integer;
  begin
    Index := Iteration and $FF;
    case Index of
      253:
        Numerator := 65533;
      254:
        Numerator := 65534;
      255:
        Numerator := 65535;
      else
        Numerator := Word(Index);
    end;

    FInputArray[Index] := Numerator;
    for X := 0 to INTERNAL_LOOPS - 1 do
      Answer := Numerator mod 7;

    FResultArray[Index] := Answer;
  end;

{ TUInt16Bit10Test }

function TUInt16Bit10Test.TestTitle: shortstring;
  begin
    Result := 'Unsigned 16-bit division by 10';
  end;

function TUInt16Bit10Test.GetDivisor: Word;
  begin
    Result := 10;
  end;

procedure TUInt16Bit10Test.DoTestIteration(Iteration: Integer);
  var
    Numerator, Answer: Word;
    Index, X: Integer;
  begin
    Index := Iteration and $FF;
    case Index of
      253:
        Numerator := 65533;
      254:
        Numerator := 65534;
      255:
        Numerator := 65535;
      else
        Numerator := Word(Index);
    end;

    FInputArray[Index] := Numerator;
    for X := 0 to INTERNAL_LOOPS - 1 do
      Answer := Numerator div 10;

    FResultArray[Index] := Answer;
  end;

{ TUInt16Bit10ModTest }

function TUInt16Bit10ModTest.TestTitle: shortstring;
  begin
    Result := 'Unsigned 16-bit modulus by 10';
  end;

function TUInt16Bit10ModTest.GetDivisor: Word;
  begin
    Result := 10;
  end;

procedure TUInt16Bit10ModTest.DoTestIteration(Iteration: Integer);
  var
    Numerator, Answer: Word;
    Index, X: Integer;
  begin
    Index := Iteration and $FF;
    case Index of
      253:
        Numerator := 65533;
      254:
        Numerator := 65534;
      255:
        Numerator := 65535;
      else
        Numerator := Word(Index);
    end;

    FInputArray[Index] := Numerator;
    for X := 0 to INTERNAL_LOOPS - 1 do
      Answer := Numerator mod 10;

    FResultArray[Index] := Answer;
  end;

{ TUInt16Bit100Test }

function TUInt16Bit100Test.TestTitle: shortstring;
  begin
    Result := 'Unsigned 16-bit division by 100';
  end;

function TUInt16Bit100Test.GetDivisor: Word;
  begin
    Result := 100;
  end;

procedure TUInt16Bit100Test.DoTestIteration(Iteration: Integer);
  var
    Numerator, Answer: Word;
    Index, X: Integer;
  begin
    Index := Iteration and $FF;
    case Index of
      253:
        Numerator := 65533;
      254:
        Numerator := 65534;
      255:
        Numerator := 65535;
      else
        Numerator := Word(Index);
    end;

    FInputArray[Index] := Numerator;
    for X := 0 to INTERNAL_LOOPS - 1 do
      Answer := Numerator div 100;

    FResultArray[Index] := Answer;
  end;

{ TUInt16Bit100ModTest }

function TUInt16Bit100ModTest.TestTitle: shortstring;
  begin
    Result := 'Unsigned 16-bit modulus by 100';
  end;

function TUInt16Bit100ModTest.GetDivisor: Word;
  begin
    Result := 100;
  end;

procedure TUInt16Bit100ModTest.DoTestIteration(Iteration: Integer);
  var
    Numerator, Answer: Word;
    Index, X: Integer;
  begin
    Index := Iteration and $FF;
    case Index of
      253:
        Numerator := 65533;
      254:
        Numerator := 65534;
      255:
        Numerator := 65535;
      else
        Numerator := Word(Index);
    end;

    FInputArray[Index] := Numerator;
    for X := 0 to INTERNAL_LOOPS - 1 do
      Answer := Numerator mod 100;

    FResultArray[Index] := Answer;
  end;

{ TUInt16Bit1000Test }

function TUInt16Bit1000Test.TestTitle: shortstring;
  begin
    Result := 'Unsigned 16-bit division by 1,000';
  end;

function TUInt16Bit1000Test.GetDivisor: Word;
  begin
    Result := 1000;
  end;

procedure TUInt16Bit1000Test.DoTestIteration(Iteration: Integer);
  var
    Numerator, Answer: Word;
    Index, X: Integer;
  begin
    Index := Iteration and $FF;
    Numerator := FU16_1000Input[Index and $F];
    FInputArray[Index] := Numerator;
    for X := 0 to INTERNAL_LOOPS - 1 do
      Answer := Numerator div 1000;

    FResultArray[Index] := Answer;
  end;

{ TUInt16Bit1000ModTest }

function TUInt16Bit1000ModTest.TestTitle: shortstring;
  begin
    Result := 'Unsigned 16-bit modulus by 1,000';
  end;

function TUInt16Bit1000ModTest.GetDivisor: Word;
  begin
    Result := 1000;
  end;

procedure TUInt16Bit1000ModTest.DoTestIteration(Iteration: Integer);
  var
    Numerator, Answer: Word;
    Index, X: Integer;
  begin
    Index := Iteration and $FF;
    Numerator := FU16_1000Input[Index and $F];
    FInputArray[Index] := Numerator;
    for X := 0 to INTERNAL_LOOPS - 1 do
      Answer := Numerator mod 1000;

    FResultArray[Index] := Answer;
  end;

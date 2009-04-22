unit tw13553;

interface

type
  TSymbol = (smNumber,smAdd,smSub,smMul,smDiv,smPow);

var
  Symbol: TSymbol;
  Number: Extended;

function NextToken: Boolean;

implementation

function NextToken: Boolean;
var
  c: Char;

  procedure GetChar; inline;
  begin
    Read(Input,c);
  end;

  procedure SkipWhite; inline;
  begin
    while c<=' ' do GetChar;
  end;

  procedure GetNumber; inline;

    function CharToNum(const c: Char): Byte; inline;
    begin
      CharToNum:=Ord(c)-Ord('0');
    end;

  var
    Divisor: LongWord;
  begin
    Number:=CharToNum(c);
    GetChar;
    while c in ['0'..'9'] do begin
      Number:=Number*10+CharToNum(c);
      GetChar;
    end;
    if c='.' then begin
      GetChar;
      Divisor:=10;
      while c in ['0'..'9'] do begin
        Number:=Number+CharToNum(c)/Divisor;
        Divisor:=Divisor*10;
        GetChar;
      end;
    end;
  end;

begin
  NextToken:=true;
  if not EOF then begin
    SkipWhite;
    case c of
      '0'..'9': begin
        Symbol:=smNumber;
        GetNumber;
      end;
      '+': begin
        Symbol:=smAdd;
      end;
      '-': begin
        Symbol:=smSub;
      end;
      '*': begin
        Symbol:=smMul;
      end;
      '/': begin
        Symbol:=smDiv;
      end;
      '^': begin
        Symbol:=smPow;
      end;
    end;
  end else
    NextToken:=false;
end;

end.

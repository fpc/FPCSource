{$mode objfpc}
{$h+}
unit tctparser;

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

  { TTestToString }
type
  TTestToString= class(TTestCase)
  private
    fStream : TMemoryStream;
    fPar : TParser;
  protected
    procedure SetUp; override; 
    procedure TearDown; override; 
  published
    procedure Test1;
    procedure Test2;
    procedure Test3;
    procedure Test4;
    procedure Test5;
    procedure Test6;
    procedure Test7;
    procedure Test8;
    procedure Test9;
    procedure Test10;
    procedure Test11;
    procedure Test12;
    procedure Test13;
    procedure Test14;
    procedure Test15;
    procedure Test16;
    procedure Test17;
  end;

  { TTestTokenInt }

  TTestTokenInt= class(TTestCase)
  private
    fStream : TMemoryStream;
    fPar : TParser;
  protected
    procedure SetUp; override; 
    procedure TearDown; override; 
  published
    procedure Test1;
    procedure Test2;
    procedure Test3;
  end;

  { TTestTokenFloat }

  TTestTokenFloat= class(TTestCase)
  private
    fStream : TMemoryStream;
    fPar : TParser;
  protected
    procedure SetUp; override; 
    procedure TearDown; override; 
  published
    procedure Test1;
    procedure Test2;
    procedure Test3;
    procedure Test4;
    procedure Test5;
    procedure Test6;
  end;

  { TTestSymbol }

  TTestSymbol= class(TTestCase)
  private
    fStream : TMemoryStream;
    fPar : TParser;
  protected
    procedure SetUp; override; 
    procedure TearDown; override;
  published
    procedure Test1;
    procedure Test2;
    procedure Test3;
  end; 

  { TTestPositions }

  TTestPositions= class(TTestCase)
  private
    fStream : TMemoryStream;
    fPar : TParser;
  protected
    procedure SetUp; override; 
    procedure TearDown; override; 
  published
    procedure Test1;
    procedure Test2;
  end; 

  { TTestBinary }

  TTestBinary= class(TTestCase)
  private
    fStream : TMemoryStream;
    fOutStr : TMemoryStream;
    fPar : TParser;
  protected
    procedure SetUp; override; 
    procedure TearDown; override; 
  published
    procedure Test1;
    procedure Test2;
  end; 

Implementation

{ ---------------------------------------------------------------------
    TTestToString
  ---------------------------------------------------------------------}
  

procedure TTestToString.Test1;
const
  aStr = '- 10';
begin
  fStream.WriteBuffer(aStr[1],length(aStr));
  fStream.Position:=0;
  fPar:=TParser.Create(fStream);
  try
    try
      fPar.CheckToken('-');
    except
      on e : EParserError do Fail('CheckToken failed');
    end;
    AssertEquals('-',fPar.TokenString);
    fPar.NextToken;
    try
      fPar.CheckToken(toInteger);
    except
      on e : EParserError do Fail('CheckToken failed');
    end;
    AssertEquals('10',fPar.TokenString);
  finally
    fPar.Free;
  end;
end;

procedure TTestToString.Test2;
const
  aStr = '-10';
begin
  fStream.WriteBuffer(aStr[1],length(aStr));
  fStream.Position:=0;
  fPar:=TParser.Create(fStream);
  try
    try
      fPar.CheckToken(toInteger);
    except
      on e : EParserError do Fail('CheckToken failed');
    end;
    AssertEquals('-10',fPar.TokenString);
  finally
    fPar.Free;
  end;
end;

procedure TTestToString.Test3;
const
  aStr = '$AFz';
begin
  fStream.WriteBuffer(aStr[1],length(aStr));
  fStream.Position:=0;
  fPar:=TParser.Create(fStream);
  try
    try
      fPar.CheckToken(toInteger);
    except
      on e : EParserError do Fail('CheckToken failed');
    end;
    AssertEquals('$AF',fPar.TokenString);
    fPar.NextToken;
    try
      fPar.CheckToken(toSymbol);
    except
      on e : EParserError do Fail('CheckToken failed');
    end;
    AssertEquals('z',fPar.TokenString);
  finally
    fPar.Free;
  end;
end;

procedure TTestToString.Test4;
const
  aStr : string = '$';
begin
  fStream.WriteBuffer(aStr[1],length(aStr));
  fStream.Position:=0;
  try
    fPar:=TParser.Create(fStream);
  except
    on e : EParserError do exit
    else
    begin
      fPar.Free;
      Fail('EParserError should be raised');
    end;
  end;
  fPar.Free;
  Fail('EParserError should be raised');
end;

procedure TTestToString.Test5;
const
  aStr = '1.';
begin
  fStream.WriteBuffer(aStr[1],length(aStr));
  fStream.Position:=0;
  fPar:=TParser.Create(fStream);
  try
    try
      fPar.CheckToken(toFloat);
    except
      on e : EParserError do Fail('CheckToken failed');
    end;
    AssertEquals('1.',fPar.TokenString);
  finally
    fPar.Free;
  end;
end;

procedure TTestToString.Test6;
const
  aStr = '1.0';
begin
  fStream.WriteBuffer(aStr[1],length(aStr));
  fStream.Position:=0;
  fPar:=TParser.Create(fStream);
  try
    try
      fPar.CheckToken(toFloat);
    except
      on e : EParserError do Fail('CheckToken failed');
    end;
    AssertEquals('1.0',fPar.TokenString);
  finally
    fPar.Free;
  end;
end;

procedure TTestToString.Test7;
const
  aStr = '1E';
begin
  fStream.WriteBuffer(aStr[1],length(aStr));
  fStream.Position:=0;
  try
    fPar:=TParser.Create(fStream);
  except
    on e : EParserError do exit
    else
    begin
      fPar.Free;
      Fail('EParserError should be raised');
    end;
  end;
  fPar.Free;
  Fail('EParserError should be raised');
end;

procedure TTestToString.Test8;
const
  aStr = '1E+';
begin
  fStream.WriteBuffer(aStr[1],length(aStr));
  fStream.Position:=0;
  try
    fPar:=TParser.Create(fStream);
  except
    on e : EParserError do exit
    else
    begin
      fPar.Free;
      Fail('EParserError should be raised');
    end;
  end;
  fPar.Free;
  Fail('EParserError should be raised');
end;

procedure TTestToString.Test9;
const
  aStr = '1.E+2';
begin
  fStream.WriteBuffer(aStr[1],length(aStr));
  fStream.Position:=0;
  fPar:=TParser.Create(fStream);
  try
    try
      fPar.CheckToken(toFloat);
    except
      on e : EParserError do Fail('CheckToken failed');
    end;
    AssertEquals('1.E+2',fPar.TokenString);
  finally
    fPar.Free;
  end;
end;

procedure TTestToString.Test10;
const
  aStr = '1.E+2a';
begin
  fStream.WriteBuffer(aStr[1],length(aStr));
  fStream.Position:=0;
  fPar:=TParser.Create(fStream);
  try
    try
      fPar.CheckToken(toFloat);
    except
      on e : EParserError do Fail('CheckToken failed');
    end;
    AssertEquals('1.E+2',fPar.TokenString);
    fPar.NextToken;
    try
      fPar.CheckToken(toSymbol);
    except
      on e : EParserError do Fail('CheckToken failed');
    end;
    AssertEquals('a',fPar.TokenString);
  finally
    fPar.Free;
  end;
end;

procedure TTestToString.Test11;
const
  aStr = '12s';
begin
  fStream.WriteBuffer(aStr[1],length(aStr));
  fStream.Position:=0;
  fPar:=TParser.Create(fStream);
  try
    try
      fPar.CheckToken(toFloat);
    except
      on e : EParserError do Fail('CheckToken failed');
    end;
    AssertEquals('12s',fPar.TokenString);
  finally
    fPar.Free;
  end;
end;

procedure TTestToString.Test12;
const
  aStr = '''string'''; //'string'
begin
  fStream.WriteBuffer(aStr[1],length(aStr));
  fStream.Position:=0;
  fPar:=TParser.Create(fStream);
  try
    try
      fPar.CheckToken(Classes.toString);
    except
      on e : EParserError do Fail('CheckToken failed');
    end;
    AssertEquals('string',fPar.TokenString);
  finally
    fPar.Free;
  end;
end;

procedure TTestToString.Test13;
const
  aStr = '''can''''t'''; //'can''t'
begin
  fStream.WriteBuffer(aStr[1],length(aStr));
  fStream.Position:=0;
  fPar:=TParser.Create(fStream);
  try
    try
      fPar.CheckToken(Classes.toString);
    except
      on e : EParserError do Fail('CheckToken failed');
    end;
    AssertEquals('can''t',fPar.TokenString);
  finally
    fPar.Free;
  end;
end;

procedure TTestToString.Test14;
const
  aStr = '''c''#97#110''''''''#116#32''open file''';  //'c'#97#110''''#116#32'open file'
begin
  fStream.WriteBuffer(aStr[1],length(aStr));
  fStream.Position:=0;
  fPar:=TParser.Create(fStream);
  try
    try
      fPar.CheckToken(Classes.toString);
    except
      on e : EParserError do Fail('CheckToken failed');
    end;
    AssertEquals('can''t open file',fPar.TokenString);
  finally
    fPar.Free;
  end;
end;

procedure TTestToString.Test15;
const
  aStr = '''perch''#232';
var ws : widestring;
begin
  fStream.WriteBuffer(aStr[1],length(aStr));
  fStream.Position:=0;
  ws:='perch'#232;
  fPar:=TParser.Create(fStream);
  try
    try
      fPar.CheckToken(toWString);
    except
      on e : EParserError do Fail('CheckToken failed');
    end;
    AssertEquals(ws,fPar.TokenWideString);
  finally
    fPar.Free;
  end;
end;

procedure TTestToString.Test16;
const
  aStr = '''unterminated string'#10'blah''';
begin
  fStream.WriteBuffer(aStr[1],length(aStr));
  fStream.Position:=0;
  try
    fPar:=TParser.Create(fStream);
  except
    on e : EParserError do exit
    else
    begin
      fPar.Free;
      Fail('EParserError should be raised');
    end;
  end;
  fPar.Free;
  Fail('EParserError should be raised');
end;

procedure TTestToString.Test17;
const
  aStr = 'first.second.third';
begin
  fStream.WriteBuffer(aStr[1],length(aStr));
  fStream.Position:=0;
  fPar:=TParser.Create(fStream);
  try
    try
      fPar.CheckToken(toSymbol);
    except
      on e : EParserError do Fail('CheckToken failed');
    end;
    AssertEquals('first',fPar.TokenString);
    fPar.NextToken;
    try
      fPar.CheckToken('.');
    except
      on e : EParserError do Fail('CheckToken failed');
    end;
    AssertEquals('.',fPar.TokenString);
    fPar.NextToken;
    try
      fPar.CheckToken(toSymbol);
    except
      on e : EParserError do Fail('CheckToken failed');
    end;
    AssertEquals('second',fPar.TokenString);
    fPar.NextToken;
    try
      fPar.CheckToken('.');
    except
      on e : EParserError do Fail('CheckToken failed');
    end;
    AssertEquals('.',fPar.TokenString);
    fPar.NextToken;
    try
      fPar.CheckToken(toSymbol);
    except
      on e : EParserError do Fail('CheckToken failed');
    end;
    AssertEquals('third',fPar.TokenString);
  finally
    fPar.Free;
  end;
end;

procedure TTestToString.SetUp;
begin
  fStream:=TMemoryStream.Create;
end; 

procedure TTestToString.TearDown;
begin
  fStream.Free;
end;

{ ---------------------------------------------------------------------
    TTestTokenInt
  ---------------------------------------------------------------------}
  

procedure TTestTokenInt.Test1;
const
  aStr = '10';
begin
  fStream.WriteBuffer(aStr[1],length(aStr));
  fStream.Position:=0;
  fPar:=TParser.Create(fStream);
  try
    AssertEquals(toInteger,fPar.Token);
    AssertEquals(10,fPar.TokenInt);
  finally
    fPar.Free;
  end;
end;

procedure TTestTokenInt.Test2;
const
  aStr = '-10';
begin
  fStream.WriteBuffer(aStr[1],length(aStr));
  fStream.Position:=0;
  fPar:=TParser.Create(fStream);
  try
    AssertEquals(toInteger,fPar.Token);
    AssertEquals(-10,fPar.TokenInt);
  finally
    fPar.Free;
  end;
end;

procedure TTestTokenInt.Test3;
const
  aStr = '$AF';
begin
  fStream.WriteBuffer(aStr[1],length(aStr));
  fStream.Position:=0;
  fPar:=TParser.Create(fStream);
  try
    AssertEquals(toInteger,fPar.Token);
    AssertEquals($AF,fPar.TokenInt);
  finally
    fPar.Free;
  end;
end;

procedure TTestTokenInt.SetUp;
begin
  fStream:=TMemoryStream.Create;
end;

procedure TTestTokenInt.TearDown;
begin
  fStream.Free;
end;

{ ---------------------------------------------------------------------
    TTestTokenFloat
  ---------------------------------------------------------------------}
  
procedure TTestTokenFloat.Test1;
const
  aStr = '1.';
begin
  fStream.WriteBuffer(aStr[1],length(aStr));
  fStream.Position:=0;
  fPar:=TParser.Create(fStream);
  try
    AssertEquals(toFloat,fPar.Token);
    AssertEquals(1.0,fPar.TokenFloat);
    AssertEquals(#0,fPar.FloatType);
  finally
    fPar.Free;
  end;
end;

procedure TTestTokenFloat.Test2;
const
  aStr = '1.0';
begin
  fStream.WriteBuffer(aStr[1],length(aStr));
  fStream.Position:=0;
  fPar:=TParser.Create(fStream);
  try
    AssertEquals(toFloat,fPar.Token);
    AssertEquals(1.0,fPar.TokenFloat);
    AssertEquals(#0,fPar.FloatType);
  finally
    fPar.Free;
  end;
end;

procedure TTestTokenFloat.Test3;
const
  aStr = '1.E+2';
begin
  fStream.WriteBuffer(aStr[1],length(aStr));
  fStream.Position:=0;
  fPar:=TParser.Create(fStream);
  try
    AssertEquals(toFloat,fPar.Token);
    AssertEquals(100.0,fPar.TokenFloat);
    AssertEquals(#0,fPar.FloatType);
  finally
    fPar.Free;
  end;
end;

procedure TTestTokenFloat.Test4;
const
  aStr = '12s';
begin
  fStream.WriteBuffer(aStr[1],length(aStr));
  fStream.Position:=0;
  fPar:=TParser.Create(fStream);
  try
    AssertEquals(toFloat,fPar.Token);
    AssertEquals(12.0,fPar.TokenFloat);
    AssertEquals('s',fPar.FloatType);
  finally
    fPar.Free;
  end;
end;

procedure TTestTokenFloat.Test5;
const
  aStr = '12d';
begin
  fStream.WriteBuffer(aStr[1],length(aStr));
  fStream.Position:=0;
  fPar:=TParser.Create(fStream);
  try
    AssertEquals(toFloat,fPar.Token);
    AssertEquals(12.0,fPar.TokenFloat);
    AssertEquals('d',fPar.FloatType);
  finally
    fPar.Free;
  end;
end;

procedure TTestTokenFloat.Test6;
const
  aStr = '12c';
begin
  fStream.WriteBuffer(aStr[1],length(aStr));
  fStream.Position:=0;
  fPar:=TParser.Create(fStream);
  try
    AssertEquals(toFloat,fPar.Token);
    AssertEquals(12.0,fPar.TokenFloat);
    AssertEquals('c',fPar.FloatType);
  finally
    fPar.Free;
  end;
end;

procedure TTestTokenFloat.SetUp; 
begin
  fStream:=TMemoryStream.Create;
end;

procedure TTestTokenFloat.TearDown; 
begin
  fStream.Free;
end;

{ ---------------------------------------------------------------------
    TTestSymbol
  ---------------------------------------------------------------------}
  

procedure TTestSymbol.Test1;
const
  aStr = 'hello world';
begin
  fStream.WriteBuffer(aStr[1],length(aStr));
  fStream.Position:=0;
  fPar:=TParser.Create(fStream);
  try
    AssertTrue(fPar.TokenSymbolIs('HELLO'));
    try
      fPar.CheckTokenSymbol('HeLlO');
    except
      on e : EParserError do Fail('CheckTokenSymbol failed');
    end;
    AssertEquals('hello',fPar.TokenComponentIdent);
    fPar.NextToken;
    AssertTrue(fPar.TokenSymbolIs('world'));
    try
      fPar.CheckTokenSymbol('wOrLd');
    except
      on e : EParserError do Fail('CheckTokenSymbol failed');
    end;
    AssertEquals('world',fPar.TokenComponentIdent);
  finally
    fPar.Free;
  end;
end;

procedure TTestSymbol.Test2;
const
  aStr = 'first.second.third';
begin
  fStream.WriteBuffer(aStr[1],length(aStr));
  fStream.Position:=0;
  fPar:=TParser.Create(fStream);
  try
    AssertTrue(fPar.TokenSymbolIs('first'));
    try
      fPar.CheckTokenSymbol('first');
    except
      on e : EParserError do Fail('CheckTokenSymbol failed');
    end;
    AssertEquals('first',fPar.TokenString);
    AssertEquals('first.second.third',fPar.TokenComponentIdent);
    AssertEquals('first.second.third',fPar.TokenString);
  finally
    fPar.Free;
  end;
end;

procedure TTestSymbol.Test3;
const
  aStr = 'first.';
begin
  fStream.WriteBuffer(aStr[1],length(aStr));
  fStream.Position:=0;
  fPar:=TParser.Create(fStream);
  try
    AssertTrue(fPar.TokenSymbolIs('first'));
    try
      fPar.CheckTokenSymbol('first');
    except
      on e : EParserError do Fail('CheckTokenSymbol failed');
    end;
    AssertEquals('first',fPar.TokenString);
    try
      fPar.TokenComponentIdent;
    except
      on e : EParserError do exit
      else
        Fail('EParserError should be raised');
    end;
    Fail('EParserError should be raised');
  finally
    fPar.Free;
  end;
end;

procedure TTestSymbol.SetUp; 
begin
  fStream:=TMemoryStream.Create;
end;

procedure TTestSymbol.TearDown;
begin
  fStream.Free;
end;

{ ---------------------------------------------------------------------
    TTestPositions
  ---------------------------------------------------------------------}
  

procedure TTestPositions.Test1;
const
  aStr = 'this is'#10'a '#13'test.'#13#10'Another line';
begin
  fStream.WriteBuffer(aStr[1],length(aStr));
  fStream.Position:=0;
  fPar:=TParser.Create(fStream);
  try
    //this
    AssertEquals(1,fPar.SourceLine);
    AssertEquals(4,fPar.SourcePos);
    //is
    fPar.NextToken;
    AssertEquals(1,fPar.SourceLine);
    AssertEquals(7,fPar.SourcePos);
    //a
    fPar.NextToken;
    AssertEquals(2,fPar.SourceLine);
    AssertEquals(9,fPar.SourcePos);
    //test
    fPar.NextToken;
    AssertEquals(3,fPar.SourceLine);
    AssertEquals(15,fPar.SourcePos);
    //.
    fPar.NextToken;
    AssertEquals(3,fPar.SourceLine);
    AssertEquals(16,fPar.SourcePos);
    //Another
    fPar.NextToken;
    AssertEquals(4,fPar.SourceLine);
    AssertEquals(25,fPar.SourcePos);
    //line
    fPar.NextToken;
    AssertEquals(4,fPar.SourceLine);
    AssertEquals(30,fPar.SourcePos);
    //eof
    fPar.NextToken;
    AssertEquals(4,fPar.SourceLine);
    AssertEquals(30,fPar.SourcePos);
    AssertEquals(toEOF,fPar.Token);
  finally
    fPar.Free;
  end;
end;

procedure TTestPositions.Test2;
const
  aStr = 'this is a test';
begin
  fStream.WriteBuffer(aStr[1],length(aStr));
  fStream.Position:=0;
  fPar:=TParser.Create(fStream);
  try
    //this
    fPar.NextToken;
    //is
  finally
    fPar.Free;
  end;
  AssertEquals(7,fStream.Position);
end;

procedure TTestPositions.SetUp; 
begin
  fStream:=TMemoryStream.Create;
end;

procedure TTestPositions.TearDown; 
begin
  fStream.Free;
end;

{ ---------------------------------------------------------------------
    TTestBinary
  ---------------------------------------------------------------------}
  
procedure TTestBinary.Test1;
const
  aStr = '{ 1234 56'+#13#10'789A somethingelse';
var buf : array[0..4] of byte = ($12,$34,$56,$78,$9A);
begin
  fStream.WriteBuffer(aStr[1],length(aStr));
  fStream.Position:=0;
  fOutStr.Position:=0;
  fPar:=TParser.Create(fStream);
  try
    fPar.HexToBinary(fOutStr);
    AssertEquals(5,fOutStr.Size);
    AssertTrue(CompareMem(@buf[0],fOutStr.Memory,5));
    AssertEquals(16,fPar.SourcePos);
  finally
    fPar.Free;
  end;
end;

procedure TTestBinary.Test2;
const
  aStr = '{ 123z';
begin
  fStream.WriteBuffer(aStr[1],length(aStr));
  fStream.Position:=0;
  fOutStr.Position:=0;
  fPar:=TParser.Create(fStream);
  try
    try
      fPar.HexToBinary(fOutStr);
    except
      on e : EParserError do exit
      else Fail('EParserError should be raised');
    end;
    Fail('EParserError should be raised');
  finally
    fPar.Free;
  end;
end;

procedure TTestBinary.SetUp; 
begin
  fStream:=TMemoryStream.Create;
  fOutStr:=TMemoryStream.Create;
end;

procedure TTestBinary.TearDown; 
begin
  fStream.Free;
  fOutStr.Free;
end; 

initialization

  RegisterTests([TTestToString,TTestTokenInt,TTestTokenFloat,TTestSymbol,TTestBinary]);

end.

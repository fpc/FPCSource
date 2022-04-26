{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2022 by Michael van Canneyt and other members of the
    Free Pascal development team

    test report dom

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit tcreportdom;

{$mode objfpc}{$H+}
{.$define writexml}

interface

uses
  Classes, SysUtils,
  {$IFDEF fptest}
  TestFramework,
  {$ELSE}
  fpcunit, testutils, testregistry,
  {$ENDIF}
  DOM, fpcanvas,
  fpreport, fpreportdom, xmlwrite;

type

  TReportDOMTester = class(TTestCase)
  Private
    FDoc : TXMLDocument;
    FRoot : TDOMElement;
    procedure AssertAttribute(const AName, AValue: DomString);
    procedure AssertValueElement(const AName, AValue: DomString);
    procedure FillBytes(S: TStream; AMax: Byte);
    Procedure AssertNoAttribute(Const AName : DomString);
  protected
    FRD : TFPReportDOM;
    procedure SetUp; override;
    procedure TearDown; override;
  end;


  TTestReportDOM = class(TReportDOMTester)
  Public
    procedure DoPop;
    procedure TestStream(DoReset: Boolean);
  published
    procedure TestCreate;
    Procedure TestAdd;
    Procedure TestPush;
    Procedure TestFind1;
    Procedure TestFind2;
    procedure TestPop1;
    procedure TestPop2;
    procedure TestPop3;
    Procedure TestStreamToHex;
    Procedure TestStreamToHex2;
    procedure TestStreamEquals1;
    procedure TestStreamEquals2;
    procedure TestStreamEquals3;
    procedure TestStreamEquals4;
    procedure TestHexToStream;
    Procedure TestWriteInteger1;
    procedure TestWriteInteger2;
    Procedure TestWriteString1;
    procedure TestWriteString2;
    Procedure TestWriteString3;
    procedure TestWriteString4;
    Procedure TestWriteWideString1;
    procedure TestWriteWideString2;
    Procedure TestWriteWideString3;
    procedure TestWriteWideString4;
    Procedure TestWriteBoolean1;
    procedure TestWriteBoolean2;
    Procedure TestWriteBoolean3;
    procedure TestWriteBoolean4;
    Procedure TestWriteFloat1;
    procedure TestWriteFloat2;
    Procedure TestWriteFloat3;
    procedure TestWriteFloat4;
    procedure TestWriteFloat5;
    procedure TestWriteFloat6;
    Procedure TestWriteDateTime1;
    Procedure TestWriteDateTime2;
    procedure TestWriteDateTime3;
    procedure TestWriteDateTime4;
    procedure TestWriteDateTime5;
    procedure TestWriteDateTime6;
    Procedure TestWriteStream1;
    procedure TestWriteStream2;
    Procedure TestWriteIntegerDiff1;
    procedure TestWriteIntegerDiff2;
    Procedure TestWriteStringDiff1;
    procedure TestWriteStringDiff2;
    Procedure TestWriteWideStringDiff1;
    procedure TestWriteWideStringDiff2;
    Procedure TestWriteBooleanDiff1;
    procedure TestWriteBooleanDiff2;
    Procedure TestWriteFloatDiff1;
    procedure TestWriteFloatDiff2;
    Procedure TestWriteDateTimeDiff1;
    Procedure TestWriteDateTimeDiff2;
    procedure TestWriteDateTimeDiff3;
    procedure TestWriteDateTimeDiff4;
    Procedure TestWriteStreamDiff1;
    Procedure TestWriteStreamDiff2;
    Procedure TestWriteStreamDiff3;
    Procedure TestReadInteger1;
    procedure TestReadInteger2;
    procedure TestReadInteger3;
    procedure TestReadInteger4;
    procedure TestReadInteger5;
    procedure TestReadInteger6;
    Procedure TestReadString1;
    procedure TestReadString2;
    Procedure TestReadString3;
    procedure TestReadString4;
    Procedure TestReadWideString1;
    procedure TestReadWideString2;
    Procedure TestReadWideString3;
    procedure TestReadWideString4;
    Procedure TestReadDateTime1;
    Procedure TestReadDateTime2;
    procedure TestReadDateTime3;
    procedure TestReadDateTime4;
    procedure TestReadDateTime5;
    procedure TestReadDateTime6;
    procedure TestReadDateTime7;
    procedure TestReadDateTime8;
    procedure TestReadDateTime9;
    procedure TestReadDateTime10;
    Procedure TestReadBoolean1;
    procedure TestReadBoolean2;
    Procedure TestReadBoolean3;
    procedure TestReadBoolean4;
    procedure TestReadBoolean5;
    procedure TestReadBoolean6;
    procedure TestReadBoolean7;
    procedure TestReadBoolean8;
    Procedure TestReadFloat1;
    procedure TestReadFloat2;
    Procedure TestReadFloat3;
    procedure TestReadFloat4;
    procedure TestReadFloat5;
    procedure TestReadFloat6;
    procedure TestReadFloat7;
    procedure TestReadStream1;
    procedure TestReadStream2;
    procedure TestReadStream3;
    procedure TestReadStream4;
    procedure TestReadStream5;
    procedure TestReadStream6;
  end;


  TTestReportFrameDom = Class(TReportDOMTester)
  private
    FF,F2 : TFPReportFrame;
    procedure FillFF;
  protected
    procedure Setup; override;
    procedure TearDown; override;
  published
    procedure TestWrite;
    procedure TestWriteDiff;
    procedure TestRead;
  end;


  TTestReportLayoutDom = Class(TReportDOMTester)
  private
    FL,F2 : TFPReportLayout;
    procedure FillFL;
  protected
    procedure Setup; override;
    procedure TearDown; override;
  published
    procedure TestWrite;
    procedure TestWriteDiff;
    procedure TestRead;
  end;


  TTestReportElementDOM =  Class(TReportDOMTester)
  private
    FE,F2 : TFPReportElement;
    procedure FillFE;
  protected
    procedure Setup; override;
    procedure TearDown; override;
  published
    procedure TestWrite1;
    procedure TestWriteDiff1;
    procedure TestWriteDiff2;
    procedure TestWriteDiff3;
    procedure TestRead1;
    procedure TestRead2;
  end;


implementation

{ ---------------------------------------------------------------------
  General routines
  ---------------------------------------------------------------------}

procedure TReportDOMTester.SetUp;
begin
  FDoc:=TXMLDocument.Create;
  FRoot:=FDoc.CreateElement('XMLReport');
  FDoc.AppendChild(FRoot);
  FRD:=TFPReportDOM.Create(FDoc,Nil);
end;

{$ifdef writexml}
Var
  WC : Integer;
{$endif}

procedure TReportDOMTester.TearDown;

begin
{$ifdef writexml}
  Inc(wc);
  WriteXML(FDoc,'xml-'+inttostr(wc)+'.xml');
{$endif}
  FreeAndNil(FRD);
  FreeAndNil(FDoc);
end;

procedure TReportDOMTester.FillBytes(S : TStream; AMax : Byte);

Var
  B : Byte;

begin
  For B:=0 to AMax do
    S.WriteBuffer(B,SizeOf(B));
end;

procedure TReportDOMTester.AssertAttribute(Const AName, AValue : DomString);

Var
  S: String;

begin
  S:='Attribute '+AName+' exists';
  AssertEquals(S,True,FRD.CurrentElement.HasAttribute(AName));
  S:='Attribute '+AName+' has correct value';
  AssertEquals(S,AValue,FRD.CurrentElement[AName]);
end;

procedure TReportDOMTester.AssertValueElement(Const AName, AValue : DomString);

Var
  S: String;
  N : TDomNode;

begin
  S:='Element with name '+AName+' exists';
  N:=FRD.CurrentElement.FindNode(AName);
  AssertNotNull(S,N);
  S:='Element with name '+AName+' is TDOMElement';
  AssertEquals(S,TDomElement,N.ClassType);
  N:=N.FirstChild;
  S:='Value element under element with name '+AName+' exists';
  AssertNotNull(S,N);
  AssertEquals('Value node is of type text',TEXT_NODE,N.NodeType);
  AssertEquals('Value node has correct content',AValue,N.NodeValue);
end;

procedure TReportDOMTester.AssertNoAttribute(const AName: DomString);

Var
  S : String;

begin
  S:='No attribute of name '+AName;
  AssertEquals(S,False, FRD.CurrentElement.hasAttribute(AName));
end;

{ ---------------------------------------------------------------------
  Actual test routines
  ---------------------------------------------------------------------}


procedure TTestReportDOM.TestCreate;
begin
  AssertSame('XML Document property is set',FDoc,FRD.Document);
  AssertSame('Root element is set ',FRoot,FRD.RootNode);
  AssertSame('Current element is set ',FRoot,FRD.CurrentElement);
end;

procedure TTestReportDOM.TestAdd;

Var
  E1,E2 : TDomElement;

begin
  E1:=FRD.CurrentElement;
  E2:=FRD.NewDOMElement('MyElement');
  AssertNotNull('NewDOMElement returns result',E2);
  AssertSame('NewDomElement is child of current element',E2,E1.FindNode('MyElement'));
  AssertEquals('New node element created with correct name','MyElement',E2.NodeName);
  AssertSame('New node element is current element',E2,FRD.CurrentElement);
end;

procedure TTestReportDOM.TestPush;

Var
  E1,E2 : TDomElement;

begin
  E1:=FRD.NewDOMElement('element1');
  AssertSame('Current element equals created',E1,FRD.CurrentElement);
  E2:=FRD.PushElement('element2');
  AssertSame('Current element equals first created',E1,E2);
  AssertEquals('New node pushed with correct name','element2',FRD.CurrentElement.NodeName);
end;

procedure TTestReportDOM.TestFind1;
Var
  E1,E2,E3 : TDomElement;

begin
  E1:=FRD.NewDOMElement('element1');
  E2:=FRD.NewDomElement('element2');
  FRD.CurrentElement:=E1;
  E3:=FRD.FindChild('element2');
  AssertSame('Found element',E2,E3);
end;

procedure TTestReportDOM.TestFind2;

Var
  E1,E2,E3 : TDomElement;

begin
  E1:=FRD.NewDOMElement('element1');
  E2:=FRD.NewDomElement('element2');
  FRD.CurrentElement:=E1;
  E3:=FRD.FindChild('element3');
  AssertNull('NonExisting element is null',E3);
end;


procedure TTestReportDOM.TestPop1;


Var
  E1,E2,E3 : TDomElement;

begin
  E1:=FRD.NewDOMElement('element1');
  AssertSame('Current element equals created',E1,FRD.CurrentElement);
  E2:=FRD.PushElement('element2');
  AssertSame('Current element equals first created',E1,E2);
  E3:=FRD.CurrentElement;
  AssertEquals('New node pushed with correct name','element2',E3.NodeName);
  AssertSame('Pop returns current element',E3,FRD.PopElement);
  AssertSame('Current element after pop is correct',E1,FRD.CurrentElement);
end;

procedure TTestReportDOM.DoPop;

begin
  FRD.PopElement;
end;

procedure TTestReportDOM.TestPop2;

begin
  AssertException('Pop on empty stack raise exception',EReportDom,@DoPop);
end;

procedure TTestReportDOM.TestPop3;

begin
  FRD.NewDOMElement('element1');
  FRD.PushElement('element2');
  FRD.PopElement;
  AssertException('Pop on empty stack raise exception',EReportDom,@DoPop);
end;


procedure TTestReportDOM.TestStream(DoReset : Boolean);

Var
  S : TMemoryStream;
  B : Byte;
  T,H : String;

begin
  S:=TMemoryStream.Create;
  try
    FillBytes(S,255);
    S.Position:=0;
    T:=FRD.StreamToHex(S);
    AssertEquals('Stream position is zero',0,S.Position);
    AssertEquals('Correct number of bytes returned by streamtohex',512,Length(T));
    For B:=0 to 255 do
      begin
      H:=Copy(T,1,2);
      Delete(T,1,2);
      AssertEquals(Format('Correct value at position %d',[b]),H,HexStr(B,2));
      end;
  Finally
    S.Free;
  end;
end;



procedure TTestReportDOM.TestStreamToHex;

begin
  TestStream(True);
end;

procedure TTestReportDOM.TestStreamToHex2;

begin
  TestStream(False);
end;


procedure TTestReportDOM.TestHexToStream;


Var
  S : TMemoryStream;
  SS : TStringStream;
  H : String;

begin
  S:=TMemoryStream.Create;
  try
    FillBytes(S,255);
    H:=FRD.StreamToHex(S);
    SS:=FRD.HexToStringStream(H);
    try
      AssertEquals('Size of stream is OK',256,SS.Size);
      AssertEquals('HexToStringStream OK',True,FRD.StreamsEqual(S,SS));
    finally
      SS.Free;
    end;
  finally
    S.Free;
  end;
end;


procedure TTestReportDOM.TestStreamEquals1;

Var
  S : TMemoryStream;

begin
  S:=TMemoryStream.Create;
  try
    AssertEquals('Same stream always equal',True,FRD.StreamsEqual(S,S));
  finally
    S.Free;
  end;
end;


procedure TTestReportDOM.TestStreamEquals2;

Var
  S1,S2 : TMemoryStream;

begin
  S1:=TMemoryStream.Create;
  try
    FillBytes(S1,255);
    S2:=TMemoryStream.Create;
    try
      FillBytes(S2,255);
      AssertEquals('Same content always equal',True,FRD.StreamsEqual(S1,S2));
    finally
      S2.Free;
    end;
  finally
    S1.Free;
  end;
end;

procedure TTestReportDOM.TestStreamEquals3;

Var
  S1,S2 : TMemoryStream;

begin
  S1:=TMemoryStream.Create;
  try
    FillBytes(S1,255);
    S2:=TMemoryStream.Create;
    try
      FillBytes(S2,254);
      AssertEquals('Different sizes makes not equal',False,FRD.StreamsEqual(S1,S2));
    finally
      S2.Free;
    end;
  finally
    S1.Free;
  end;
end;

procedure TTestReportDOM.TestStreamEquals4;

Var
  S1,S2 : TMemoryStream;
  B : Byte;

begin
  S1:=TMemoryStream.Create;
  try
    FillBytes(S1,255);
    AssertEquals(0,S1.Seek(0,soFromBeginning));
    B:=10;
    S1.WriteBuffer(B,1);
    B:=12;
    S1.Position:=0;
    S1.ReadBuffer(B,1);
    AssertEquals(10,B);
    AssertEquals(256,S1.Size);
    S2:=TMemoryStream.Create;
    try
      FillBytes(S2,255);
      AssertEquals('Different streams makes not equal',False,FRD.StreamsEqual(S1,S2));
    finally
      S2.Free;
    end;
  finally
    S1.Free;
  end;
end;



procedure TTestReportDOM.TestWriteInteger1;

Var
  E : TDOMElement;

begin
  E:=FRD.NewDOMElement('MyElement');
  FRD.WriteInteger('Int',1,psAttr);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  AssertAttribute('Int','1');
end;

procedure TTestReportDOM.TestWriteInteger2;

Var
  E : TDOMElement;

begin
  E:=FRD.NewDOMElement('MyElement');
  FRD.WriteInteger('Int',1,psElement);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  AssertValueElement('Int','1');
end;

procedure TTestReportDOM.TestWriteString1;

Var
  E : TDOMElement;

begin
  E:=FRD.NewDOMElement('MyElement');
  FRD.WriteString('Str','Aloha',psAttr);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  AssertAttribute('Str','Aloha');
end;

procedure TTestReportDOM.TestWriteString2;

Var
  E : TDOMElement;

begin
  E:=FRD.NewDOMElement('MyElement');
  FRD.WriteString('Str','Aloha',psElement);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  AssertValueElement('Str','Aloha');
end;

procedure TTestReportDOM.TestWriteString3;

Var
  E : TDOMElement;

begin
  E:=FRD.NewDOMElement('MyElement');
  FRD.WriteString('Str','',psAttr);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  AssertAttribute('Str','');
end;

procedure TTestReportDOM.TestWriteString4;

Var
  E : TDOMElement;

begin
  E:=FRD.NewDOMElement('MyElement');
  FRD.WriteString('Str','',psElement);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  AssertValueElement('Str','');

end;

procedure TTestReportDOM.TestWriteWideString1;

Var
  E : TDOMElement;
  W : WideString;

begin
  E:=FRD.NewDOMElement('MyElement');
  W:='Wide Aloha';
  FRD.WriteWideString('WideStr',W,psAttr);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  AssertAttribute('WideStr',W);
end;

procedure TTestReportDOM.TestWriteWideString2;

Var
  E : TDOMElement;
  W : WideString;

begin
  E:=FRD.NewDOMElement('MyElement');
  W:='Wide Aloha';
  FRD.WriteWideString('WideStr',W,psElement);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  AssertValueElement('WideStr',W);
end;

procedure TTestReportDOM.TestWriteWideString3;

Var
  E : TDOMElement;
  W : WideString;

begin
  E:=FRD.NewDOMElement('MyElement');
  W:='';
  FRD.WriteWideString('WideStr',W,psAttr);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  AssertAttribute('WideStr','');
end;

procedure TTestReportDOM.TestWriteWideString4;

Var
  E : TDOMElement;
  W : WideString;

begin
  E:=FRD.NewDOMElement('MyElement');
  W:='';
  FRD.WriteWideString('WideStr',W,psElement);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  AssertValueElement('WideStr',W);
end;


procedure TTestReportDOM.TestWriteBoolean1;

Var
  E : TDOMElement;

begin
  E:=FRD.NewDOMElement('MyElement');
  FRD.WriteBoolean('Bool',True,psAttr);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  AssertAttribute('Bool','1');
end;

procedure TTestReportDOM.TestWriteBoolean2;

Var
  E : TDOMElement;

begin
  E:=FRD.NewDOMElement('MyElement');
  FRD.WriteBoolean('Bool',False,psAttr);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  AssertAttribute('Bool','0');
end;

procedure TTestReportDOM.TestWriteBoolean3;

Var
  E : TDOMElement;

begin
  E:=FRD.NewDOMElement('MyElement');
  FRD.WriteBoolean('Bool',True,psElement);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  AssertValueElement('Bool','1');
end;

procedure TTestReportDOM.TestWriteBoolean4;

Var
  E : TDOMElement;

begin
  E:=FRD.NewDOMElement('MyElement');
  FRD.WriteBoolean('Bool',False,psElement);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  AssertValueElement('Bool','0');
end;

procedure TTestReportDOM.TestWriteFloat1;

Var
  E : TDOMElement;

begin
  E:=FRD.NewDOMElement('MyElement');
  FRD.WriteFloat('Float',1.23,psAttr);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  AssertAttribute('Float','1.230000000E+0000');
end;

procedure TTestReportDOM.TestWriteFloat2;

Var
  E : TDOMElement;

begin
  E:=FRD.NewDOMElement('MyElement');
  FRD.WriteFloat('Float',1.23,psElement);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  AssertValueElement('Float','1.230000000E+0000');
end;

procedure TTestReportDOM.TestWriteFloat3;

Var
  E : TDOMElement;

begin
  E:=FRD.NewDOMElement('MyElement');
  FRD.WriteFloat('Float',-1.23,psAttr);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  AssertAttribute('Float','-1.230000000E+0000');
end;

procedure TTestReportDOM.TestWriteFloat4;

Var
  E : TDOMElement;

begin
  E:=FRD.NewDOMElement('MyElement');
  FRD.WriteFloat('Float',-1.23,psElement);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  AssertValueElement('Float','-1.230000000E+0000');
end;

procedure TTestReportDOM.TestWriteFloat5;

Var
  E : TDOMElement;
  x : extended;

begin
  E:=FRD.NewDOMElement('MyElement');
  X:=0.000000000;
  FRD.WriteFloat('Float',x,psAttr);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  AssertAttribute('Float','0.000000000E+0000');
end;

procedure TTestReportDOM.TestWriteFloat6;

Var
  E : TDOMElement;
  x : extended;

begin
  E:=FRD.NewDOMElement('MyElement');
  X:=0.000000000;
  FRD.WriteFloat('Float',x,psElement);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  AssertValueElement('Float','0.000000000E+0000');
end;

procedure TTestReportDOM.TestWriteDateTime1;

Var
  E : TDOMElement;
  D : TDateTime;

begin
  E:=FRD.NewDOMElement('MyElement');
  D:=EncodeDate(2008,9,18);
  FRD.WriteDateTime('Date',D,psAttr);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  AssertAttribute('Date','20080918');
end;

procedure TTestReportDOM.TestWriteDateTime2;

Var
  E : TDOMElement;
  D : TDateTime;

begin
  E:=FRD.NewDOMElement('MyElement');
  D:=EncodeDate(2008,9,18);
  FRD.WriteDateTime('Date',D,psElement);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  AssertValueElement('Date','20080918');
end;

procedure TTestReportDOM.TestWriteDateTime3;

Var
  E : TDOMElement;
  D : TDateTime;

begin
  E:=FRD.NewDOMElement('MyElement');
  D:=EncodeDate(2008,9,18)+EncodeTime(11,03,55,123);
  FRD.WriteDateTime('Date',D,psAttr);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  AssertAttribute('Date','20080918110355123');
end;

procedure TTestReportDOM.TestWriteDateTime4;

Var
  E : TDOMElement;
  D : TDateTime;

begin
  E:=FRD.NewDOMElement('MyElement');
  D:=EncodeDate(2008,9,18)+EncodeTime(11,03,55,123);
  FRD.WriteDateTime('Date',D,psElement);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  AssertValueElement('Date','20080918110355123');
end;

procedure TTestReportDOM.TestWriteDateTime5;

Var
  E : TDOMElement;
  D : TDateTime;

begin
  E:=FRD.NewDOMElement('MyElement');
  D:=EncodeTime(11,03,55,123);
  FRD.WriteDateTime('Date',D,psAttr);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  AssertAttribute('Date','00000000110355123');
end;

procedure TTestReportDOM.TestWriteDateTime6;

Var
  E : TDOMElement;
  D : TDateTime;

begin
  E:=FRD.NewDOMElement('MyElement');
  D:=EncodeTime(11,03,55,123);
  FRD.WriteDateTime('Date',D,psElement);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  AssertValueElement('Date','00000000110355123');
end;

procedure TTestReportDOM.TestWriteStream1;
Var
  E : TDOMElement;
  S : TMemoryStream;

begin
  E:=FRD.NewDOMElement('MyElement');
  S:=TMemoryStream.Create;
  try
    FillBytes(S,15);
    FRD.WriteStream('Stream',S,psAttr);
    AssertSame('Current element not changed',E,FRD.CurrentElement);
    AssertAttribute('Stream','000102030405060708090A0B0C0D0E0F');
  finally
    FreeAndNil(S);
  end;
end;

procedure TTestReportDOM.TestWriteStream2;

Var
  E : TDOMElement;
  S : TMemoryStream;

begin
  E:=FRD.NewDOMElement('MyElement');
  S:=TMemoryStream.Create;
  try
    FillBytes(S,15);
    FRD.WriteStream('Stream',S,psElement);
    AssertSame('Current element not changed',E,FRD.CurrentElement);
    AssertValueElement('Stream','000102030405060708090A0B0C0D0E0F');
  finally
    FreeAndNil(S);
  end;
end;

procedure TTestReportDOM.TestWriteIntegerDiff1;

Var
  E : TDOMElement;

begin
  E:=FRD.NewDOMElement('MyElement');
  FRD.WriteIntegerDiff('Int',1,0,psAttr);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  AssertAttribute('Int','1');
end;

procedure TTestReportDOM.TestWriteIntegerDiff2;

Var
  E : TDOMElement;

begin
  E:=FRD.NewDOMElement('MyElement');
  FRD.WriteIntegerDiff('Int',1,1,psAttr);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  AssertNoAttribute('Int');
end;

procedure TTestReportDOM.TestWriteStringDiff1;

Var
  E : TDOMElement;

begin
  E:=FRD.NewDOMElement('MyElement');
  FRD.WriteStringDiff('Str','Aloha','mopa',psAttr);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  AssertAttribute('Str','Aloha');
end;

procedure TTestReportDOM.TestWriteStringDiff2;

Var
  E : TDOMElement;

begin
  E:=FRD.NewDOMElement('MyElement');
  FRD.WriteStringDiff('Str','Aloha','Aloha',psAttr);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  AssertNoAttribute('Str');
end;

procedure TTestReportDOM.TestWriteWideStringDiff1;

Var
  E : TDOMElement;
  W : WideString;

begin
  E:=FRD.NewDOMElement('MyElement');
  W:='Wide Aloha';
  FRD.WriteWideStringDiff('WideStr',W,W,psAttr);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  AssertNoAttribute('WideStr');
end;

procedure TTestReportDOM.TestWriteWideStringDiff2;
Var
  E : TDOMElement;
  W : WideString;

begin
  E:=FRD.NewDOMElement('MyElement');
  W:='Wide Aloha';
  FRD.WriteWideStringDiff('WideStr',W,W+' me',psAttr);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  AssertAttribute('WideStr',W);
end;

procedure TTestReportDOM.TestWriteBooleanDiff1;

Var
  E : TDOMElement;

begin
  E:=FRD.NewDOMElement('MyElement');
  FRD.WriteBooleanDiff('Bool',True,False,psAttr);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  AssertAttribute('Bool','1');
end;

procedure TTestReportDOM.TestWriteBooleanDiff2;
Var
  E : TDOMElement;

begin
  E:=FRD.NewDOMElement('MyElement');
  FRD.WriteBooleanDiff('Bool',True,True,psAttr);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  AssertNoAttribute('Bool');
end;

procedure TTestReportDOM.TestWriteFloatDiff1;

Var
  E : TDOMElement;

begin
  E:=FRD.NewDOMElement('MyElement');
  FRD.WriteFloatDiff('Float',1.23,1.24,psAttr);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  AssertAttribute('Float','1.230000000E+0000');
end;

procedure TTestReportDOM.TestWriteFloatDiff2;

Var
  E : TDOMElement;

begin
  E:=FRD.NewDOMElement('MyElement');
  FRD.WriteFloatDiff('Float',1.23,1.23,psAttr);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  AssertNoAttribute('Float');
end;

procedure TTestReportDOM.TestWriteDateTimeDiff1;

Var
  E : TDOMElement;
  D : TDateTime;

begin
  E:=FRD.NewDOMElement('MyElement');
  D:=EncodeDate(2008,9,18);
  FRD.WriteDateTimeDiff('Date',D,D+1,psAttr);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  AssertAttribute('Date','20080918');
end;

procedure TTestReportDOM.TestWriteDateTimeDiff2;
Var
  E : TDOMElement;
  D : TDateTime;

begin
  E:=FRD.NewDOMElement('MyElement');
  D:=EncodeDate(2008,9,18);
  FRD.WriteDateTimeDiff('Date',D,D,psAttr);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  AssertNoAttribute('Date');
end;

procedure TTestReportDOM.TestWriteDateTimeDiff3;
Var
  E : TDOMElement;
  D : TDateTime;

begin
  E:=FRD.NewDOMElement('MyElement');
  D:=EncodeDate(2008,9,18)+EncodeTime(0,0,0,1);
  FRD.WriteDateTimeDiff('Date',D,D,psAttr);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  AssertNoAttribute('Date');
end;

procedure TTestReportDOM.TestWriteDateTimeDiff4;
Var
  E : TDOMElement;
  D : TDateTime;

begin
  E:=FRD.NewDOMElement('MyElement');
  D:=EncodeDate(2008,9,18);
  FRD.WriteDateTimeDiff('Date',D,D+EncodeTime(0,0,0,1),psAttr);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  AssertAttribute('Date','20080918');
end;

procedure TTestReportDOM.TestWriteStreamDiff1;

Var
  E : TDOMElement;
  S : TMemoryStream;

begin
  E:=FRD.NewDOMElement('MyElement');
  S:=TMemoryStream.Create;
  try
    FillBytes(S,15);
    FRD.WriteStreamDiff('Stream',S,S,psAttr);
    AssertSame('Current element not changed',E,FRD.CurrentElement);
    AssertNoAttribute('Stream');
  finally
    FreeAndNil(S);
  end;
end;

procedure TTestReportDOM.TestWriteStreamDiff2;

Var
  E : TDOMElement;
  S,T : TMemoryStream;

begin
  E:=FRD.NewDOMElement('MyElement');
  S:=TMemoryStream.Create;
  try
    FillBytes(S,15);
    T:=TMemoryStream.Create;
    try
      FillBytes(T,15);
      FRD.WriteStreamDiff('Stream',S,T,psAttr);
      AssertSame('Current element not changed',E,FRD.CurrentElement);
      AssertNoAttribute('Stream');
    finally
      FreeAndNil(T);
    end;
  finally
    FreeAndNil(S);
  end;
end;

procedure TTestReportDOM.TestWriteStreamDiff3;

Var
  E : TDOMElement;
  S,T : TMemoryStream;

begin
  E:=FRD.NewDOMElement('MyElement');
  S:=TMemoryStream.Create;
  try
    FillBytes(S,15);
    T:=TMemoryStream.Create;
    try
      FillBytes(T,16);
      FRD.WriteStreamDiff('Stream',S,T,psAttr);
      AssertSame('Current element not changed',E,FRD.CurrentElement);
      AssertAttribute('Stream','000102030405060708090A0B0C0D0E0F');
    finally
      FreeAndNil(T);
    end;
  finally
    FreeAndNil(S);
  end;
end;

procedure TTestReportDOM.TestReadInteger1;

Var
  E : TDOMElement;

begin
  E:=FRD.NewDOMElement('MyElement');
  FRD.WriteInteger('Int',1,psAttr);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  AssertEquals('Reading integer property',1,FRD.ReadInteger('Int',-1,psAttr));
  AssertSame('Current element not changed',E,FRD.CurrentElement);
end;

procedure TTestReportDOM.TestReadInteger2;

Var
  E : TDOMElement;

begin
  E:=FRD.NewDOMElement('MyElement');
  FRD.WriteInteger('Int',1,psAttr);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  AssertEquals('Reading integer property with wrong storagetype',-1,FRD.ReadInteger('Int',-1,psElement));
  AssertSame('Current element not changed',E,FRD.CurrentElement);
end;

procedure TTestReportDOM.TestReadInteger3;

Var
  E : TDOMElement;

begin
  E:=FRD.NewDOMElement('MyElement');
  AssertEquals('Reading non-existent integer property (attr)',-1,FRD.ReadInteger('Int',-1,psAttr));
end;

procedure TTestReportDOM.TestReadInteger4;

Var
  E : TDOMElement;

begin
  E:=FRD.NewDOMElement('MyElement');
  AssertEquals('Reading non-existent integer property (element)',-1,FRD.ReadInteger('Int',-1,psElement));
end;

procedure TTestReportDOM.TestReadInteger5;

Var
  E : TDOMElement;

begin
  E:=FRD.NewDOMElement('MyElement');
  FRD.WriteString('Int','Aloha',psAttr);
  AssertEquals('Reading wrongly typed integer property (attr)',-1,FRD.ReadInteger('Int',-1,psAttr));
end;


procedure TTestReportDOM.TestReadInteger6;

Var
  E : TDOMElement;

begin
  E:=FRD.NewDOMElement('MyElement');
  FRD.WriteString('Int','Aloha',psElement);
  AssertEquals('Reading wrongly typed integer property (element)',-1,FRD.ReadInteger('Int',-1,psElement));
end;

procedure TTestReportDOM.TestReadString1;

Var
  E : TDOMElement;

begin
  E:=FRD.NewDOMElement('MyElement');
  FRD.WriteString('Str','Aloha',psAttr);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  AssertEquals('Reading string property (attr) ','Aloha',FRD.ReadString('Str','(none)',psattr));
  AssertSame('Current element not changed',E,FRD.CurrentElement);
end;

procedure TTestReportDOM.TestReadString2;

Var
  E : TDOMElement;

begin
  E:=FRD.NewDOMElement('MyElement');
  FRD.WriteString('Str','Aloha',psElement);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  AssertEquals('Reading string property (element)','Aloha',FRD.ReadString('Str','(none)',psElement));
  AssertSame('Current element not changed',E,FRD.CurrentElement);
end;

procedure TTestReportDOM.TestReadString3;

Var
  E : TDOMElement;

begin
  E:=FRD.NewDOMElement('MyElement');
  AssertEquals('Reading non-existent string property (attr)','(none)',FRD.ReadString('Str','(none)',psAttr));
end;

procedure TTestReportDOM.TestReadString4;

Var
  E : TDOMElement;

begin
  E:=FRD.NewDOMElement('MyElement');
  AssertEquals('Reading non-existent string property (element)','(none)',FRD.ReadString('Str','(none)',psElement));
end;

procedure TTestReportDOM.TestReadWideString1;

Var
  E : TDOMElement;
  W : WideString;

begin
  E:=FRD.NewDOMElement('MyElement');
  W:='Wide Aloha';
  FRD.WriteWideString('WideStr',W,psAttr);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  AssertEquals('Reading wide string property (attr) ',W,FRD.ReadWideString('WideStr','(none)',psattr));
  AssertSame('Current element not changed',E,FRD.CurrentElement);
end;

procedure TTestReportDOM.TestReadWideString2;
Var
  E : TDOMElement;
  W : WideString;

begin
  E:=FRD.NewDOMElement('MyElement');
  W:='Wide Aloha';
  FRD.WriteWideString('WideStr',W,psElement);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  AssertEquals('Reading wide string property (element) ',W,FRD.ReadWideString('WideStr','(none)',psElement));
  AssertSame('Current element not changed',E,FRD.CurrentElement);
end;

procedure TTestReportDOM.TestReadWideString3;

Var
  E : TDOMElement;
  W : WideString;

begin
  E:=FRD.NewDOMElement('MyElement');
  W:='Wide Aloha';
  AssertEquals('Reading non-existing wide string property (attr) ',W,FRD.ReadWideString('WideStr',W,psattr));
  AssertSame('Current element not changed',E,FRD.CurrentElement);
end;

procedure TTestReportDOM.TestReadWideString4;
Var
  E : TDOMElement;
  W : WideString;

begin
  E:=FRD.NewDOMElement('MyElement');
  W:='Wide Aloha';
  AssertEquals('Reading non-existing wide string property (element) ',W,FRD.ReadWideString('WideStr',W,psElement));
  AssertSame('Current element not changed',E,FRD.CurrentElement);
end;

procedure TTestReportDOM.TestReadDateTime1;

Var
  E : TDOMElement;
  D : TDateTime;

begin
  E:=FRD.NewDOMElement('MyElement');
  D:=EncodeDate(2008,9,18);
  FRD.WriteDateTime('Date',D,psAttr);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  AssertEquals('Reading date property (Attr)',D,FRD.ReadDateTime('Date',D-1,psAttr));
  AssertSame('Current element not changed',E,FRD.CurrentElement);
end;

procedure TTestReportDOM.TestReadDateTime2;

Var
  E : TDOMElement;
  D : TDateTime;

begin
  E:=FRD.NewDOMElement('MyElement');
  D:=EncodeDate(2008,9,18);
  FRD.WriteDateTime('Date',D,psElement);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  AssertEquals('Reading date property (Attr)',D,FRD.ReadDateTime('Date',D-1,psElement));
  AssertSame('Current element not changed',E,FRD.CurrentElement);
end;

procedure TTestReportDOM.TestReadDateTime3;

Var
  E : TDOMElement;
  D : TDateTime;

begin
  E:=FRD.NewDOMElement('MyElement');
  D:=EncodeDate(2008,9,18)+EncodeTime(11,03,55,123);
  FRD.WriteDateTime('Date',D,psAttr);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  AssertEquals('Reading date/time property (Attr)',D,FRD.ReadDateTime('Date',D-1,psAttr));
  AssertSame('Current element not changed',E,FRD.CurrentElement);
end;

procedure TTestReportDOM.TestReadDateTime4;

Var
  E : TDOMElement;
  D : TDateTime;

begin
  E:=FRD.NewDOMElement('MyElement');
  D:=EncodeDate(2008,9,18)+EncodeTime(11,03,55,123);
  FRD.WriteDateTime('Date',D,psElement);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  AssertEquals('Reading date/time property (element)',D,FRD.ReadDateTime('Date',D-1,psElement));
  AssertSame('Current element not changed',E,FRD.CurrentElement);
end;

procedure TTestReportDOM.TestReadDateTime5;

Var
  E : TDOMElement;
  D : TDateTime;

begin
  E:=FRD.NewDOMElement('MyElement');
  D:=EncodeTime(11,03,55,123);
  AssertEquals('Reading non-existent time property (element)',D,FRD.ReadDateTime('Date',D,psElement));
  AssertSame('Current element not changed',E,FRD.CurrentElement);
end;

procedure TTestReportDOM.TestReadDateTime6;

Var
  E : TDOMElement;
  D : TDateTime;

begin
  E:=FRD.NewDOMElement('MyElement');
  D:=EncodeTime(11,03,55,123);
  AssertEquals('Reading non-existent time property (element)',D,FRD.ReadDateTime('Date',D,psAttr));
  AssertSame('Current element not changed',E,FRD.CurrentElement);
end;

procedure TTestReportDOM.TestReadDateTime7;

Var
  E : TDOMElement;
  D : TDateTime;

begin
  E:=FRD.NewDOMElement('MyElement');
  D:=EncodeDate(2008,9,18);
  FRD.WriteString('Date','20080918',psAttr);
  AssertEquals('Reading date-only property (element)',D,FRD.ReadDateTime('Date',D-1,psAttr));
  AssertSame('Current element not changed',E,FRD.CurrentElement);
end;

procedure TTestReportDOM.TestReadDateTime8;

Var
  E : TDOMElement;
  D : TDateTime;

begin
  E:=FRD.NewDOMElement('MyElement');
  D:=EncodeDate(2008,9,18);
  FRD.WriteString('Date','20080918aaa',psAttr);
  AssertEquals('Reading wrong time/date-ok property (element)',D-1,FRD.ReadDateTime('Date',D-1,psAttr));
  AssertSame('Current element not changed',E,FRD.CurrentElement);
end;

procedure TTestReportDOM.TestReadDateTime9;

Var
  E : TDOMElement;
  D : TDateTime;

begin
  E:=FRD.NewDOMElement('MyElement');
  D:=EncodeDate(2008,9,18)+EncodeTime(11,03,55,0);
  FRD.WriteString('Date','20080918110355',psAttr);
  AssertEquals('Reading wrong time (no millisec)/date property (element)',DateTimeToStr(D),DateTimeToStr(FRD.ReadDateTime('Date',D-1,psAttr)));
  AssertSame('Current element not changed',E,FRD.CurrentElement);
end;

procedure TTestReportDOM.TestReadDateTime10;

Var
  E : TDOMElement;
  D : TDateTime;

begin
  E:=FRD.NewDOMElement('MyElement');
  D:=EncodeDate(2008,9,18)+EncodeTime(11,03,55,0);
  FRD.WriteString('Date','200809',psAttr);
  AssertEquals('Reading wrong date property (element)',D-1,FRD.ReadDateTime('Date',D-1,psAttr));
  AssertSame('Current element not changed',E,FRD.CurrentElement);
end;

procedure TTestReportDOM.TestReadBoolean1;

Var
  E : TDOMElement;

begin
  E:=FRD.NewDOMElement('MyElement');
  FRD.WriteBoolean('Bool',True,psAttr);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  AssertEquals('Reading true boolean property (attr)',True,FRD.ReadBoolean('Bool',False,psAttr));
  AssertSame('Current element not changed',E,FRD.CurrentElement);
end;

procedure TTestReportDOM.TestReadBoolean2;

Var
  E : TDOMElement;

begin
  E:=FRD.NewDOMElement('MyElement');
  FRD.WriteBoolean('Bool',False,psAttr);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  AssertEquals('Reading true boolean property (attr)',False,FRD.ReadBoolean('Bool',True,psAttr));
  AssertSame('Current element not changed',E,FRD.CurrentElement);
end;

procedure TTestReportDOM.TestReadBoolean3;

Var
  E : TDOMElement;

begin
  E:=FRD.NewDOMElement('MyElement');
  FRD.WriteBoolean('Bool',True,psElement);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  AssertEquals('Reading true boolean property (element)',True,FRD.ReadBoolean('Bool',False,psElement));
  AssertSame('Current element not changed',E,FRD.CurrentElement);
end;

procedure TTestReportDOM.TestReadBoolean4;
Var
  E : TDOMElement;

begin
  E:=FRD.NewDOMElement('MyElement');
  FRD.WriteBoolean('Bool',False,psElement);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  AssertEquals('Reading true boolean property (element)',False,FRD.ReadBoolean('Bool',True,psElement));
  AssertSame('Current element not changed',E,FRD.CurrentElement);
end;

procedure TTestReportDOM.TestReadBoolean5;
Var
  E : TDOMElement;

begin
  E:=FRD.NewDOMElement('MyElement');
  FRD.WriteString('Bool','Aloha',psAttr);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  AssertEquals('Reading invalid boolean property',True,FRD.ReadBoolean('Bool',True,psElement));
  AssertSame('Current element not changed',E,FRD.CurrentElement);
end;

procedure TTestReportDOM.TestReadBoolean6;
Var
  E : TDOMElement;

begin
  E:=FRD.NewDOMElement('MyElement');
  FRD.WriteString('Bool','Aloha',psAttr);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  AssertEquals('Reading invalid boolean property',False,FRD.ReadBoolean('Bool',False,psAttr));
  AssertSame('Current element not changed',E,FRD.CurrentElement);
end;

procedure TTestReportDOM.TestReadBoolean7;
Var
  E : TDOMElement;

begin
  E:=FRD.NewDOMElement('MyElement');
  AssertEquals('Reading non-existent boolean property',True,FRD.ReadBoolean('Bool',True,psAttr));
  AssertSame('Current element not changed',E,FRD.CurrentElement);
end;

procedure TTestReportDOM.TestReadBoolean8;
Var
  E : TDOMElement;

begin
  E:=FRD.NewDOMElement('MyElement');
  AssertEquals('Reading non-existent boolean property (element)',True,FRD.ReadBoolean('Bool',True,psElement));
  AssertSame('Current element not changed',E,FRD.CurrentElement);
end;

procedure TTestReportDOM.TestReadFloat1;

Var
  E : TDOMElement;

begin
  E:=FRD.NewDOMElement('MyElement');
  FRD.WriteFloat('Float',1.23,psAttr);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  AssertEquals('Reading float property (attr)',1.23,FRD.ReadFloat('Float',2.34,psAttr),0.001);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
end;

procedure TTestReportDOM.TestReadFloat2;

Var
  E : TDOMElement;

begin
  E:=FRD.NewDOMElement('MyElement');
  FRD.WriteFloat('Float',1.23,psElement);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  AssertEquals('Reading float property (attr)',1.23,FRD.ReadFloat('Float',2.34,psElement),0.001);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
end;

procedure TTestReportDOM.TestReadFloat3;
Var
  E : TDOMElement;

begin
  E:=FRD.NewDOMElement('MyElement');
  FRD.WriteString('Float','1.23a',psElement);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  AssertEquals('Reading wrong float property (element)',2.34,FRD.ReadFloat('Float',2.34,psElement),0.001);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
end;

procedure TTestReportDOM.TestReadFloat4;

Var
  E : TDOMElement;

begin
  E:=FRD.NewDOMElement('MyElement');
  FRD.WriteString('Float','1.23a',psAttr);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  AssertEquals('Reading wrong float property (attr)',2.34,FRD.ReadFloat('Float',2.34,psAttr),0.001);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
end;

procedure TTestReportDOM.TestReadFloat5;

Var
  E : TDOMElement;

begin
  E:=FRD.NewDOMElement('MyElement');
  AssertEquals('Reading nonexistent float property (attr)',2.34,FRD.ReadFloat('Float',2.34,psAttr),0.001);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
end;

procedure TTestReportDOM.TestReadFloat6;

Var
  E : TDOMElement;

begin
  E:=FRD.NewDOMElement('MyElement');
  AssertEquals('Reading nonexistent float property (element)',2.34,FRD.ReadFloat('Float',2.34,psElement),0.001);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
end;

procedure TTestReportDOM.TestReadFloat7;

Var
  E : TDOMElement;

begin
  E:=FRD.NewDOMElement('MyElement');
  FRD.WriteString('Float','1',psAttr);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  AssertEquals('Reading integer-formatted float property (attr)',1,FRD.ReadFloat('Float',2.34,psAttr),0.001);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
end;

procedure TTestReportDOM.TestReadStream1;

Var
  E : TDOMElement;
  S,T : TMemoryStream;

begin
  E:=FRD.NewDOMElement('MyElement');
  S:=TMemoryStream.Create;
  try
    FillBytes(S,15);
    FRD.WriteStream('Stream',S,psAttr);
    AssertSame('Current element not changed',E,FRD.CurrentElement);
    T:=TMemoryStream.Create;
    try
      AssertEquals('Reading stream data (attr)',True,FRD.ReadStream('Stream',T,psAttr));
      AssertEquals('Read stream equals written stream',True,FRD.StreamsEqual(S,T));
    finally
      T.Free;
    end;
  finally
    FreeAndNil(S);
  end;
end;

procedure TTestReportDOM.TestReadStream2;
Var
  E : TDOMElement;
  S,T : TMemoryStream;

begin
  E:=FRD.NewDOMElement('MyElement');
  S:=TMemoryStream.Create;
  try
    FillBytes(S,15);
    FRD.WriteStream('Stream',S,psElement);
    AssertSame('Current element not changed',E,FRD.CurrentElement);
    T:=TMemoryStream.Create;
    try
      AssertEquals('Reading stream data (element)',True,FRD.ReadStream('Stream',T,psElement));
      AssertEquals('Read stream equals written stream',True,FRD.StreamsEqual(S,T));
    finally
      T.Free;
    end;
  finally
    FreeAndNil(S);
  end;
end;

procedure TTestReportDOM.TestReadStream3;

Var
  E : TDOMElement;
  S,T : TMemoryStream;

begin
  E:=FRD.NewDOMElement('MyElement');
  FRD.WriteString('Stream','',psElement);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  T:=TMemoryStream.Create;
  try
    AssertEquals('Reading stream data (element)',False,FRD.ReadStream('Stream',T,psElement));
    AssertEquals('Read stream is empty',0,T.Size);
  finally
    T.Free;
  end;
end;

procedure TTestReportDOM.TestReadStream4;

Var
  E : TDOMElement;
  S,T : TMemoryStream;

begin
  E:=FRD.NewDOMElement('MyElement');
  FRD.WriteString('Stream','',psAttr);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  T:=TMemoryStream.Create;
  try
    AssertEquals('Reading stream data (attr)',False,FRD.ReadStream('Stream',T,psAttr));
    AssertEquals('Read stream is empty',0,T.Size);
  finally
    T.Free;
  end;
end;

procedure TTestReportDOM.TestReadStream5;

Var
  E : TDOMElement;
  S,T : TMemoryStream;

begin
  E:=FRD.NewDOMElement('MyElement');
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  T:=TMemoryStream.Create;
  try
    AssertEquals('Reading non-existent stream data (attr)',False,FRD.ReadStream('Stream',T,psAttr));
    AssertEquals('Read stream is empty',0,T.Size);
  finally
    T.Free;
  end;
end;

procedure TTestReportDOM.TestReadStream6;

Var
  E : TDOMElement;
  S,T : TMemoryStream;

begin
  E:=FRD.NewDOMElement('MyElement');
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  T:=TMemoryStream.Create;
  try
    AssertEquals('Reading non-existent stream data (element)',False,FRD.ReadStream('Stream',T,psElement));
    AssertEquals('Read stream is empty',0,T.Size);
  finally
    T.Free;
  end;
end;

{ TTestReportFrameDom }

procedure TTestReportFrameDom.Setup;
begin
  inherited Setup;
  FF:=TFPReportframe.Create(Nil);
  F2:=TFPReportframe.Create(Nil);
  FRD.NewDOMElement('Shape');
end;

procedure TTestReportFrameDom.TearDown;
begin
  FreeAndNil(FF);
  FreeAndNil(F2);
  inherited TearDown;
end;

Procedure FillFrame(FF : TFPReportFrame);

begin
  FF.Width:=2;
  FF.Pen:=psDot;
  FF.Shape:=fsRoundedRect;
  FF.Color:=23;
  FF.Lines:=[flTop,flBottom];
end;

procedure TTestReportFrameDom.FillFF;

begin
  FillFrame(FF);
end;

procedure TTestReportFrameDom.TestWrite;
begin
  FillFF;
  FF.WriteXML(FRD,Nil);
  AssertEquals('Width saved',2,FRD.ReadInteger('W',1));
  AssertEquals('Pen saved',ord(psDot),FRD.ReadInteger('W',1));
  AssertEquals('Color saved',ord(23),FRD.ReadInteger('C',1));
  AssertEquals('Shape saved',ord(fsRoundedRect),FRD.ReadInteger('S',0));
  AssertEquals('Lines saved',Integer([flTop,flBottom]),FRD.ReadInteger('L',0));
end;

procedure TTestReportFrameDom.TestWriteDiff;

begin
  FillFF;
  FF.WriteXML(FRD,F2);
  AssertEquals('Width saved',2,FRD.ReadInteger('W',1));
  AssertEquals('Pen saved',ord(psDot),FRD.ReadInteger('W',1));
  AssertEquals('Color saved',ord(23),FRD.ReadInteger('C',1));
  AssertEquals('Shape saved',ord(fsRoundedRect),FRD.ReadInteger('S',0));
  AssertEquals('Lines saved',Integer([flTop,flBottom]),FRD.ReadInteger('L',0));

end;

procedure TTestReportFrameDom.TestRead;
begin
  FillFF;
  FF.WriteXML(FRD,Nil);
  F2.ReadXML(FRD);
  AssertEquals('Width loaded',FF.Width,F2.Width);
  AssertEquals('Pen loaded',Ord(FF.Pen),Ord(F2.Pen));
  AssertEquals('Color loaded',Ord(FF.Color),Ord(F2.Color));
  AssertEquals('Shape loaded',Ord(FF.Shape),Ord(F2.Shape));
  AssertEquals('Lines loaded',Integer(FF.Lines),Integer(F2.Lines));
end;

{ TTestReportLayoutDom }

Procedure FillLayout(FL : TFPReportLayout);

begin
  FL.Top:=1.2;
  FL.Left:=3.4;
  FL.Width:=5.6;
  FL.Height:=7.8;
end;

procedure TTestReportLayoutDom.FillFL;
begin
  FillLayout(FL);
end;

procedure TTestReportLayoutDom.Setup;
begin
  inherited Setup;
  FL:=TFPReportLayout.Create(Nil);
  F2:=TFPReportLayout.Create(Nil);
  FRD.NewDOMElement('layout');
end;

procedure TTestReportLayoutDom.TearDown;
begin
  FreeAndNil(Fl);
  FreeAndNil(F2);
  inherited TearDown;
end;

procedure TTestReportLayoutDom.TestWrite;
begin
  FillFL;
  FL.WriteXML(FRD,Nil);
  AssertEquals('Top saved',1.2,FRD.ReadFloat('t',0.0));
  AssertEquals('Left saved',3.4,FRD.ReadFloat('l',0.0));
  AssertEquals('Width saved',5.6,FRD.ReadFloat('w',0.0));
  AssertEquals('Height saved',7.8,FRD.ReadFloat('h',0.0));
end;

procedure TTestReportLayoutDom.TestWriteDiff;
begin
  FillFL;
  FL.WriteXML(FRD,F2);
  AssertEquals('Top saved',1.2,FRD.ReadFloat('t',0.0));
  AssertEquals('Left saved',3.4,FRD.ReadFloat('l',0.0));
  AssertEquals('Width saved',5.6,FRD.ReadFloat('w',0.0));
  AssertEquals('Height saved',7.8,FRD.ReadFloat('h',0.0));
end;

procedure TTestReportLayoutDom.TestRead;
begin
  FillFL;
  FL.WriteXML(FRD,Nil);
  F2.ReadXML(FRD);
  AssertEquals('Top saved',FL.Top,F2.Top);
  AssertEquals('Left saved',FL.Left,F2.Left);
  AssertEquals('Width saved',FL.Width,F2.Width);
  AssertEquals('Height saved',FL.Height,F2.Height);
end;

{ TTestReportElementDOM }

procedure TTestReportElementDOM.FillFE;
begin
  FillLayout(FE.Layout);
  FillFrame(FE.Frame);
end;

procedure TTestReportElementDOM.Setup;
begin
  inherited Setup;
  FE:=TFPReportElement.Create(Nil);
  F2:=TFPReportElement.Create(Nil);
  FRD.NewDomElement('element');
end;

procedure TTestReportElementDOM.TearDown;
begin
  FreeAndNil(F2);
  FreeAndNil(FE);
  inherited TearDown;
end;

procedure TTestReportElementDOM.TestWrite1;

Var
  E : TDomElement;

begin
  FillFE;
  E:=FRD.CurrentElement;
  FE.WriteXML(FRD);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  E:=FRD.FindChild('Layout');
  AssertNotNull('Layout was saved',E);
  FRD.PushElement(E);
  try
    AssertEquals('Top saved',1.2,FRD.ReadFloat('t',0.0));
    AssertEquals('Left saved',3.4,FRD.ReadFloat('l',0.0));
    AssertEquals('Width saved',5.6,FRD.ReadFloat('w',0.0));
    AssertEquals('Height saved',7.8,FRD.ReadFloat('h',0.0));
  finally
    FRD.PopElement;
  end;
  E:=FRD.FindChild('Frame');
  AssertNotNull('Frame was saved',E);
  FRD.PushElement(E);
  try
    AssertEquals('Width saved',2,FRD.ReadInteger('W',1));
    AssertEquals('Pen saved',ord(psDot),FRD.ReadInteger('W',1));
    AssertEquals('Color saved',ord(23),FRD.ReadInteger('C',1));
    AssertEquals('Shape saved',ord(fsRoundedRect),FRD.ReadInteger('S',0));
    AssertEquals('Lines saved',Integer([flTop,flBottom]),FRD.ReadInteger('L',0));
  finally
    FRD.PopElement;
  end;
end;

procedure TTestReportElementDOM.TestWriteDiff1;

Var
  E : TDomElement;

begin
  FillFE;
  E:=FRD.CurrentElement;
  FE.WriteXML(FRD,F2);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  E:=FRD.FindChild('Layout');
  AssertNotNull('Layout was saved',E);
  FRD.PushElement(E);
  try
    AssertEquals('Top saved',1.2,FRD.ReadFloat('t',0.0));
    AssertEquals('Left saved',3.4,FRD.ReadFloat('l',0.0));
    AssertEquals('Width saved',5.6,FRD.ReadFloat('w',0.0));
    AssertEquals('Height saved',7.8,FRD.ReadFloat('h',0.0));
  finally
    FRD.PopElement;
  end;
  E:=FRD.FindChild('Frame');
  AssertNotNull('Frame was saved',E);
  FRD.PushElement(E);
  try
    AssertEquals('Width saved',2,FRD.ReadInteger('W',1));
    AssertEquals('Pen saved',ord(psDot),FRD.ReadInteger('W',1));
    AssertEquals('Color saved',ord(23),FRD.ReadInteger('C',1));
    AssertEquals('Shape saved',ord(fsRoundedRect),FRD.ReadInteger('S',0));
    AssertEquals('Lines saved',Integer([flTop,flBottom]),FRD.ReadInteger('L',0));
  finally
    FRD.PopElement;
  end;
end;

procedure TTestReportElementDOM.TestWriteDiff2;

Var
  E : TDomElement;

begin
  FillFE;
  E:=FRD.CurrentElement;
  F2.Layout.Assign(FE.Layout);
  FE.WriteXML(FRD,F2);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  E:=FRD.FindChild('Layout');
  AssertNotNull('Layout was saved',E);
  FRD.PushElement(E);
  try
    AssertEquals('Top saved',1.2,FRD.ReadFloat('t',0.0));
    AssertEquals('Left saved',3.4,FRD.ReadFloat('l',0.0));
    AssertEquals('Width saved',5.6,FRD.ReadFloat('w',0.0));
    AssertEquals('Height saved',7.8,FRD.ReadFloat('h',0.0));
  finally
    FRD.PopElement;
  end;
  E:=FRD.FindChild('Frame');
  AssertNotNull('Frame was saved',E);
  FRD.PushElement(E);
  try
    AssertEquals('Width saved',2,FRD.ReadInteger('W',1));
    AssertEquals('Pen saved',ord(psDot),FRD.ReadInteger('W',1));
    AssertEquals('Color saved',ord(23),FRD.ReadInteger('C',1));
    AssertEquals('Shape saved',ord(fsRoundedRect),FRD.ReadInteger('S',0));
    AssertEquals('Lines saved',Integer([flTop,flBottom]),FRD.ReadInteger('L',0));
  finally
    FRD.PopElement;
  end;
end;

procedure TTestReportElementDOM.TestWriteDiff3;

Var
  E : TDomElement;

begin
  FillFE;
  E:=FRD.CurrentElement;
  F2.Frame.Assign(FE.Frame);
  FE.WriteXML(FRD,F2);
  AssertSame('Current element not changed',E,FRD.CurrentElement);
  E:=FRD.FindChild('Layout');
  AssertNotNull('Layout was saved',E);
  E:=FRD.FindChild('Frame');
  AssertNull('Frame was not saved',E);
end;

procedure TTestReportElementDOM.TestRead1;

Var
  E : TDomElement;

begin
  FillFE;
  E:=FRD.CurrentElement;
  FE.WriteXML(FRD,Nil);
  F2.ReadXML(FRD);
  AssertEquals('Layout was read',True,FE.Layout.Equals(F2.Layout));
  AssertEquals('Frame was read',True,FE.Frame.Equals(F2.Frame));
end;

procedure TTestReportElementDOM.TestRead2;

Var
  E : TDomElement;

begin
  FillFE;
  E:=FRD.CurrentElement;
  F2.Frame.Assign(FE.Frame);
  // Only layout is written
  FE.WriteXML(FRD,F2);
  FreeAndNil(F2);
  F2:=TFPReportElement.Create(Nil);
  F2.ReadXML(FRD);
  AssertEquals('Layout was read',True,FE.Layout.Equals(F2.Layout));
  AssertEquals('Frame was not read',False,FE.Frame.Equals(F2.Frame));
end;

initialization
  RegisterTests({$IFDEF fptest} 'ReportDOM', {$ENDIF}
       [TTestReportDOM{$IFDEF fptest}.Suite{$ENDIF},
        TTestReportFrameDom{$IFDEF fptest}.Suite{$ENDIF},
        TTestReportLayoutDom{$IFDEF fptest}.Suite{$ENDIF},
        TTestReportElementDOM{$IFDEF fptest}.Suite{$ENDIF}
        ]);
end.


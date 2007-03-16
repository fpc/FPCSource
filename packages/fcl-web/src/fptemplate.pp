{
    $Id: header,v 1.1 2000/07/13 06:33:45 michael Exp $
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$H+}
{$define NOCONTNRS}
unit fpTemplate;


interface

uses
  SysUtils,
  Classes;

Const
  DefaultParseDepth = 100;
  MaxDelimLength    = 5;
  
Type
  TParseDelimiter = String[5];
  
Var
  DefaultStartDelimiter : TParseDelimiter = '{';
  DefaultEndDelimiter  : TParseDelimiter = '}';

Type
  TGetParamEvent = Procedure(Sender : TObject; Const ParamName : String; Out AValue : String) Of Object;

  { TTemplateParser }

  TTemplateParser = Class(TObject)
  Private
    FParseLevel : Integer;
    FMaxParseDepth : Integer;
    FEndDelimiter: TParseDelimiter;
    FStartDelimiter: TParseDelimiter;
    FRecursive: Boolean;
    FValues : TStringList;
    FOnGetParam: TGetParamEvent;
    function GetDelimiter(Index: integer): TParseDelimiter;
    function GetValue(Key : String): String;
    procedure SetDelimiter(Index: integer; const AValue: TParseDelimiter);
    procedure SetValue(Key : String; const AValue: String);
  Public
    Constructor Create;
    Destructor Destroy; override;
    Procedure Clear;
    Function GetParam(Const Key : String; Out AValue : String) : Boolean;
    Function ParseString(Src : String) : String;
    Function ParseStream(Src : TStream; Dest : TStream) : Integer; // Wrapper, Returns number of bytes written.
    Procedure ParseStrings(Src : TStrings; Dest : TStrings) ; // Wrapper
    Property OnGetParam : TGetParamEvent Read FOnGetParam Write FOnGetParam;  // Called if not found in values
    Property StartDelimiter : TParseDelimiter Index 1 Read GetDelimiter Write SetDelimiter; // Start char/string, default '}'
    Property EndDelimiter : TParseDelimiter Index 2 Read GetDelimiter Write SetDelimiter;  // end char/string, default '}'
    Property Values[Key : String] : String Read GetValue Write SetValue; // Contains static values.
    Property Recursive : Boolean Read FRecursive Write FRecursive;
  end;

  { TFPCustomTemplate }

  TFPCustomTemplate = Class(TPersistent)
  private
    FEndDelimiter: TParseDelimiter;
    FStartDelimiter: TParseDelimiter;
    FFileName: String;
    FTemplate: String;
    FOnGetParam: TGetParamEvent;
  Protected
    Procedure GetParam(Sender : TObject; Const ParamName : String; Out AValue : String);virtual;
    Function CreateParser : TTemplateParser; virtual;
  Public
    Function HasContent : Boolean;
    Function GetContent : String;
    Procedure Assign(Source : TPersistent); override;
    Property StartDelimiter : TParseDelimiter Read FStartDelimiter Write FStartDelimiter;
    Property EndDelimiter : TParseDelimiter Read FEndDelimiter Write FEndDelimiter;
    Property FileName : String Read FFileName Write FFileName;
    Property Template : String Read FTemplate Write FTemplate;
    Property OnGetParam : TGetParamEvent Read FOnGetParam Write FOnGetParam;
  end;
  
  TFPTemplate = Class(TFPCustomTemplate)
  Published
    Property FileName;
    Property Template;
  end;
  
  ETemplateParser = Class(Exception);

Var
  MaxParseDepth : Integer = DefaultParseDepth;


implementation

Resourcestring
  SErrParseDepthExceeded = 'Maximum parse level (%d) exceeded.';
  SErrNoEmptyDelimiters = 'Delimiters cannot be empty';
  
{ TTemplateParser }
Type

  { TStringItem }

  TStringItem = Class(TObject)
  Private
    FValue : String;
  Public
    Constructor Create(AValue : String);
    Property Value : String Read FValue Write FValue;
  end;

{ TStringItem }

constructor TStringItem.Create(AValue: String);
begin
  FValue:=AValue;
end;

function TTemplateParser.GetValue(Key : String): String;

Var
  I : Integer;

begin
  Result:='';
  If Assigned(FValues) then
    begin
    I:=FValues.IndexOf(Key);
    If (I<>-1) then
      Result:=TStringItem(FValues.Objects[i]).Value;
    end;
end;

function TTemplateParser.GetDelimiter(Index: integer): TParseDelimiter;
begin
  If Index=1 then
    Result:=FStartDelimiter
  else
    Result:=FEndDelimiter;
end;

procedure TTemplateParser.SetDelimiter(Index: integer;
  const AValue: TParseDelimiter);
begin
  If Length(AValue)=0 then
    Raise ETemplateParser.Create(SErrNoEmptyDelimiters);
  If Index=1 then
    FStartDelimiter:=AValue
  else
    FEndDelimiter:=AValue;

end;

procedure TTemplateParser.SetValue(Key : String; const AValue: String);

Var
  I : Integer;
  SI : TStringItem;
  
begin
  If (AValue='') then
    begin
    If Assigned(FValues) then
      begin
      I:=FValues.IndexOf(Key);
      If (I<>-1) then
        begin
        FValues.Objects[i].Free;
        FValues.Delete(I);
        end;
      end;
    end
  else
    begin
    if Not Assigned(FValues) then
      begin
      FVAlues:=TStringList.Create;
      FValues.Sorted:=True;
      end;
    I:=FValues.IndexOf(Key);
    If (I=-1) then
      FValues.AddObject(Key,TStringItem.Create(AValue))
    else
      TStringItem(FValues.Objects[I]).Value:=AValue;
    end;
end;

constructor TTemplateParser.Create;

begin
  FMaxParseDepth:=MaxParseDepth;
  FStartDelimiter:=DefaultStartDelimiter;
  FEndDelimiter:=DefaultEndDelimiter;
end;

destructor TTemplateParser.Destroy;

begin
  Clear;
  inherited Destroy;
end;

procedure TTemplateParser.Clear;

Var
  I : Integer;
  
begin
  If Assigned(FValues) then
    For I:=0 to FValues.Count-1 do
      FValues.Objects[i].Free;
  FreeAndNil(FValues);
end;

function TTemplateParser.GetParam(const Key: String; out AValue: String): Boolean;
  
Var
  I : Integer;
  
begin
  If Assigned(FValues) then
    I:=FValues.IndexOf(Key)
  else
    I:=-1;
  Result:=(I<>-1);
  If Result then
    AValue:=TStringItem(FValues.Objects[i]).Value
  else
    begin
    Result:=Assigned(FOnGetParam);
    If Result then
      FOnGetParam(Self,Key,AValue);
    end;
  If Result and Recursive then
    AValue:=ParseString(AValue);
end;

Function FindDelimiter(SP : PChar; D : TParseDelimiter; MaxLen : Integer) : PChar; Inline;

Var
  P,P2 : PChar;
  I,DLen : Integer;

begin
  Result:=Nil;
  DLen:=Length(D);
  Dec(MaxLen,(DLen-1));
  If MaxLen<=0 then
   exit;
  P:=SP;
  While (Result=Nil) and (P-SP<=MaxLen) do
    begin
    While (P-SP<=MaxLen) and (P^<>D[1]) do
      Inc(P);
    If ((P-SP)<=MaxLen) then
      begin
      Result:=P;
      P2:=P+1;
      // Check Other characters
      I:=2;
      While (I<=DLen) and (Result<>Nil) do
        If (P2^=D[i]) then
          begin
          inc(i);
          Inc(p2);
          end
        else
          begin
          P:=Result;
          Result:=Nil;
          end;
      // Either result<>Nil -> match or result=nil -> no match
      inc(P);
      end;
    end;
end;

Procedure AddToString(Var S : String; P : PChar; NChars : Integer);inline;

Var
  SLen : Integer;

begin
  SLen:=Length(S);
  SetLength(S,SLen+NChars);
  Move(P^,S[Slen+1],NChars);
end;

function TTemplateParser.ParseString(Src: String): String;

Var
  PN,PV : String;
  i,RLen,SLen,STlen : Integer;
  TS,TE,SP,P : PChar;

begin
  Inc(FParseLevel);
  If FParseLevel>FMaxParseDepth then
    Raise ETemplateParser.CreateFmt(SErrParseDepthExceeded,[FMaxParseDepth]);
  SLen:=Length(Src); // Minimum
  If SLen=0 then
    exit;
  STLen:=Length(FStartDelimiter);
  Result:='';
  SP:=PChar(Src);
  P:=SP;
  While (P-SP<SLen) do
    begin
    TS:=FindDelimiter(P,FStartDelimiter,SLen-(P-SP));
    If (TS=Nil) then
      begin
      TS:=P;
      P:=SP+SLen
      end
    else
      begin
      I:=TS-P;
      TE:=FindDelimiter(TS,FendDelimiter,SLen-I+1);
      If (TE=Nil) then
        begin
        TS:=P;
        P:=SP+SLen;
        end
      else
        begin
        // Add text prior to template to result
        AddToString(Result,P,I);
        // retrieve template name
        inc(TS,Length(FendDelimiter));
        I:=TE-TS;
        Setlength(PN,I);
        Move(TS^,PN[1],I);
        If GetParam(PN,PV) then
          begin
          Result:=Result+PV;
          end;
        P:=TE+Length(FEndDelimiter);
        TS:=P;
        end;
      end
    end;
  I:=P-TS;
  If (I>0) then
    AddToString(Result,TS,I);
end;

function TTemplateParser.ParseStream(Src: TStream; Dest: TStream): Integer;

Var
  SS : TStringStream;
  S,R : String;
  
begin
  SS:=TStringStream.Create('');
  Try
    SS.CopyFrom(Src,0);
    S:=SS.DataString;
  Finally
    SS.Free;
  end;
  R:=ParseString(S);
  Result:=Length(R);
  If (Result>0) then
    Dest.Write(R[1],Result);
end;

procedure TTemplateParser.ParseStrings(Src: TStrings; Dest: TStrings);

Var
  I : Integer;

begin
  For I:=0 to Src.Count-1 do
    Dest.Add(ParseString(Src[i]));
end;

{ TFPCustomTemplate }

procedure TFPCustomTemplate.GetParam(Sender: TObject; const ParamName: String;
  out AValue: String);
  
begin
  If Assigned(FOnGetParam) then
   FOnGetParam(Self,ParamName,AValue);
end;

function TFPCustomTemplate.CreateParser: TTemplateParser;

begin
  Result:=TTemplateParser.Create;
  If (FStartDelimiter<>'') then
    Result.StartDelimiter:=FStartDelimiter;
  If (FEndDelimiter<>'') then
    Result.EndDelimiter:=FEndDelimiter;
  Result.OnGetParam:=@GetParam;
end;

function TFPCustomTemplate.HasContent: Boolean;

begin
  Result:=(FTemplate<>'') or (FFileName<>'');
end;

function TFPCustomTemplate.GetContent: String;

Var
  P : TTemplateParser;
  S : TStringStream;
  F : TFileStream;
  
begin
  F:=Nil;
  S:=Nil;
  If HasContent then
    begin
    if (FFileName<>'') then
      begin
      F:=TFileStream.Create(FFileName,fmOpenRead);
      S:=TStringStream.Create('');
      end;
    Try
      P:=CreateParser;
      Try
        If (F<>Nil) then
          begin
          P.ParseStream(F,S);
          Result:=S.DataString;
          end
        else
          Result:=P.ParseString(FTemplate);
      Finally
        P.Free;
      end;
    Finally
      F.Free;
      S.Free;
    end;
    end;
end;

procedure TFPCustomTemplate.Assign(Source: TPersistent);

Var
  T : TFPCustomTemplate;

begin
  If Source is TFPCustomTemplate then
    begin
    T:=Source as TFPCustomTemplate;
    FEndDelimiter:=T.EndDelimiter;
    FStartDelimiter:=T.StartDelimiter;
    FFileName:=T.FileName;
    FTemplate:=T.Template;
    FOnGetParam:=T.OnGetParam;
    end
  else
    inherited Assign(Source);
end;

end.


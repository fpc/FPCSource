{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by the Free Pascal development team

    Abstract SQL scripting engine.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit sqlscript;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

type

  TSQLScriptStatementEvent = procedure(Sender: TObject; Statement: TStrings; var StopExecution: Boolean) of object;
  TSQLScriptDirectiveEvent = procedure(Sender: TObject; Directive, Argument: AnsiString; var StopExecution: Boolean) of object;
  TSQLScriptExceptionEvent = procedure(Sender: TObject; Statement: TStrings; TheException: Exception; var Continue: boolean) of object;
  TSQLSkipMode = (smNone, smIfBranch, smElseBranch, smAll);

  { TCustomSQLScript }

  TCustomSQLScript = class(TComponent)
  private
    FAutoCommit: Boolean;
    FLine: Integer;
    FCol: Integer;
    FDefines: TStrings;
    FOnException: TSQLScriptExceptionEvent;
    FSkipMode: TSQLSkipMode;
    FIsSkipping: Boolean;
    FSkipStackIndex: Integer;
    FSkipModeStack: array[0..255] of TSQLSkipMode;
    FIsSkippingStack: array[0..255] of Boolean;
    FAborted: Boolean;
    FUseSetTerm, FUseDefines, FUseCommit,
    FCommentsInSQL: Boolean;
    FTerminator: AnsiString;
    FSQL: TStrings;
    FCurrentStripped,
    FCurrentStatement: TStrings;
    FDirectives: TStrings;
    FComment,
    FEmitLine: Boolean;
    procedure SetDefines(const Value: TStrings);
    function FindNextSeparator(sep: array of string): AnsiString;
    procedure AddToStatement(value: AnsiString; ForceNewLine : boolean);
    procedure SetDirectives(value: TStrings);
    procedure SetSQL(value: TStrings);
    procedure SQLChange(Sender: TObject);
    function GetLine: Integer;
  protected
    procedure ClearStatement; virtual;
    procedure InternalStatement (Statement: TStrings; var StopExecution: Boolean); virtual;
    procedure InternalDirective (Directive, Argument: String; var StopExecution: Boolean); virtual;
    // Runs commit. If ComitRetaining, use CommitRetraining if possible, else stop/starttransaction
    procedure InternalCommit(CommitRetaining: boolean=true); virtual;
    Function ProcessConditional(Directive : String; Param : String) : Boolean; virtual;
    function NextStatement: AnsiString; virtual;
    procedure ProcessStatement; virtual;
    function Available: Boolean; virtual;
    procedure DefaultDirectives; virtual;
    procedure ExecuteStatement (Statement: TStrings; var StopExecution: Boolean); virtual; abstract;
    procedure ExecuteDirective (Directive, Argument: String; var StopExecution: Boolean); virtual; abstract;
    // Executes commit. If possible and CommitRetaining, use CommitRetaining, else
    procedure ExecuteCommit(CommitRetaining: boolean=true); virtual; abstract;
  public
    constructor Create (AnOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute; virtual;
  protected
    property Aborted: Boolean read FAborted;
    property Line: Integer read GetLine;
    Property AutoCommit : Boolean Read FAutoCommit Write FAutoCommit;
    property CommentsInSQL: Boolean read FCommentsInSQL write FCommentsInSQL;
    property UseSetTerm: Boolean read FUseSetTerm write FUseSetTerm;
    property UseCommit: Boolean read FUseCommit write FUseCommit;
    property UseDefines: Boolean read FUseDefines write FUseDefines;
    property Defines : TStrings Read FDefines Write SetDefines;
    property Directives: TStrings read FDirectives write SetDirectives;
    property Script: TStrings read FSQL write SetSQL;  // script to execute
    property Terminator: AnsiString read FTerminator write FTerminator;
    property OnException : TSQLScriptExceptionEvent read FOnException write FOnException;
  end;

  { TEventSQLScript }

  TEventSQLScript = class (TCustomSQLScript)
  private
    FAfterExec: TNotifyEvent;
    FBeforeExec: TNotifyEvent;
    FOnCommit: TNotifyEvent;
    FOnSQLStatement: TSQLScriptStatementEvent;
    FOnDirective: TSQLScriptDirectiveEvent;
  protected
    procedure ExecuteStatement (SQLStatement: TStrings; var StopExecution: Boolean); override;
    procedure ExecuteDirective (Directive, Argument: String; var StopExecution: Boolean); override;
    procedure ExecuteCommit(CommitRetaining: boolean=true); override;
  public
    procedure Execute; override;
    property Aborted;
    property Line;
  published
    property Directives;
    property Defines;
    property Script;
    property Terminator;
    property CommentsinSQL;
    property UseSetTerm;
    property UseCommit;
    property UseDefines;
    property OnException;
    property OnSQLStatement: TSQLScriptStatementEvent read FOnSQLStatement write FOnSQLStatement;
    property OnDirective: TSQLScriptDirectiveEvent read FOnDirective write FOnDirective;
    property OnCommit: TNotifyEvent read FOnCommit write FOnCommit;
    property BeforeExecute : TNotifyEvent read FBeforeExec write FBeforeExec;
    property AfterExecute : TNotifyEvent read FAfterExec write FAfterExec;
  end;

  ESQLScript = Class(Exception);

implementation

Resourcestring
 SErrIfXXXNestingLimitReached = '#IFDEF nesting limit reached';
 SErrInvalidEndif = '#ENDIF without #IFDEF';
 SErrInvalidElse  = '#ELSE without #IFDEF';

{ ---------------------------------------------------------------------
    Auxiliary Functions
  ---------------------------------------------------------------------}
  
function StartsWith(S1, S2: AnsiString): Boolean;

var
  L1,L2 : Integer;

begin
  Result:=False;
  L1:=Length(S1);
  L2:=Length(S2);
  if (L2=0) or (L1<L2) then
    Exit;
  Result:=(AnsiCompareStr(Copy(s1,1,L2),S2)=0);
  Result := Result and ((L2 = L1) or (s1[L2+1] = ' '));
end;

function GetFirstSeparator(S: AnsiString; Sep: array of string): AnsiString;

var
  i, C, M: Integer;

begin
  M:=length(S) + 1;
  Result:='';
  for i:=0 to high(Sep) do
    begin
    C:=Pos(Sep[i],S);
    if (C<>0) and (C<M) then
      begin
      M:=C;
      Result:=Sep[i];
      end;
    end;
end;

Function ConvertWhiteSpace(S : String) : String;

begin
  Result:=StringReplace(S,#13,' ',[rfReplaceAll]);
  Result:=StringReplace(Result,#10,' ',[rfReplaceAll]);
  Result:=Trim(Result);
end;

{ ---------------------------------------------------------------------
    TSQLScript
  ---------------------------------------------------------------------}

procedure TCustomSQLScript.SQLChange(Sender: TObject);
begin
  FLine:=1;
  FCol:=1;
end;

procedure TCustomSQLScript.SetDirectives(value: TStrings);

var 
  i : Integer;
  S : AnsiString;
  
begin
  FDirectives.Clear();
  if (Value<>Nil) then
    begin
    for i:=0 to value.Count - 1 do
      begin
      S:=UpperCase(ConvertWhiteSpace(value[i]));
      if Length(S)>0 then 
        FDirectives.Add(S);
      end;
    end;
  DefaultDirectives;
end;

procedure TCustomSQLScript.SetSQL(value: TStrings);
begin
  FSQL.Assign(value);
  FLine:=1;
  FCol:=1;
end;

function TCustomSQLScript.GetLine: Integer;
begin
  Result:=FLine - 1;
end;

procedure TCustomSQLScript.AddToStatement(value: AnsiString; ForceNewLine : Boolean);

  Procedure DA(L : TStrings);

  begin
    With L do
      if ForceNewLine or (Count=0) then
        Add(value)
      else
        Strings[Count-1]:=Strings[Count-1] + value;
  end;

begin
  DA(FCurrentStatement);
  if Not FComment then
    DA(FCurrentStripped);
end;

function TCustomSQLScript.FindNextSeparator(Sep: array of string): AnsiString;

var
  S: AnsiString;

begin
  Result:='';
  while (FLine<=FSQL.Count) do
    begin
    S:=FSQL.Strings[FLine-1];
    if (FCol>1) then
      begin
      S:=Copy(S,FCol,length(S));
      end;
    Result:=GetFirstSeparator(S,Sep);
    if (Result='') then
      begin
      if FEmitLine then
        AddToStatement(S,(FCol=1));
      FCol:=1;
      FLine:=FLine+1;
      end
    else
      begin
      if FEmitLine then
        AddToStatement(Copy(S,1,Pos(Result,S)-1),(FCol=1));
      FCol:=(FCol-1)+Pos(Result,S);
      break;
      end;
    end;
end;

function TCustomSQLScript.Available: Boolean;

begin
  With FSQL do
    Result:=(FLine<Count) or (FCol<Length(Strings[Count-1]))
end;

procedure TCustomSQLScript.InternalStatement(Statement: TStrings;  var StopExecution: Boolean);

var 
  cont : boolean;
  
begin
  try
    ExecuteStatement(Statement, StopExecution);
  except
    on E : Exception do
      begin
      cont := false;
      if assigned (FOnException) then
        FOnException (self, Statement, E, cont);
      if not cont then
        Raise;
      end;
  end;
end;

procedure TCustomSQLScript.InternalDirective(Directive, Argument: String;  var StopExecution: Boolean);

var 
  cont : boolean;
  l : TStrings;
  
begin
  try
    ExecuteDirective(Directive, Argument, StopExecution);
  except
    on E : Exception do
      begin
      cont := false;
      if assigned (FOnException) then
        begin
        l := TStringlist.Create;
        try
          L.Add(Directive);
          if Argument <> '' then
            L.Add(Argument);
          FOnException (self, l, E, cont);
        finally
          L.Free;
        end;
        end;
      if not cont then
        Raise;
      end;
  end;
end;

procedure TCustomSQLScript.InternalCommit(CommitRetaining: boolean=true);

var 
  cont : boolean;
  l : TStrings;
  
begin
  try
    ExecuteCommit(CommitRetaining);
  except
    on E : Exception do
      begin
      cont := false;
      if assigned (FOnException) then
        begin
        l := TStringlist.Create;
        try
          L.Add('COMMIT');
          FOnException (self, l, E, cont);
        finally
          L.Free;
        end;
        end;
      if not cont then
        Raise;
      end;
  end;
end;

procedure TCustomSQLScript.ClearStatement;

begin
  FCurrentStatement.Clear;
  FCurrentStripped.Clear;
end;

procedure TCustomSQLScript.ProcessStatement;

Var
  S,
  Directive : String;
  I : longint;

begin
  if (FCurrentStatement.Count=0) then
    Exit;
  S:=Trim(FCurrentStripped.Text);
  I:=0;
  Directive:='';
  While (i<FDirectives.Count) and (Directive='') do
    begin
    If StartsWith(AnsiUpperCase(S), FDirectives[i]) Then
      Directive:=FDirectives[i];
    Inc(I);
    end;
  If (Directive<>'') then
    begin
    S:=Trim(Copy(S,Length(Directive)+1,length(S)));
    If (Directive[1]='#') then
      begin
      if not FUseDefines or not ProcessConditional(Directive,S) then
        if Not FIsSkipping then
          InternalDirective (Directive, S, FAborted);
      end
    else If Not FIsSkipping then
      begin
      // If AutoCommit, skip any explicit commits.
      if FUseCommit
        and ((Directive = 'COMMIT') or (Directive = 'COMMIT WORK' {SQL standard}))
        and not FAutoCommit then
        InternalCommit(false) //explicit commit, no commit retaining
      else if FUseCommit
        and (Directive = 'COMMIT RETAIN') {at least Firebird syntax}
        and not FAutoCommit then
        InternalCommit(true)
      else if FUseSetTerm
        and (Directive = 'SET TERM' {Firebird/Interbase only}) then
        FTerminator:=S
      else
        InternalDirective (Directive,S,FAborted)
      end
    end
  else
    if (not FIsSkipping) then
      begin
      InternalStatement(FCurrentStatement,FAborted);
      If FAutoCommit and not FAborted then
        InternalCommit;
      end;
end;

procedure TCustomSQLScript.Execute;

begin
  FSkipMode:=smNone;
  FIsSkipping:=False;
  FSkipStackIndex:=0;
  Faborted:=False;
  DefaultDirectives;
  Repeat
    NextStatement();
    if Length(Trim(FCurrentStripped.Text))>0 then
      ProcessStatement;
  Until FAborted or Not Available;
end;

function TCustomSQLScript.NextStatement: AnsiString;

var
  pnt: AnsiString;
  terminator_found: Boolean;

begin
  terminator_found:=False;
  ClearStatement;
  while FLine <= FSQL.Count do
    begin
    pnt:=FindNextSeparator([FTerminator, '/*', '"', '''', '--']);
    if (pnt=FTerminator) then
      begin
      FCol:=FCol + length(pnt);
      terminator_found:=True;
      break;
      end
    else if pnt = '/*' then
      begin
      FComment:=True;
      if FCommentsInSQL then
        AddToStatement(pnt,false)
      else
        FEmitLine:=False;
      FCol:=FCol + length(pnt);
      pnt:=FindNextSeparator(['*/']);
      if FCommentsInSQL then
        AddToStatement(pnt,false)
      else
        FEmitLine:=True;
      FCol:=FCol + length(pnt);
      FComment:=False;
      end
    else if pnt = '--' then
      begin
      FComment:=True;
      if FCommentsInSQL then
        AddToStatement(Copy(FSQL[FLine-1],FCol,Length(FSQL[FLine-1])-FCol+1),True);
      Inc(Fline);
      FCol:=0;
      FComment:=False;
      end
    else if pnt = '"' then
      begin
      AddToStatement(pnt,false);
      FCol:=FCol + length(pnt);
      pnt:=FindNextSeparator(['"']);
      AddToStatement(pnt,false);
      FCol:=FCol + length(pnt);
      end
    else if pnt = '''' then
      begin
      AddToStatement(pnt,False);
      FCol:=FCol + length(pnt);
      pnt:=FindNextSeparator(['''']);
      AddToStatement(pnt,false);
      FCol:=FCol + length(pnt);
      end;
    end;
  if not terminator_found then
    ClearStatement;
  while (FCurrentStatement.Count > 0) and (trim(FCurrentStatement.Strings[0]) = '') do
    FCurrentStatement.Delete(0);
  while (FCurrentStripped.Count > 0) and (trim(FCurrentStripped.Strings[0]) = '') do
    FCurrentStripped.Delete(0);
  Result:=FCurrentStatement.Text;
end;

Constructor TCustomSQLScript.Create (AnOwner: TComponent);

Var
  L : TStringList;

begin
  inherited;
  L:=TStringList.Create;
  With L do
    begin
    Sorted:=True;
    Duplicates:=dupIgnore;
    end;
  FDefines:=L;  
  FCommentsInSQL:=True;
  FTerminator:=';';
  L:=TStringList.Create();
  L.OnChange:=@SQLChange;
  FSQL:=L;
  FDirectives:=TStringList.Create();
  FCurrentStripped:=TStringList.Create();
  FCurrentStatement:=TStringList.Create();
  FLine:=1;
  FCol:=1;
  FEmitLine:=True;
  FUseCommit := true;
  FUseDefines := True;
  FUseSetTerm := True;
  DefaultDirectives;
end;

destructor TCustomSQLScript.Destroy;

begin
  FreeAndNil(FCurrentStripped);
  FreeAndNil(FCurrentStatement);
  FreeAndNil(FSQL);
  FreeAndNil(FDirectives);
  FreeAndNil(FDefines);
  inherited Destroy;
end;

procedure TCustomSQLScript.SetDefines(const Value: TStrings);
begin
  FDefines.Assign(Value);
end;

procedure TCustomSQLScript.DefaultDirectives;
begin
  With FDirectives do
    begin
    // Insertion order matters as testing for directives will be done with StartsWith
    if FUseSetTerm then
      Add('SET TERM');
    if FUseCommit then
    begin
      Add('COMMIT WORK'); {SQL Standard, equivalent to commit}
      Add('COMMIT RETAIN'); {Firebird/Interbase; probably won't hurt on other dbs}
      Add('COMMIT'); {Shorthand used in many dbs, e.g. Firebird}
    end;
    if FUseDefines then
      begin
      Add('#IFDEF');
      Add('#IFNDEF');
      Add('#ELSE');
      Add('#ENDIF');
      Add('#DEFINE');
      Add('#UNDEF');
      Add('#UNDEFINE');
      end;
    end;
end;

Function TCustomSQLScript.ProcessConditional(Directive: String; Param : String) : Boolean;

  Procedure PushSkipMode;

  begin
    if FSkipStackIndex=High(FSkipModeStack) then
      Raise ESQLScript.Create(SErrIfXXXNestingLimitReached);
    FSkipModeStack[FSkipStackIndex]:=FSkipMode;
    FIsSkippingStack[FSkipStackIndex]:=FIsSkipping;
    Inc(FSkipStackIndex);
  end;

  Procedure PopSkipMode;

  begin
    if FSkipStackIndex = 0 then
      Raise ESQLScript.Create(SErrInvalidEndif);
    Dec(FSkipStackIndex);
    FSkipMode := FSkipModeStack[FSkipStackIndex];
    FIsSkipping := FIsSkippingStack[FSkipStackIndex];
  end;

Var
  Index : Integer;

begin
  Result:=True;
  if (Directive='#DEFINE') then
    begin
    if not FIsSkipping then
      FDefines.Add(Param);
    end
  else if (Directive='#UNDEF') or (Directive='#UNDEFINE') then
    begin
    if not FIsSkipping then
      begin
      Index:=FDefines.IndexOf(Param);
      if (Index>=0) then
        FDefines.Delete(Index);
      end;
    end
  else if (Directive='#IFDEF') or (Directive='#IFNDEF') then
    begin
    PushSkipMode;
    if FIsSkipping then
      begin
      FSkipMode:=smAll;
      FIsSkipping:=true;
      end
    else
      begin
      Index:=FDefines.IndexOf(Param);
      if ((Directive='#IFDEF') and (Index<0)) or
         ((Directive='#IFNDEF') and (Index>=0)) then
        begin
        FSkipMode:=smIfBranch;
        FIsSkipping:=true;
        end
      else
        FSkipMode := smElseBranch;
      end;
    end
  else if (Directive='#ELSE') then
    begin
    if (FSkipStackIndex=0) then
      Raise ESQLScript.Create(SErrInvalidElse);
    if (FSkipMode=smIfBranch) then
      FIsSkipping:=false
    else if (FSkipMode=smElseBranch) then
      FIsSkipping:=true;
    end
  else if (Directive='#ENDIF') then
    PopSkipMode
  else
    Result:=False;
end;

{ TEventSQLScript }

procedure TEventSQLScript.ExecuteStatement(SQLStatement: TStrings;
  var StopExecution: Boolean);
begin
  if assigned (FOnSQLStatement) then
    FOnSQLStatement (self, SQLStatement, StopExecution);
end;

procedure TEventSQLScript.ExecuteDirective(Directive, Argument: String;
  var StopExecution: Boolean);
begin
  if assigned (FOnDirective) then
    FOnDirective (Self, Directive, Argument, StopExecution);
end;

procedure TEventSQLScript.ExecuteCommit(CommitRetaining: boolean=true);
begin
  if assigned (FOnCommit) then
    FOnCommit (Self);
end;

procedure TEventSQLScript.Execute;
begin
  if assigned (FBeforeExec) then
    FBeforeExec (Self);
  inherited Execute;
  if assigned (FAfterExec) then
    FAfterExec (Self);
end;

end.


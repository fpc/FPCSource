unit FPCodTmp; { Code Templates }

interface

uses Objects,
     WUtils;

type
    PCodeTemplate = ^TCodeTemplate;
    TCodeTemplate = object(TObject)
      constructor Init(const AShortCut: string; AText: PUnsortedStringCollection);
      function    GetShortCut: string;
      procedure   GetText(AList: PUnsortedStringCollection);
      destructor  Done; virtual;
    private
      ShortCut: PString;
      Text: PUnsortedStringCollection;
    end;

    PCodeTemplateCollection = ^TCodeTemplateCollection;
    TCodeTemplateCollection = object(TSortedCollection)
      function Compare(Key1, Key2: Pointer): sw_Integer; virtual;
      function SearchByShortCut(const ShortCut: string): PCodeTemplate; virtual;
    end;

const CodeTemplates : PCodeTemplateCollection = nil;

function FPTranslateCodeTemplate(const Shortcut: string; ALines: PUnsortedStringCollection): boolean;

procedure InitCodeTemplates;
procedure DoneCodeTemplates;

implementation

constructor TCodeTemplate.Init(const AShortCut: string; AText: PUnsortedStringCollection);
procedure CopyIt(P: PString); {$ifndef FPC}far;{$endif}
begin
  Text^.Insert(NewStr(GetStr(P)));
end;
begin
  inherited Init;
  ShortCut:=NewStr(AShortCut);
  New(Text, Init(10,10));
  if Assigned(AText) then
  AText^.ForEach(@CopyIt);
end;

function TCodeTemplate.GetShortCut: string;
begin
  GetShortCut:=GetStr(ShortCut);
end;

procedure TCodeTemplate.GetText(AList: PUnsortedStringCollection);
procedure CopyIt(P: PString); {$ifndef FPC}far;{$endif}
begin
  AList^.Insert(NewStr(GetStr(P)));
end;
begin
  if Assigned(AList) then
  Text^.ForEach(@CopyIt);
end;

destructor TCodeTemplate.Done;
begin
  if Assigned(ShortCut) then DisposeStr(ShortCut); ShortCut:=nil;
  if Assigned(Text) then Dispose(Text, Done); Text:=nil;
  inherited Done;
end;

function TCodeTemplateCollection.Compare(Key1, Key2: Pointer): sw_Integer;
var K1: PCodeTemplate absolute Key1;
    K2: PCodeTemplate absolute Key2;
    R: Sw_integer;
    S1,S2: string;
begin
  S1:=UpCaseStr(K1^.GetShortCut);
  S2:=UpCaseStr(K2^.GetShortCut);
  if S1<S2 then R:=-1 else
  if S1>S2 then R:=1 else
  R:=0;
  Compare:=R;
end;

function TCodeTemplateCollection.SearchByShortCut(const ShortCut: string): PCodeTemplate;
var T: TCodeTemplate;
    Index: sw_integer;
    P: PCodeTemplate;
begin
  T.Init(ShortCut,nil);
  if Search(@T,Index)=false then P:=nil else
    P:=At(Index);
  T.Done;
  SearchByShortCut:=P;
end;

function FPTranslateCodeTemplate(const Shortcut: string; ALines: PUnsortedStringCollection): boolean;
var OK: boolean;
    P: PCodeTemplate;
begin
  OK:=Assigned(CodeTemplates);
  if OK then
  begin
    P:=CodeTemplates^.SearchByShortCut(ShortCut);
    OK:=Assigned(P);
    if OK then
      P^.GetText(ALines);
  end;
{$ifdef GABOR}
  {
    this is for testing purposes only. once the user front-end for defining
    CodeTemplates is implemented, this can be removed - Gabor
  }
  if (OK=false) and (UpCaseStr(ShortCut)='CASES') then
  begin
    OK:=true;
    ALines^.Insert(NewStr('case  of'));
    ALines^.Insert(NewStr('  : ;'));
    ALines^.Insert(NewStr('  : ;'));
    ALines^.Insert(NewStr('end;'));
  end else
  if (OK=false) and (UpCaseStr(ShortCut)='CASEE') then
  begin
    OK:=true;
    ALines^.Insert(NewStr('case  of'));
    ALines^.Insert(NewStr('  : ;'));
    ALines^.Insert(NewStr('  : ;'));
    ALines^.Insert(NewStr('else ;'));
    ALines^.Insert(NewStr('end;'));
  end else
  ;
{$endif}
  FPTranslateCodeTemplate:=OK;
end;

procedure InitCodeTemplates;
begin
  if Assigned(CodeTemplates) then Exit;

  New(CodeTemplates, Init(10,10));
end;

procedure DoneCodeTemplates;
begin
  if Assigned(CodeTemplates) then Dispose(CodeTemplates, Done);
  CodeTemplates:=nil;
end;

END.
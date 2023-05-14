unit TCResolveMultiErrors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, testregistry, tcresolver, PParser,
  PScanner;

type

  { TTestResolveMultiErrors }

  TTestResolveMultiErrors = Class(TCustomTestResolver)
  protected
    procedure SetUp; override;
    procedure OnParserError(Sender: TObject; const aContext: TRecoveryContext;
      var aAllowRecovery: Boolean); virtual;
  Published
    procedure TestStatements_IdentifiersNotFound;
  end;

implementation

{ TTestResolveMultiErrors }

procedure TTestResolveMultiErrors.OnParserError(Sender: TObject;
  const aContext: TRecoveryContext; var aAllowRecovery: Boolean);
{$IFDEF VerbosePasResolver}
var
  aParser: TPasParser;
{$ENDIF}
begin
  if Sender=nil then exit;
  if aContext.Error=nil then ;
  if aAllowRecovery then ;
  {$IFDEF VerbosePasResolver}
  aParser:=Sender as TPasParser;
  writeln('TTestResolveMultiErrors.OnParserError ',aParser.LastMsgType,' (',aParser.LastMsgNumber,') Msg="',aParser.LastMsg,'"');
  {$ENDIF}
end;

procedure TTestResolveMultiErrors.SetUp;
begin
  inherited SetUp;
  Parser.MaxErrorCount:=100;
  Parser.OnError:=@OnParserError;
end;

procedure TTestResolveMultiErrors.TestStatements_IdentifiersNotFound;
begin
  StartProgram(false);
  Add('begin');
  //Add('  a:=3;');
  //Add('  b;');
  ParseProgram;
end;

initialization
  RegisterTests([TTestResolveMultiErrors]);

end.


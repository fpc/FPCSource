program dpk2lpk;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Types, CustApp, dpktolpk;

type


  { TDPK2LPKApplication }

  TDPK2LPKApplication = class(TCustomApplication)
  private
    procedure WriteLog(const Msg: String);
  protected
    FQuiet : Boolean;
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Usage(const Msg: String); virtual;
  end;


{ TDPK2LPKApplication }

procedure TDPK2LPKApplication.WriteLog(Const Msg : String);

begin
  if FQuiet then Exit;
  Writeln(StdErr,Msg);
end;

procedure TDPK2LPKApplication.DoRun;

const
  Short = 'ho:k:uq';
  Long : Array of string = ('help','output:','known:','update','quiet');

var
  ErrorMsg: String;
  Cnv : TDPK2LPKConverter;
  OFN,PFN,KFN : String;
  FNS : TStringDynArray;

begin
  Terminate;
  ErrorMsg:=CheckOptions(Short,Long);
  if (ErrorMsg<>'') or HasOption('h','help') then
    begin
    Usage(ErrorMsg);
    Exit;
    end;
  FNS:=GetNonOptions(Short,Long);
  if Length(FNS)=0 then
    begin
    Usage('Need one or more input files');
    exit;
    end;
  FQuiet:=HasOption('q','quiet');
  OFN:=GetOptionValue('o','output');

  if (OFN<>'') and (Length(FNS)>1) then
    begin
    Usage('Cannot specify output file with more than one input file');
    exit;
    end;
  Cnv:=TDPK2LPKConverter.Create(Self);
  try
    KFN:=GetOptionValue('k','known');
    if (KFN<>'') and FileExists(KFN) then
      CNV.KnownPackages.LoadFromFile(KFN);
    for PFN in FNS do
      begin
      if (OFN='') then
        OFN:=ChangeFileExt(PFN,'.lpk');
      CNV.UpdateKnown:=HasOption('u','update');
      WriteLog(Format('Converting %s to %s',[PFN,OFN]));
      try
        CNV.Convert(PFN,OFN);
      except
        On E : Exception do
          WriteLog(Format('Error %s Converting %s to %s : %s',[E.ClassName,PFN,OFN,E.MEssage]));
      end;
      OFN:='';
      end;
    if HasOption('u','update') and (KFN<>'') then
      CNV.KnownPackages.SaveToFile(KFN);
  finally
    Cnv.Free;
  end;

end;

constructor TDPK2LPKApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TDPK2LPKApplication.Destroy;
begin
  inherited Destroy;
end;

procedure TDPK2LPKApplication.Usage(Const Msg : String);

begin
  if Msg<>'' then
    Writeln('Error: ',Msg);
  Writeln('Usage: ',ExeName, ' [options] File1 [File2]');
  Writeln('Where [options] is one or more of:');
  Writeln('-h --help        this help');
  Writeln('-k --known=FILE  File with known packages, which can be added to requires if encountered');
  Writeln('-q --quiet       Produce less output');
  Writeln('-u --update      Add processed packages to known packages file');
  ExitCode:=Ord(Msg<>'');
end;

var
  Application: TDPK2LPKApplication;
begin
  Application:=TDPK2LPKApplication.Create(nil);
  Application.Title:='Convert Delphi To Lazarus Package';
  Application.Run;
  Application.Free;
end.


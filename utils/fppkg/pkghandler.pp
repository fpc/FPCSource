{$mode objfpc}
{$h+}
unit pkghandler;

interface

uses Classes,SysUtils, fpmktype;

Type

  TVerbosity = (vError,vInfo,vCommands,vDebug);
  TVerbosities = Set of TVerbosity;
  TMessageEvent = Procedure (Sender : TObject; Const Msg : String) of object;
  
  { TPackageHandler }

  TPackageHandler = Class(TComponent)
  private
    FBackupFile: Boolean;
    FOnMessage: TMessageEvent;
    FVerbosity: TVerbosities;
  Protected
    Procedure Error(Const Msg : String);
    Procedure Error(Const Fmt : String; Args : Array of const);
  Public
    Procedure BackupFile(Const FileName : String);
    Constructor Create(AOwner : TComponent); override;
    Procedure Verbose(Msg : String); 
    Procedure Verbose(Fmt : String; Args : Array of const); 
    Procedure Verbose(Level : TVerbosity; Msg : String);
    Procedure Verbose(Level : TVerbosity; Fmt : String; Args : Array of const);
    Property BackupFiles : Boolean Read FBackupFile Write FBackupFile;
    Property OnMessage : TMessageEvent Read FOnMessage Write FOnMessage;
    Property Verbosity : TVerbosities Read FVerbosity Write FVerbosity;
  end;
  
  EPackageHandler = Class(EInstallerError);

Function StringToVerbosity (S : String) : TVerbosity;
Function VerbosityToString (V : TVerbosity): String;

  
Implementation

uses pkgmessages,typinfo;

function StringToVerbosity(S: String): TVerbosity;

Var
  I : integer;

begin
  I:=GetEnumValue(TypeInfo(TVerbosity),'v'+S);
  If (I<>-1) then
    Result:=TVerbosity(I)
  else
    Raise EPackageHandler.CreateFmt(SErrInvalidVerbosity,[S]);
end;

Function VerbosityToString (V : TVerbosity): String;
begin
  Result:=GetEnumName(TypeInfo(TVerbosity),Integer(V));
  Delete(Result,1,1);// Delete 'v'
end;

{ TPackageHandler }

procedure TPackageHandler.Error(const Msg: String);
begin
  Raise EPackageHandler.CreateFmt('%s : %s',[ClassName,Msg]);
end;

procedure TPackageHandler.Error(const Fmt: String; Args: Array of const);
begin
  Error(Format(Fmt,Args));
end;

procedure TPackageHandler.BackupFile(const FileName: String);

Var
  BFN : String;
  

begin
  BFN:=FileName+'.bak';
  If not RenameFile(FileName,BFN) then
    Error(SErrBackupFailed,[FileName,BFN]);
end;

constructor TPackageHandler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FVerbosity:=[vError];
end;

procedure TPackageHandler.Verbose(Msg: String);
begin
  Verbose(vInfo,Msg);
end;

procedure TPackageHandler.Verbose(Fmt: String; Args: array of const);
begin
  Verbose(vInfo,Fmt,Args);
end;

procedure TPackageHandler.Verbose(Level: TVerbosity; Msg: String);
begin
  If (Level in FVerbosity) and Assigned(FOnMessage) then
    FOnMessage(Self,Msg);
end;

procedure TPackageHandler.Verbose(Level: TVerbosity; Fmt: String;
  Args: array of const);
begin
  // Save a format call
  If (Level in FVerbosity) and Assigned(FOnMessage) then
    Verbose(Level,Format(Fmt,Args));
end;

end.

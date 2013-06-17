unit cmpoptions;

{$mode objfpc}

interface

type

  { TOptions }

  TOptions = class(TObject)
  private
    FHelp: boolean;
    FNoDestFileExtention: boolean;
    FNoSourceFileExtention: boolean;
    FSilent: boolean;
    FSourceFileExtention: String;
    FSourceMask: String;
    FDestFileExtention: String;
    FDestPath: String;
  public
    constructor Create;

    procedure LoadParams;

    property Help                 : boolean read FHelp write FHelp;
    property SourceMask           : String read FSourceMask write FSourceMask;
    property SourceFileExtention  : String read FSourceFileExtention write FSourceFileExtention;
    property NoSourceFileExtention: boolean read FNoSourceFileExtention;

    property DestPath             : String read FDestPath write FDestPath;
    property DestFileExtention    : String read FDestFileExtention write FDestFileExtention;
    property NoDestFileExtention  : boolean read FNoDestFileExtention;
    property Silent               : boolean read FSilent;
  end;

implementation

uses SysUtils;

{ TOptions }

constructor TOptions.Create;
begin
  FHelp                     := false;
  FSourceMask               := '';
  FNoSourceFileExtention    := false;
  FDestFileExtention        := '';
  FDestPath                 := '';
  FSilent                   := false;
end;

procedure TOptions.LoadParams;
var
  i: integer;
  sParam     : Char;
  sValue     : String;
  IsInvalidParam: boolean;
begin
  if ParamCount = 0 then FHelp := true
   else FHelp := false;

  FSourceMask            := IncludeTrailingBackslash(GetCurrentDir) + '*.*';
  FSourceFileExtention   := '';
  FNoSourceFileExtention := false;

  FDestPath              := IncludeTrailingBackslash(GetCurrentDir);
  FDestFileExtention     := '';
  FNoDestFileExtention   := false;

  FSilent := false;

  for i := 1 to ParamCount do
  begin
    if copy(ParamStr(i), 1, 1) = '-' then
    begin
      sParam := copy(ParamStr(i) + '  ', 2, 1)[1];
      sValue := copy(ParamStr(i), 3, length(ParamStr(i)));

      IsInvalidParam := false;
      case sParam of
         'h': FHelp := true;
         'm': begin
                FSourceMask := sValue;

                if copy(FSourceMask, 1, 2) = '\\' then FSourceMask := ExpandUNCFileName(FSourceMask)
                 else FSourceMask := ExpandFileName(FSourceMask);

                if ExtractFileName(FSourceMask) = '' then FSourceMask := FSourceMask + '*';
              end;
         'n': begin
                FSourceFileExtention   := sValue;
                FNoSourceFileExtention := FSourceFileExtention = '';
              end;
         'd': begin
                FDestPath := sValue;

                if copy(FDestPath, 1, 2) = '\\' then FDestPath := ExpandUNCFileName(FDestPath)
                 else FDestPath := ExpandFileName(FDestPath);
              end;
         'e': begin
                FDestFileExtention := sValue;
                FNoDestFileExtention := FDestFileExtention = '';
              end;
         's': FSilent := true;
         else begin
                FHelp := true;
                writeln(format('invalid param "%s"', [ParamStr(i)]));
              end;
      end;
    end
    else IsInvalidParam := true;

    if (FNoSourceFileExtention = false) and
       (FSourceFileExtention = '') then
    begin
      FSourceFileExtention := ExtractFileExt(FSourceMask);
    end
    else if (ExtractFileExt(FSourceMask) <> '') and
            (FSourceFileExtention <> '') and
            (ExtractFileExt(FSourceMask) <> FSourceFileExtention) then
    begin
      writeln(format('parameter conflict: different sourcefile extention "%s" and "%s"',
                     [ExtractFileExt(FSourceMask), FSourceFileExtention]));
      FHelp := true;
    end;

    if IsInvalidParam then
    begin
      FHelp := true;
      writeln(format('invalid param "%s"', [ParamStr(i)]));
    end;
  end;
end;


end.

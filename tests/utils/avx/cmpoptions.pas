unit cmpoptions;

{$mode objfpc}

interface

type

  { TOptions }

  TOptions = class(TObject)
  private
    FHelp: boolean;
    FNoDestFileExtension: boolean;
    FNoSourceFileExtension: boolean;
    FSilent: boolean;
    FSourceFileExtension: String;
    FSourceMask: String;
    FDestFileExtension: String;
    FDestPath: String;
  public
    constructor Create;

    procedure LoadParams;

    property Help                 : boolean read FHelp write FHelp;
    property SourceMask           : String read FSourceMask write FSourceMask;
    property SourceFileExtension  : String read FSourceFileExtension write FSourceFileExtension;
    property NoSourceFileExtension: boolean read FNoSourceFileExtension;

    property DestPath             : String read FDestPath write FDestPath;
    property DestFileExtension    : String read FDestFileExtension write FDestFileExtension;
    property NoDestFileExtension  : boolean read FNoDestFileExtension;
    property Silent               : boolean read FSilent;
  end;

implementation

uses SysUtils;

{ TOptions }

constructor TOptions.Create;
begin
  FHelp                     := false;
  FSourceMask               := '';
  FNoSourceFileExtension    := false;
  FDestFileExtension        := '';
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
  FSourceFileExtension   := '';
  FNoSourceFileExtension := false;

  FDestPath              := IncludeTrailingBackslash(GetCurrentDir);
  FDestFileExtension     := '';
  FNoDestFileExtension   := false;

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
                FSourceFileExtension   := sValue;
                FNoSourceFileExtension := FSourceFileExtension = '';
              end;
         'd': begin
                FDestPath := sValue;

                if copy(FDestPath, 1, 2) = '\\' then FDestPath := ExpandUNCFileName(FDestPath)
                 else FDestPath := ExpandFileName(FDestPath);
              end;
         'e': begin
                FDestFileExtension := sValue;
                FNoDestFileExtension := FDestFileExtension = '';
              end;
         's': FSilent := true;
         else begin
                FHelp := true;
                writeln(format('invalid param "%s"', [ParamStr(i)]));
              end;
      end;
    end
    else IsInvalidParam := true;

    if (FNoSourceFileExtension = false) and
       (FSourceFileExtension = '') then
    begin
      FSourceFileExtension := ExtractFileExt(FSourceMask);
    end
    else if (ExtractFileExt(FSourceMask) <> '') and
            (FSourceFileExtension <> '') and
            (ExtractFileExt(FSourceMask) <> FSourceFileExtension) then
    begin
      writeln(format('parameter conflict: different sourcefile extension "%s" and "%s"',
                     [ExtractFileExt(FSourceMask), FSourceFileExtension]));
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

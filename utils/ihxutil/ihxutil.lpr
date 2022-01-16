{ IHX (Intel Hex format) utility program

  This is the main program of the tool.

  Copyright (C) 2020 Nikolay Nikolov <nickysn@users.sourceforg.net>

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
  Boston, MA 02110-1335, USA.
}

program ihxutil;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, CustApp, ihxreader, tzxwriter, zxbasic
  { you can add units after this };

const
  ShortOptions = 'hb:c:t:';
  LongOptions: array [0..0] of string = (
    'help'
  );

type

  TOutputType = (
    otTZX,
    otBin
  );

  { TIHXUtil }

  TIHXUtil = class(TCustomApplication)
  private
    FInputFileName: string;
    FOutputFileName: string;
    FBasicProgramName: string;
    FBinaryProgramName: string;
    FInputImage: TIHXReader;
    FOutputFile: TStream;
    FOutputType: TOutputType;
    FTapeWriter: TTZXWriter;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TIHX2TZX }

procedure TIHXUtil.DoRun;
var
  ErrorMsg, t: String;
  NonOptions: TStringArray;
  BasicProgram: AnsiString;
begin
  if ParamCount = 0 then
  begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  // quick check parameters
  ErrorMsg:=CheckOptions(ShortOptions, LongOptions);
  if ErrorMsg<>'' then
    raise Exception.Create(ErrorMsg);

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if HasOption('b', '') then
    FBasicProgramName := GetOptionValue('b', '');
  if HasOption('c', '') then
    FBinaryProgramName := GetOptionValue('c', '');

  if HasOption('t', '') then begin
    t := GetOptionValue('t', '');
    case t of
      'tzx': FOutputType := otTZX;
      'bin': FOutputType := otBin;
      else
        raise Exception.CreateFmt('Invalid option for output type parameter: %s', [t]);
    end;
  end else
    FOutputType := otTZX;

  NonOptions := GetNonOptions(ShortOptions, LongOptions);
  if Length(NonOptions) = 0 then
    raise Exception.Create('Missing input file');
  if Length(NonOptions) > 2 then
    raise Exception.Create('Too many files specified');
  FInputFileName := NonOptions[0];
  if Length(NonOptions) >= 2 then
    FOutputFileName := NonOptions[1]
  else
    FOutputFileName := ChangeFileExt(FInputFileName, '.tzx');

  { add your program here }
  FInputImage.ReadIHXFile(FInputFileName);

  FOutputFile := TFileStream.Create(FOutputFileName, fmCreate);

  case FOutputType of
    otTZX: begin
      FTapeWriter := TTZXWriter.Create(FOutputFile);

      BasicProgram := BAS_EncodeLine(10, ' '+BC_LOAD+'"" '+BC_CODE) +
                      BAS_EncodeLine(20, ' '+BC_PRINT+BC_USR+BAS_EncodeNumber(FInputImage.Origin));

      FTapeWriter.AppendProgramFile(FBasicProgramName, 10, Length(BasicProgram), BasicProgram[1], Length(BasicProgram));
      FTapeWriter.AppendCodeFile(FBinaryProgramName, FInputImage.Origin, FInputImage.Data[0], Length(FInputImage.Data));
    end;
    otBin: begin
      FOutputFile.Write(FInputImage.Data[0], Length(FInputImage.Data));
    end;
  end;

  // stop program loop
  Terminate;
end;

constructor TIHXUtil.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  FInputImage := TIHXReader.Create;
  FBasicProgramName := 'basic';
  FBinaryProgramName := 'test';
end;

destructor TIHXUtil.Destroy;
begin
  FreeAndNil(FInputImage);
  FreeAndNil(FTapeWriter);
  FreeAndNil(FOutputFile);
  inherited Destroy;
end;

procedure TIHXUtil.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' [options] ihx_file [out_file]');
  Writeln('Options: -b <name>   specify the name of the BASIC loader program on the tape');
  Writeln('         -c <name>   specify the name of the machine code program on the tape');
  Writeln('         -t <type>   specify the output type; valid types are:');
  Writeln('                        tzx ZX Spectrum Tape file (default)');
  Writeln('                        bin Flat binary file (e.g. MSX-DOS COM)');
  Writeln('         -h          display this help');
  Writeln('         --help      display this help');
end;

var
  Application: TIHXUtil;
begin
  Application:=TIHXUtil.Create(nil);
  Application.Title:='ihxutil';
  Application.Run;
  Application.Free;
end.


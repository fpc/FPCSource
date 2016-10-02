{

  Copyright (C) <avx-testfile-generator> <Torsten Grundke>

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
  to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
  MA 02110-1301, USA.
}

{$mode objfpc}

unit options;

interface

type

  TOptions = class(TObject)
  private
    FHelp: boolean;
    FX64: boolean;
    FOutputFormat: Char;
    FPath: string;
  public
    constructor Create;

    procedure LoadParams;

    property Help: boolean read FHelp write FHelp;
    property OutputFormat: Char read FOutputFormat write FOutputFormat;
    property X64: boolean read FX64 write FX64;
    property Path: string read FPath write FPath;
  end;

implementation

uses SysUtils;

{ TOptions }

constructor TOptions.Create;
begin
  FHelp          := false;
  FX64           := false;
  FOutputFormat  := '?';
  FPath          := '';
end;

procedure TOptions.LoadParams;
var
  i: integer;
  sParam: Char;
  sValue: String;
  IsInvalidParam: boolean;
begin
  if ParamCount = 0 then FHelp := true
   else FHelp := false;

  FX64 := false;
  FOutputFormat := 'f'; // default = fpc
  FPath := IncludeTrailingBackslash(GetCurrentDir);

  for i := 1 to ParamCount do
  begin
    if copy(ParamStr(i), 1, 1) = '-' then
    begin
      sParam := copy(ParamStr(i) + '  ', 2, 1)[1];
      sValue := copy(ParamStr(i), 3, length(ParamStr(i)));

      IsInvalidParam := false;
      case sParam of
         'h': FHelp := true;
         'f': if sValue = 'fpc' then FOutputFormat := 'f'
               else if sValue = 'nasm' then FOutputFormat := 'n'
               else if sValue = 'fasm' then FOutputFormat := 'F'
               else IsInvalidParam := true;
         'p': if sValue = 'x8664' then
              begin
                Fx64 := true;
              end
              else IsInvalidParam := true;
         'o': if sValue <> '' then
              begin
                FPath :=  IncludeTrailingBackslash(sValue);
              end
              else
              begin
                FPath := '';
              end;
         else begin
                FHelp := true;
                writeln(format('invalid param "%s"', [ParamStr(i)]));
              end;
      end;
    end
    else IsInvalidParam := true;

    if IsInvalidParam then
    begin
      FHelp := true;
      writeln(format('invalid param "%s"', [ParamStr(i)]));
    end;
  end;
end;

end.

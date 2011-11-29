{
Ported to FPC by Nikolay Nikolov (nickysn@users.sourceforge.net)
}

{
 Modes example for OpenPTC 1.0 C++ implementation
 Copyright (c) Glenn Fiedler (ptc@gaffer.org)
 This source code is in the public domain
}

program ModesExample;

{$MODE objfpc}

uses
  ptc;

procedure print(format: IPTCFormat);
begin
  { check format type }
  if format.direct then
    { check alpha }
    if format.a = 0 then
      { direct color format without alpha }
      Write('Format(', format.bits:2, ',$', HexStr(format.r, 8), ',$', HexStr(format.g, 8), ',$', HexStr(format.b, 8), ')')
    else
      { direct color format with alpha }
      Write('Format(', format.bits:2, ',$', HexStr(format.r, 8), ',$', HexStr(format.g, 8), ',$', HexStr(format.b, 8), ',$', HexStr(format.a, 8), ')')
  else
    { indexed color format }
    Write('Format(', format.bits:2, ')');
end;

procedure print(mode: IPTCMode);
begin
  { print mode width and height }
  Write(' ', mode.width:4, ' x ', mode.height);
  if mode.height < 1000 then
    Write(' ');
  if mode.height < 100 then
    Write(' ');
  if mode.height < 10 then
    Write(' ');
  Write(' x ');

  { print mode format }
  print(mode.format);

  { newline }
  Writeln;
end;

var
  console: IPTCConsole;
  modes: TPTCModeList;
  index: Integer;
begin
  try
    { create console }
    console := TPTCConsoleFactory.CreateNew;

    { get list of console modes }
    modes := console.modes;

    { check for empty list }
    if Length(modes) = 0 then
      { the console mode list was empty }
      Writeln('[console mode list is not available]')
    else
    begin
      { print mode list header }
      Writeln('[console modes]');

      { iterate through all modes }
      for index := Low(modes) to High(modes) do
      begin
        { print mode }
        print(modes[index]);
      end;
    end;
  except
    on error: TPTCError do
      { report error }
      error.report;
  end;
end.

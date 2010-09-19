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

procedure print(const format: TPTCFormat);
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

procedure print(const mode: TPTCMode);
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
  console: TPTCConsole = nil;
  modes: PPTCMode;
  index: Integer;
begin
  try
    try
      { create console }
      console := TPTCConsole.Create;

      { get list of console modes }
      modes := console.modes;

      { check for empty list }
      if not modes[0].valid then
        { the console mode list was empty }
        Writeln('[console mode list is not available]')
      else
      begin
        { print mode list header }
        Writeln('[console modes]');

        { mode index }
        index := 0;

        { iterate through all modes }
        while modes[index].valid do
        begin
          { print mode }
          print(modes[index]);

          { next mode }
          Inc(index);
        end;
      end;
    finally
      console.Free;
    end;
  except
    on error: TPTCError do
      { report error }
      error.report;
  end;
end.

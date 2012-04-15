{
Ported to FPC by Nikolay Nikolov (nickysn@users.sourceforge.net)
}

{
 Info example for OpenPTC 1.0 C++ implementation
 Copyright (c) Glenn Fiedler (ptc@gaffer.org)
 This source code is in the public domain
}

program InfoExample;

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

var
  console: IPTCConsole;
begin
  try
    try
      Writeln('[ptcpas version]');
      { print ptcpas version string define }
      Writeln(PTCPAS_VERSION);
      Writeln;

      { create console }
      console := TPTCConsoleFactory.CreateNew;

      { open the console }
      console.open('Info example');

      { print console data }
      Writeln('[console data]');
      Writeln('name   = ', console.name);
      Writeln('title  = ', console.title);
      Writeln('width  = ', console.width);
      Writeln('height = ', console.height);
      Writeln('pages  = ', console.pages);
      Writeln('pitch  = ', console.pitch);
      Write('format = ');
      print(console.format);
      Writeln;
      Writeln;

      { print console information }
      Writeln('[console information]');
      Writeln(console.information);
    finally
      if Assigned(console) then
        console.close;
    end;
  except
    on error: TPTCError do
      { report error }
      error.report;
  end;
end.

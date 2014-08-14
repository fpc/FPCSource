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
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}

{$mode objfpc}

program AVXTestGenerator;
uses
  sysutils,
  AVXOpCodes in 'AVXOpCodes.pas',
  AsmTestGenerator in 'AsmTestGenerator.pas',
  Options in 'Options.pas';

begin
  with TAVXTestGenerator.Create do
  try
    with TOptions.Create do
    try
      LoadParams;

      if Help then
      begin
        writeln('avx-testfile-generator 0.1');
        writeln('author: torsten grundke');
        writeln('');
        writeln('make avx assembler-testfiles');
        writeln('');
        writeln('-h  help');
        writeln('-f  [fpc,nasm,fasm] outputformat');
        writeln('-p  [x8664] codegenerator for x86_64 platform');
        writeln('-o  destination path');
        writeln('');
      end
      else
      begin
        case OutputFormat of
          'f': MakeTestFiles(tfFPC, x64, Path);
          'F': MakeTestFiles(tfFasm, x64, Path);
          'n': MakeTestFiles(tfNasm, x64, Path);
        end;
      end;
    finally
      Free;
    end;
  finally
    Free;
  end;
end.

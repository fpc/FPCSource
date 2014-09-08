program avxtestfilecmp;
{$mode objfpc}
uses
  sysutils,
  filecomparer in 'filecomparer.pas',
  cmpoptions in 'cmpoptions.pas';

var
  ch: Char;
  sm: String;
begin
  with TFileComparer.Create do
  try
    with TOptions.Create do
    try
      LoadParams;

      if Help then
      begin
        writeln('avx-testfile-generator 0.1');
        writeln('author: torsten grundke');
        writeln('');
        writeln('compare avx-assembler-testfiles');
        writeln('');
        writeln('-h  help');
        writeln('-m  sourcefile mask');
        writeln('-n  sourcefile extension');
        writeln('-d  destination path');
        writeln('-e  destinationfile extension');
        writeln('-s  silent');
        writeln('');
        {$IFDEF WINDOWS}
        writeln('examples:  -mc:\tmp\*.obj -dc:\tmp\avx\ -eexe');
        writeln('           -m/tmp/* -n -d/tmp/avx/ -e');
        {$ELSE}
        writeln('examples:  -m/tmp/*.obj -d/tmp/avx/ -ebin');
        writeln('           -m/tmp/* -n -d/tmp/avx/ -e');
        {$ENDIF}
        writeln;
      end
      else
      begin
        sm := SourceMask;

        if ExtractFileExt(sm) = '' then
        begin
          if trim(SourceFileExtension) <> '' then
          begin
            if copy(SourceFileExtension, 1, 1) <> '.' then sm := sm + '.' + SourceFileExtension
             else sm := sm + SourceFileExtension;
          end;
        end;

        if (ExtractFilePath(sm) = DestPath) and
           (DestFileExtension = '') then
        begin
          writeln(format('Do you want compare the same files (sourcepath: "%s"  destination path: "%s"). [Y/N]',
                         [ExtractFilePath(sm), DestPath]));

          read(ch);
          if ch in ['Y', 'y', 'J', 'N'] then CompareFiles(NoSourceFileExtension, NoDestFileExtension, Silent,
                                                          sm, DestPath, DestFileExtension);
        end
        else CompareFiles(NoSourceFileExtension, NoDestFileExtension, Silent,
                          sm, DestPath, DestFileExtension);
      end;
    finally
      Free;
    end;
  finally
    Free;
  end;

end.

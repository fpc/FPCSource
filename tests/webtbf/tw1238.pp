{ %FAIL }
{ Source provided for Free Pascal Bug Report 1238 }
{ Submitted by "Mazen NEIFER" on  2000-11-14 }
{ e-mail: mazen_neifer@ayna.com }
PROGRAM Concat;
VAR
  InputFile,OutputFile:File;
  c:Char;
  Buffer:Array[PtrInt]OF Byte;
  ReadCount,WriteCount:DWord;
BEGIN
  Assign(OutputFile,'Maple.tar.gz');
  ReWrite(OutputFile,1);
  FOR c:='a' TO 'n' DO
    BEGIN
      Assign(InputFile,'xa'+c);
      Reset(InputFile,1);
      BlockRead(InputFile,Buffer,SizeOf(Buffer),ReadCount);
      BlockWrite(OutputFile,Buffer,SizeOf(Buffer),WriteCount);
      Close(InputFile);
    END;
  Close(OutputFile);
END.

USES
  FVCommon,FileIO;

VAR
  Handle : THandle;
  buf    : ARRAY[0..255] OF CHAR;
  n      : LongWord;
BEGIN
  Handle := FileOpen(AsciiZ('test'), fa_Create);
  writeln('FileOpen: ',Handle);

  buf := 'Test'#0;
  writeln('FileWrite: ', FileWrite(handle, Buf, 5, n));
  writeln('Bytes written: ', n);

  Writeln('SetFileSize: ', SetFileSize(handle, 4));

  Writeln('SetFilePos: ', SetFilePos(handle, 2, 0, (LongInt(n))));
  Writeln('Actual: ', n);

  Writeln('FileRead: ', FileRead(Handle, buf, 2, n) );
  Writeln('Actual: ', n);

  Writeln('Buf[0]=', Buf[0], ' Buf[1]=', Buf[1]);

  Writeln('FileClose: ', FileClose(Handle));
END.



{ %target=win32,win64 }
Program StrRedir;
uses Classes, Process, Sysutils;
const MaxByte = 255;
type
      TStrBuf = packed record {As a way to read buffers into strings}
      case Boolean of
         true: (  size: Byte;
                  buf:  array[0..MaxByte] of Char;
               );
         false:(  txt:  ShortString;  );
      end;

var
   MoreProcess: TProcess;
   readCount:   integer;
   strBuf:      TStrBuf;

begin
   MoreProcess := TProcess.Create(nil);
   MoreProcess.CommandLine := GetEnvironmentVariable('WINDIR')+'\system32\more.com';
   MoreProcess.Options := [poUsePipes];
   MoreProcess.Execute;
   strBuf.txt := 'Anton';
   MoreProcess.Input.Write(strBuf.buf, strBuf.size);
   MoreProcess.CloseInput();
   writeLn('Waiting...');    //This never ends
   while MoreProcess.Running do
   begin
      Sleep(50);
      //strBuf.size := MoreProcess.Output.Read(strBuf.buf, 255);
   end;
   writeLn('Wait finished.');
   Sleep(100);
   strBuf.size := MoreProcess.Output.Read(strBuf.buf, 255);
   write(strBuf.txt);
   writeLn('------');
end.

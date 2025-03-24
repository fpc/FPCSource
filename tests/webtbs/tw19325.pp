{ %target=win32,win64 }
Program StrRedir;
uses Classes, Process, Sysutils;
const MaxByte = 255;
const MaxCount = 100;

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
   loopCount:   integer;
   strBuf:      TStrBuf;
   strBuf2:     TStrBuf;
begin
   loopCount:=0;
   MoreProcess := TProcess.Create(nil);
   MoreProcess.CommandLine := GetEnvironmentVariable('WINDIR')+'\system32\more.com';
   MoreProcess.Options := [poUsePipes];
   MoreProcess.Execute;
   strBuf.txt := 'Anton';
   MoreProcess.Input.Write(strBuf.buf, strBuf.size);
   MoreProcess.CloseInput();
   writeLn('Waiting...');    //This never ends
   while MoreProcess.Running and (loopCount<MaxCount) do
   begin
      Sleep(50);
      inc(loopCount);
      //strBuf.size := MoreProcess.Output.Read(strBuf.buf, 255);
   end;
   if not MoreProcess.Running then
     writeLn('Wait finished.')
   else
     begin
       MoreProcess.Terminate(1);
     end;
   Sleep(100);
   strBuf2.size := MoreProcess.Output.Read(strBuf2.buf, 255);
   write(strBuf.txt);
   if (strBuf.txt <> strBuf2.txt) or (loopCount=MaxCount)
      or (MoreProcess.ExitCode<>0) then
     begin
       writeln('Test about inheritable pipe on Windows OS fails');
       halt(1);
     end;
   writeLn('------');
end.

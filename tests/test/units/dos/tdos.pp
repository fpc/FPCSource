{ %INTERACTIVE }
{
  $Id$

  Program to test DOS unit by Peter Vreman.
  Only main TP functions are tested (nothing with Interrupts/Break/Verify).
}
{$V-}
program tesidos;
uses dos;

procedure TestInfo;
var
  dt    : DateTime;
  ptime : longint;
  wday  : word;
  HSecs : word;
begin
  writeln;
  writeln('Info Functions');
  writeln('**************');
  writeln('Dosversion     : ',lo(DosVersion),'.',hi(DosVersion));
  GetDate(Dt.Year,Dt.Month,Dt.Day,wday);
  writeln('Current Date   : ',Dt.Month,'-',Dt.Day,'-',Dt.Year,' weekday ',wday);
  GetTime(Dt.Hour,Dt.Min,Dt.Sec,HSecs);
  writeln('Current Time   : ',Dt.Hour,':',Dt.Min,':',Dt.Sec,' hsecs ',HSecs);
  PackTime(Dt,ptime);
  writeln('Packed like dos: ',ptime);
  UnpackTime(ptime,DT);
  writeln('Unpacked again : ',Dt.Month,'-',Dt.Day,'-',Dt.Year,'  ',Dt.Hour,':',Dt.Min,':',Dt.Sec);
  writeln;
  write('Press Enter');
  Readln;
end;


procedure TestEnvironment;
var
  i : longint;
begin
  writeln;
  writeln('Environment Functions');
  writeln('*********************');
  writeln('Amount of environment strings : ',EnvCount);
  writeln('GetEnv TERM : ',GetEnv('TERM'));
  writeln('GetEnv HOST : ',GetEnv('HOST'));
  writeln('GetEnv PATH : ',GetEnv('PATH'));
  writeln('GetEnv SHELL: ',GetEnv('SHELL'));
  write('Press Enter for all Environment Strings using EnvStr()');
  Readln;
  for i:=1 to EnvCount do
   writeln(EnvStr(i));
  write('Press Enter');
  Readln;
end;


procedure TestExec;
begin
  writeln;
  writeln('Exec Functions');
  writeln('**************');
  write('Press Enter for an Exec of ''ls -la''');
  Readln;
{$ifdef unix }
  Exec('ls','-la');
{$else not unix }
  SwapVectors;
  Exec('ls','-la');
  SwapVectors;
{$endif not unix }
  write('Press Enter');
  Readln;
end;



procedure TestDisk;
var
  Dir : SearchRec;
  DT  : DateTime;
begin
  writeln;
  writeln('Disk Functions');
  writeln('**************');
  writeln('DiskFree 0 : ',DiskFree(0));
  writeln('DiskSize 0 : ',DiskSize(0));
  {writeln('DiskSize 1 : ',DiskSize(1)); this is a: on dos  ??! }
  writeln('DiskSize 1 : ',DiskSize(3)); { this is c: on dos }
{$IFDEF LINUX}
  AddDisk('/fd0');
  writeln('DiskSize 4 : ',DiskSize(4));
{$ENDIF}
  write('Press Enter for FindFirst/FindNext Test');
  Readln;

  FindFirst('*.*',$20,Dir);
  while (DosError=0) do
   begin
     UnpackTime(dir.Time,DT);
     Writeln(dir.Name,' ',dir.Size,' ',DT.Year,'-',DT.Month,'-',DT.Day);
     FindNext(Dir);
   end;
  write('Press Enter');
  Readln;
end;



procedure TestFile;
var
  test,
  name,dir,ext : string;
begin
  writeln;
  writeln('File(name) Functions');
  writeln('********************');
{$ifdef linux }
  test:='/usr/local/bin/ppc.so';
  writeln('FSplit(',test,')');
  FSplit(test,dir,name,ext);
  writeln('dir: ',dir,' name: ',name,' ext: ',ext);
  test:='/usr/bin.1/ppc';
  writeln('FSplit(',test,')');
  FSplit(test,dir,name,ext);
  writeln('dir: ',dir,' name: ',name,' ext: ',ext);
  test:='mtools.tar.gz';
  writeln('FSplit(',test,')');
  FSplit(test,dir,name,ext);
  writeln('dir: ',dir,' name: ',name,' ext: ',ext);

  Writeln('Expanded dos.pp                 : ',FExpand('dos.pp'));
  Writeln('Expanded ../dos.pp              : ',FExpand('../dos.pp'));
  Writeln('Expanded /usr/local/dos.pp      : ',FExpand('/usr/local/dos.pp'));
  Writeln('Expanded ../dos/./../././dos.pp : ',FExpand('../dos/./../././dos.pp'));

  test:='../;/usr/;/usr/bin/;/usr/bin;/bin/;';
{$else not linux }
  test:='\usr\local\bin\ppc.so';
  writeln('FSplit(',test,')');
  FSplit(test,dir,name,ext);
  writeln('dir: ',dir,' name: ',name,' ext: ',ext);
  test:='\usr\bin.1\ppc';
  writeln('FSplit(',test,')');
  FSplit(test,dir,name,ext);
  writeln('dir: ',dir,' name: ',name,' ext: ',ext);
  test:='mtools.tar.gz';
  writeln('FSplit(',test,')');
  FSplit(test,dir,name,ext);
  writeln('dir: ',dir,' name: ',name,' ext: ',ext);

  Writeln('Expanded dos.pp                 : ',FExpand('dos.pp'));
  Writeln('Expanded ..\dos.pp              : ',FExpand('..\dos.pp'));
  Writeln('Expanded \usr\local\dos.pp      : ',FExpand('\usr\local\dos.pp'));
  Writeln('Expanded ..\dos\.\..\.\.\dos.pp : ',FExpand('..\dos\.\..\.\.\dos.pp'));

  test:='..\;\usr\;\usr\bin\;\usr\bin;\bin\;';
{$endif not linux}
  test:=test+getenv('PATH');
{$ifdef unix}
  Writeln('FSearch ls: ',FSearch('ls',test));
{$else not unix}
  Writeln('FSearch ls: ',FSearch('ls.exe',test));
{$endif not unix}

  write('Press Enter');
  Readln;
end;



begin
  TestInfo;
  TestEnvironment;
  TestExec;
  TestDisk;
  TestFile;
end.


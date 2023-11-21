{$IFDEF HASAMIGA}
 {$DEFINE NO_UNIT_PROCESS}
{$ENDIF}

{$IFDEF OS2}
 {$DEFINE NO_UNIT_PROCESS}
{$ENDIF OS2}

{$IFDEF GO32V2}
 {$DEFINE NO_UNIT_PROCESS}
{$ENDIF GO32V2}

{$ifndef NO_UNIT_PROCESS}
  {$define HAS_UNIT_PROCESS}
{$endif NO_UNIT_PROCESS}

{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses
{$ifdef unix}
  cthreads,
{$endif}
  fpmkunit,
{$IFDEF HAS_UNIT_PROCESS}
  process,
{$ENDIF HAS_UNIT_PROCESS}
  sysutils;
{$endif ALLPACKAGES}

{$ifdef HAS_UNIT_PROCESS}
procedure fpcm_update_revision_info(Sender: TObject);

  function ReadGitLine(AProcess: TProcess; var ALine: string): boolean;
  var
    b,i: byte;
  begin
    result := true;
    ALine := '';
    i := 1;
    repeat
    if i = 0 then
      sleep(100);
    i := AProcess.Output.Read(b,1);
    if i > 0 then
      begin
        if b = 13 then
          continue;
        if b = 10 then
          exit;
        ALine := ALine + chr(b);
      end;
    until not AProcess.Running and (i=0);

    result := (ALine <> '');
  end;

var
  P : TPackage;
  GitBin : String;
  GitProcess: TProcess;
  f: text;
  fileurl, line, date, lastdate,
  hash, lasthash, oldhash,
  oldhashstring, olddate : string;
  i, io : longint;

begin
  // If revision.inc does exist, try to update the file with the latest
  // commit from git. And include this information in the fpcmake
  // executable.
  With installer do
    begin
      if not FileExists(BuildEngine.AddPathPrefix(P,'revision.inc')) then
        begin
          BuildEngine.Log(vlWarning, 'File revision.inc not found. Svn-revision will not be included in fpcmake executable.');
          Exit;
        end;

      // Run svn info, and catch output.
      P := sender as TPackage;
      P.Options.Add('-dREVINC');
      GitBin := ExeSearch(AddProgramExtension('git', Defaults.BuildOS), GetEnvironmentvariable('PATH'));
      if GitBin<>'' then
        begin
          GitProcess := TProcess.create(nil);
          try
            GitProcess.Executable := GitBin;
            GitProcess.Parameters.Add('log');
            GitProcess.Parameters.Add('-n');
            GitProcess.Parameters.Add('1');
            GitProcess.Parameters.Add('--date=short');
            GitProcess.Parameters.Add('--pretty=%cd');
            GitProcess.Parameters.Add(BuildEngine.AddPathPrefix(P,'fpcmpkg.pp'));
            GitProcess.Parameters.Add(BuildEngine.AddPathPrefix(P,'fpcmake.pp'));
            GitProcess.Parameters.Add(BuildEngine.AddPathPrefix(P,'fpcmwr.pp'));
            GitProcess.Parameters.Add(BuildEngine.AddPathPrefix(P,'fpcmmain.pp'));
            GitProcess.Parameters.Add(BuildEngine.AddPathPrefix(P,'fpcmdic.pp'));
            GitProcess.Parameters.Add(BuildEngine.AddPathPrefix(P,'fpcmake.ini'));
            GitProcess.Parameters.Add(BuildEngine.AddPathPrefix(P,'Makefile.fpc'));            GitProcess.Options:=[poUsePipes];
            GitProcess.Execute;

            // Search for latest revision in output:
            lastdate:='0';
            ReadGitLine(GitProcess, lastdate);
          finally
            GitProcess.Free;
          end;

          GitProcess := TProcess.create(nil);
          try
            GitProcess.Executable := GitBin;
            GitProcess.Parameters.Add('log');
            GitProcess.Parameters.Add('-n');
            GitProcess.Parameters.Add('1');
            GitProcess.Parameters.Add('--pretty=%h');
            GitProcess.Parameters.Add(BuildEngine.AddPathPrefix(P,'fpcmpkg.pp'));
            GitProcess.Parameters.Add(BuildEngine.AddPathPrefix(P,'fpcmake.pp'));
            GitProcess.Parameters.Add(BuildEngine.AddPathPrefix(P,'fpcmwr.pp'));
            GitProcess.Parameters.Add(BuildEngine.AddPathPrefix(P,'fpcmmain.pp'));
            GitProcess.Parameters.Add(BuildEngine.AddPathPrefix(P,'fpcmdic.pp'));
            GitProcess.Parameters.Add(BuildEngine.AddPathPrefix(P,'fpcmake.ini'));
            GitProcess.Parameters.Add(BuildEngine.AddPathPrefix(P,'Makefile.fpc'));            GitProcess.Options:=[poUsePipes];
            GitProcess.Options:=[poUsePipes];
            GitProcess.Execute;

            lasthash:='0';
            ReadGitLine(GitProcess, lasthash);
          finally
            GitProcess.Free;
          end;

          oldhash:='';
          olddate:='';
          // Write the latest change-date and revision to file revision.inc
          system.assign(f,BuildEngine.AddPathPrefix(P,'revision.inc'));
          io:=ioresult;
          reset(f);
          io:=ioresult;
          if io<>0 then
            begin
              BuildEngine.Log(vlWarning,'revision.inc reset failed, io='+IntToStr(io));
            end
          else
            begin
              readln(f,oldhashstring);
              close(f);
              BuildEngine.Log(vlCommand, 'oldhashstring '+oldhashstring);
              if oldhashstring[1]='''' then
                oldhashstring:=copy(oldhashstring,2,length(oldhashstring));
              i:=length(oldhashstring);
              if oldhashstring[i]='''' then
                oldhashstring:=copy(oldhashstring,1,i-1);
              i:=pos(' hash ',oldhashstring);
              if i>0 then
                begin
                  oldhash:=copy(oldhashstring,i+6,length(oldhashstring));
                  olddate:=copy(oldhashstring,1,i-1);
                  BuildEngine.Log(vlCommand,'Old values '+olddate+' '+oldhash);
                  if (olddate >= lastdate) and (oldhash <> lasthash) then
                    begin
                      BuildEngine.Log(vlCommand,'New values '+lastdate+' '+lasthash);
                      BuildEngine.Log(vlCommand,'Keeping old values');
                      lasthash:=oldhash;
                      lastdate:=olddate;
                    end;
                end;

            end;
          if (lastdate=olddate) and (lasthash=oldhash) then
            BuildEngine.Log(vlCommand,'revision.inc unchanged')
           else
            begin
              BuildEngine.Log(vlCommand,'revision.inc set to '''+lastdate+' hash '+lasthash+'''');

              system.assign(f,BuildEngine.AddPathPrefix(P,'revision.inc'));
              rewrite(f);
              io:=ioresult;
              if io <> 0 then
                begin
                  BuildEngine.Log(vlError, 'Error opening revision.inc for writing');
                  halt(3);
                end;
              Writeln(f,'''',lastdate,' hash ',lasthash,'''');
              close(f);
            end
        end
      else
        BuildEngine.Log(vlWarning,'Git executable (git) not found. Git-hash in fpcmake executable might be out of date.');
    end;
end;
{$endif HAS_UNIT_PROCESS}

procedure add_fpcm(const ADirectory: string);

Var
  P : TPackage;
  T : TTarget;
  Data2IncBin : String;

begin
  With Installer do
    begin
    P:=AddPackage('utils-fpcm');
    P.ShortName:='fpcm';
    P.OSes:=AllOSes-[embedded,msdos,nativent,win16,macosclassic,atari,palmos,zxspectrum,msxdos,amstradcpc,sinclairql,human68k];
    if Defaults.CPU=jvm then
      P.OSes := P.OSes - [java,android];

    P.Author := '<various>';
    P.License := 'LGPL with modification';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'Tool to generate Makefile''s out of Makefile.fpc files';
    P.NeedLibC:= false;

{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.3.1';

    P.Dependencies.Add('fcl-base');
{$ifdef TEST_FPMKUNIT}
    P.Dependencies.Add('fpmkunit');
{$endif}

    T:=P.Targets.AddProgram('fpcmake.pp');

{$ifdef HAS_UNIT_PROCESS}
    P.BeforeCompileProc:=@fpcm_update_revision_info;
{$else HAS_UNIT_PROCESS}
    writeln('Process-unit not available. Svn-revision in fpmake executable might be out-of-date.');
{$endif HAS_UNIT_PROCESS}

    Data2IncBin := AddProgramExtension('data2inc',Defaults.BuildOS);
    p.Commands.AddCommand(caBeforeCompile, Data2IncBin, '-b -s ' + P.Directory + 'fpcmake.ini ' + P.Directory + 'fpcmake.inc fpcmakeini','fpcmake.inc','fpcmake.ini');
    T:=P.Targets.AddUnit('fpcmmain.pp');
    T.install:=false;
    T.ResourceStrings:=true;
    P.Targets.AddUnit('fpcmdic.pp').install:=false;
    P.Targets.AddUnit('fpcmwr.pp').install:=false;
    P.Targets.AddUnit('fpcmpkg.pp').install:=false;

    // P.Sources.AddSrc('fpcmake.ini');
    P.Sources.AddSrc('fpcmake.inc');
    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_fpcm('');
  Installer.Run;
end.
{$endif ALLPACKAGES}





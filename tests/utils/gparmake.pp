{ See procedure "Usage". This code is in the public domain. }

Program GParMake;

Uses
  Classes;

procedure Usage;
  begin
    writeln('GParMake: create make rules for parallel execution of testsuite');
    writeln('Usage: gparmake [-a] [-f] <outputfile> <dirname> <startchunk> <tests_per_chunk> <test1> [<test2> ...]');
    writeln('Output: makefile fragment with rules to run the tests in sequences of <tests_per_chunk>');
    writeln('  -a: Append to existing files');
    writeln('  -f: redirect output to separate files');
    writeln;
    halt(1);
  end;

var
  doappend: boolean;
  doredirect: boolean;

{ make all numbers of the same string length so they can be sorted
  lexographically }
function rulenr2str(rulenr: longint): string;
  var
    i: longint;
  begin
    str(rulenr:9,rulenr2str);
    for i:=1 to length(rulenr2str)-1 do
      if rulenr2str[i]=' ' then
        rulenr2str[i]:='0';
  end;

procedure WriteChunkRule(rulenr: longint; const dirname, files: ansistring);
  var
    rulestr, redirectfile: string;
  begin
    rulestr:=rulenr2str(rulenr)+dirname;
    writeln('$(TEST_OUTPUTDIR)/testchunk_',rulestr,'-stamp.$(TEST_FULL_TARGET): testprep-stamp.$(TEST_FULL_TARGET)');
    write(#9'$(Q)$(DOTEST) $(DOTESTOPT) -Lchunk',rulestr,' -e ',files);
    if doredirect then
      begin
        redirectfile:='$(TEST_OUTPUTDIR)/sep_log.chunk'+rulestr;
	writeln(' > '+redirectfile);
      end
    else
      writeln;
    writeln(#9'$(ECHOREDIR) $(TEST_DATETIME) > $@');
    writeln;
    writeln('$(addsuffix .chunk',rulestr,', $(LOGFILES)) : $(TEST_OUTPUTDIR)/testchunk_',rulestr,'-stamp.$(TEST_FULL_TARGET)');
    writeln;
    writeln('.INTERMEDIATE: $(addsuffix .chunk',rulestr,', $(LOGFILES)) $(TEST_OUTPUTDIR)/testchunk_',rulestr,'-stamp.$(TEST_FULL_TARGET)');
    writeln;
  end;


var
  startchunk: longint;
  dirname : ansistring;
  FileList : TStringList;

Function ProcessArgs: longint;
  var
    i,
    paramnr,
    chunktargetsize,
    chunksize,
    chunknr,
    nextfileindex,
    error: longint;
    testname,
    nexttestname,
    testlist,
    s,
    outputname: ansistring;
    filelist : array of ansistring;
    responsefile : text;

  procedure AddFile(const s : ansistring);
    begin
      if nextfileindex>high(filelist) then
        SetLength(filelist,length(filelist)+128);
      filelist[nextfileindex]:=s;
      inc(nextfileindex);
    end;

  procedure FlushChunk;
    begin
      WriteChunkRule(chunknr,dirname,testlist);
      inc(chunknr);
      testlist:='';
      chunksize:=0;
    end;

  begin
    if paramcount < 3 then
      Usage;

    doappend:=false;
    doredirect:=false;

    paramnr:=1;
    if paramstr(paramnr)='-a' then
      begin
        doappend:=true;
        inc(paramnr);
      end;

    if paramstr(paramnr)='-f' then
      begin
        doredirect:=true;
        inc(paramnr);
      end;

    outputname:=paramstr(paramnr);
    inc(paramnr);

    dirname:=paramstr(paramnr);
    inc(paramnr);

    val(paramstr(paramnr),startchunk,error);
    if error<>0 then
      Usage;
    inc(paramnr);

    val(paramstr(paramnr),chunktargetsize,error);
    if error<>0 then
      Usage;
    inc(paramnr);

    { only redirect output after all possible cases where we may have to write
      the usage screen }
    assign(output,outputname);
    if doappend then
      append(output)
    else
      rewrite(output);

    chunknr:=startchunk;
    chunksize:=0;
    testlist:='';
    nextfileindex:=0;
    for i := paramnr to paramcount do
      begin
        if paramstr(i)[1]='@' then
          begin
            assign(responsefile,copy(paramstr(i),2,length(paramstr(i))));
            reset(responsefile);
            while not(eof(responsefile)) do
              begin
                readln(responsefile,s);
                AddFile(s);
              end;
            close(responsefile);
          end
        else
          AddFile(paramstr(i));
      end;

    for i := 0 to nextfileindex-1 do
      begin
        testname:=filelist[i];
        testlist:=testlist+' '+testname;
        inc(chunksize);
        if chunksize>=chunktargetsize then
          begin
            if (i=nextfileindex-1) then
              FlushChunk
            else
              begin
                { keep tests with the same name except for the last character in the same chunk,
                  because they may have to be executed in order (skip ".pp" suffix and last char) }
                if i+1>=nextfileindex then
                  nexttestname:=''
                else
                  nexttestname:=filelist[i+1];
                if lowercase(copy(testname,1,length(testname)-4))<>lowercase(copy(nexttestname,1,length(nexttestname)-4)) then
                  FlushChunk;
              end;
          end;
      end;
    if chunksize<>0 then
      FlushChunk;
    ProcessArgs:=chunknr-1;
  end;


procedure WriteWrapperRules(totalchunks: longint);
  const
    lognames: array[1..4] of string[11] = ('log','faillist','longlog','sep_log');
  var
    logi,
    i: longint;
  begin
    for logi:=low(lognames) to high(lognames) do
      begin
        if (logi=4) and not doredirect then
          continue;
        write('$(TEST_OUTPUTDIR)/',lognames[logi],' :');
        for i:=startchunk to totalchunks do
          write(' $(TEST_OUTPUTDIR)/',lognames[logi],'.chunk',rulenr2str(i)+dirname);
        writeln;
        { if you have multiple rules for one (non-pattern) target, all
          prerequisites will be merged, but only one of the rules can have a
          recipe }
        if not doappend then
          begin
            writeln(#9'$(Q)$(CONCAT) $(sort $^) $@');
            writeln;
          end;
        writeln;
      end;
    if not doappend then
      begin
        writeln('gparmake_allexectests : $(LOGFILES)');
        writeln;
      end;
  end;


var
  totalchunks: longint;
begin
  totalchunks:=ProcessArgs;
  WriteWrapperRules(totalchunks);
  close(output);
end.

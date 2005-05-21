{
    This file is part of the Free Pascal test suite.
    Copyright (c) 2002 by the Free Pascal development team.

    This program generates a digest
    of the last tests run.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

program digest;

uses
  teststr;


const
  failed_to_compile_count : longint = 0;
  success_compilation_failed_count : longint = 0;
  failed_compilation_successful_count : longint = 0;
  successfully_compiled_count : longint = 0;
  failed_to_run_count : longint = 0;
  known_run_problem : longint = 0;
  successfully_run_count : longint = 0;
  skipping_graph_test_count : longint = 0;
  skipping_interactive_test_count : longint = 0;
  skipping_known_bug_count : longint = 0;
  skipping_other_version_count : longint = 0;
  skipping_other_cpu_count : longint = 0;
  skipping_other_target_count : longint = 0;
  skipping_run_unit_count : longint = 0;
  skipping_run_test_count : longint = 0;
  unknown_lines : longint = 0;
  unexpected_run : longint = 0;
  next_should_be_run : boolean = false;

var
  prevline : string;

procedure analyse (const st : string);
var
  should_be_run : boolean;
begin
  if st=prevline then
    exit;
  prevline:=st;
  should_be_run:=next_should_be_run;
  if next_should_be_run and
     (pos(failed_to_run,st)<>1) and
     (pos(successfully_run,st)<>1) and
     (pos(skipping_known_bug,st)<>1) and
     (pos(skipping_run_test,st)<>1) and
     (pos(skipping_run_unit,st)<>1) then
    begin
      Writeln('No run found for "',prevline,'"');
    end;
  next_should_be_run:=false;
  if pos(failed_to_compile,st)=1 then
    begin
      inc(failed_to_compile_count);
    end
  else if pos(success_compilation_failed,st)=1 then
    begin
      inc(success_compilation_failed_count);
    end
  else if pos(failed_compilation_successful,st)=1 then
    begin
      inc(failed_compilation_successful_count);
    end
  else if pos(successfully_compiled,st)=1 then
    begin
      inc(successfully_compiled_count);
      next_should_be_run:=true;
    end
  else if (pos(failed_to_run,st)=1) then
    begin
      inc(failed_to_run_count);
      if not should_be_run then
        inc(unexpected_run);
      if pos(known_problem,st)>0 then
        inc(known_run_problem);
    end
  else if pos(successfully_run,st)=1 then
    begin
      inc(successfully_run_count);
      if not should_be_run then
        inc(unexpected_run);
    end
  else if pos(skipping_graph_test,st)=1 then
    begin
      inc(skipping_graph_test_count);
    end
  else if pos(skipping_interactive_test,st)=1 then
    begin
      inc(skipping_interactive_test_count);
    end
  else if pos(skipping_known_bug,st)=1 then
    begin
      inc(skipping_known_bug_count);
    end
  else if pos(skipping_compiler_version_too_low,st)=1 then
    begin
      inc(skipping_other_version_count);
    end
  else if pos(skipping_compiler_version_too_high,st)=1 then
    begin
      inc(skipping_other_version_count);
    end
  else if pos(skipping_other_cpu,st)=1 then
    begin
      inc(skipping_other_cpu_count);
    end
  else if pos(skipping_other_target,st)=1 then
    begin
      inc(skipping_other_target_count);
    end
  else if pos(skipping_run_unit,st)=1 then
    begin
      inc(skipping_run_unit_count);
    end
  else if pos(skipping_run_test,st)=1 then
    begin
      inc(skipping_run_test_count);
    end
  else
    begin
      Writeln('Unknown line "',st,'"');
      inc(unknown_lines);
    end;
end;

procedure display_results;
var
  number_compilations : longint;
  number_skipped : longint;
  number_runs : longint;
  all_errors : longint;
  all_success : longint;

begin
  all_errors:=failed_to_compile_count
    +failed_compilation_successful_count
    +failed_to_run_count;
  all_success:=success_compilation_failed_count
    +successfully_compiled_count
    +successfully_run_count;
  { about compilations }
  number_compilations:=failed_to_compile_count
    +success_compilation_failed_count
    +failed_compilation_successful_count
    +successfully_compiled_count;
  { about runs }
  number_runs:=failed_to_run_count+successfully_run_count;

  Writeln('Total = ',number_compilations+number_runs,' (',
    all_errors,':',
    all_success,')');

  Writeln('Total number of compilations = ', number_compilations,' (',
    failed_to_compile_count+failed_compilation_successful_count,':',
    successfully_compiled_count+success_compilation_failed_count,')');
  Writeln('Successfully compiled = ', successfully_compiled_count);
  Writeln('Successfully failed = ', success_compilation_failed_count);
  Writeln('Compilation failures = ', failed_to_compile_count);
  Writeln('Compilation that did not fail while they should = ', failed_compilation_successful_count);

  Writeln('Total number of runs = ', number_runs,' (',
    failed_to_run_count,':',
    successfully_run_count,')');
  Writeln('Successful runs = ', successfully_run_count);
  Writeln('Failed runs = ', failed_to_run_count);
  if known_run_problem>0 then
    Writeln('From these ',known_run_problem,' known problems');

  if successfully_compiled_count <>
     number_runs+skipping_run_unit_count+skipping_run_test_count then
    begin
      Writeln('Number units compiled = ',skipping_run_unit_count);
      Writeln('Number program that should not be run = ',skipping_run_test_count);
    end;
  { about skipped tests }
  number_skipped:= skipping_graph_test_count
    +skipping_interactive_test_count
    +skipping_known_bug_count
    +skipping_other_version_count
    +skipping_other_cpu_count
    +skipping_other_target_count;
  { don't count these ones ...
    skipping_run_unit_count
    skipping_run_test_count }
  Writeln('Number of skipped tests = ',number_skipped);
  Writeln('Number of skipped graph tests = ',skipping_graph_test_count);
  Writeln('Number of skipped interactive tests = ',skipping_interactive_test_count);
  Writeln('Number of skipped known bug tests = ',skipping_known_bug_count);
  Writeln('Number of skipped tests for other versions = ',skipping_other_version_count);
  Writeln('Number of skipped tests for other cpus = ',skipping_other_cpu_count);
  Writeln('Number of skipped tests for other targets = ',skipping_other_target_count);
  if unknown_lines>0 then
    Writeln('Number of unrecognized lines = ',unknown_lines);

  if unexpected_run>0 then
    Writeln('Number of unexpected runs = ',unexpected_run);
end;

var
  logfile : text;
  logfilename,
  line : string;

begin
  if Paramcount>0 then
    logfilename:=Paramstr(1)
  else
    logfilename:=ResLogfile;

  assign(logfile,logfilename);
{$i-}
  reset(logfile);
  if ioresult<>0 then
    begin
      Writeln('Unable to open ',logfilename);
      halt(1);
    end;
{$i+}
  while not eof(logfile) do
    begin
      readln(logfile,line);
      analyse(line);
    end;
  close(logfile);
  display_results;
end.

{ The Computer Language Benchmarks Game
  http://shootout.alioth.debian.org

  contributed by Steve Fisher
  modified by Peter Vreman

  compile with
  fpc -O3 regex-dna.pp
}

uses regexpr,strutils;

function replace_matches( const target: pchar;  const repl: ansistring;
                const str: ansistring;  var dest: ansistring ): longint;
var
  engine : tRegexprEngine;
  count, index, size : longint;
  pstart : pchar;
  starti : longint;
begin
  if not GenerateRegExprEngine( target, [], engine) then
  begin
    writeln( 'Failed to generate regex. engine for "',target,'".' );
    halt(1)
  end;
  count := 0;
  dest := '';
  starti := 1;
  pstart := pchar(str);
  while starti <= length(str) do
  begin
    if RegExprPos(engine, pstart, index, size ) then
    begin
      inc(count);
      dest := dest + Copy( str, starti, index) + repl;
      inc(pstart,index+size);
      inc(starti,index+size);
    end
    else
      break
  end;
  DestroyRegExprEngine( engine );
  dest:=dest+Copy( str, starti, length(str)-starti+1);
  exit(count);
end;


function count_matches( target: pchar; const str: ansistring ): longint;
var
  engine : tRegexprEngine;
  pstart : pchar;
  starti,
  count, index, size : longint;
begin
  if not GenerateRegExprEngine( target, [ref_caseinsensitive], engine) then
  begin
    writeln( 'Failed to generate regex. engine for "',target,'".' );
    halt(1)
  end;
  count := 0;
  pstart := pchar(str);
  starti := 1;
  while starti <= length(str) do
  begin
    if RegExprPos(engine, pstart, index, size ) then
    begin
      inc(count);
      inc(pstart,index+size);
      inc(starti,index+size);
    end
    else
      break
  end;
  DestroyRegExprEngine( engine );
  exit(count)
end;

const
  patterns : array[1..9] of pchar =
    (
      '(agggtaaa)|(tttaccct)',
      '([cgt]gggtaaa)|(tttaccc[acg])',
      '(a[act]ggtaaa)|(tttacc[agt]t)',
      '(ag[act]gtaaa)|(tttac[agt]ct)',
      '(agg[act]taaa)|(ttta[agt]cct)',
      '(aggg[acg]aaa)|(ttt[cgt]ccct)',
      '(agggt[cgt]aa)|(tt[acg]accct)',
      '(agggta[cgt]a)|(t[acg]taccct)',
      '(agggtaa[cgt])|([acg]ttaccct)'
    );
  replacements : array[1..11,1..2] of pchar =
  (
    ('B', '(c|g|t)'), ('D', '(a|g|t)'), ('H', '(a|c|t)'), ('K', '(g|t)'),
    ('M', '(a|c)'), ('N', '(a|c|g|t)'), ('R', '(a|g)'), ('S', '(c|t)'),
    ('V', '(a|c|g)'), ('W', '(a|t)'), ('Y', '(c|t)')
  );


var
  pattern : pchar;
  sequence, new_seq : ansiString;
  line, tmp: string[255];
  letter, repl : pchar;
  i, count, init_length, clean_length, reps : longint;
  inbuf : array[0..64*1024] of char;
begin
  settextbuf(input,inbuf);
  sequence := '';
  init_length := 0;
  while not eof do
  begin
    readln( line );
    init_length += length( line ) + 1;
    if line[1] <> '>' then
      sequence := sequence + line;
  end;
  clean_length := length(sequence);

  for i := low(patterns) to high(patterns) do
  begin
    pattern := patterns[i];
    count := count_matches( pattern, sequence );
    tmp := delChars( delChars(pattern,'('), ')' );
    writeln( tmp, ' ', count);
  end;


  //  Replace.
  for i := low(replacements) to high(replacements) do
  begin
    letter := replacements[i][1];  repl := replacements[i][2];
    reps := replace_matches(letter,repl,sequence,new_seq);
    sequence := new_seq;
  end;


  writeln;
  writeln( init_length );
  writeln( clean_length );
  writeln( length(sequence) );
end.

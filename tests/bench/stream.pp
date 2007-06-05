program stream;

{$ifdef unix}
uses baseunix,unix;
{$endif}

{$ifdef windows}
uses windows;
{$endif}

{-----------------------------------------------------------------------}
{ Original code developed by John D. McCalpin                           }
{ Programmers: John D. McCalpin                                         }
{              Joe R. Zagar                                             }
{ Pascal conversion: Daniel Mantione                                    }
{                                                                       }
{ This program measures memory transfer rates in MB/s for simple        }
{ computational kernels coded in Pascal.                                }
{-----------------------------------------------------------------------}
{ Copyright 1991-2005: John D. McCalpin                                 }
{-----------------------------------------------------------------------}
{ License:                                                              }
{  1. You are free to use this program and/or to redistribute           }
{     this program.                                                     }
{  2. You are free to modify this program for your own use,             }
{     including commercial use, subject to the publication              }
{     restrictions in item 3.                                           }
{  3. You are free to publish results obtained from running this        }
{     program, or from works that you derive from this program,         }
{     with the following limitations:                                   }
{     3a. In order to be referred to as "STREAM benchmark results",     }
{         published results must be in conformance to the STREAM        }
{         Run Rules, (briefly reviewed below) published at              }
{         http://www.cs.virginia.edu/stream/ref.html                    }
{         and incorporated herein by reference.                         }
{         As the copyright holder, John McCalpin retains the            }
{         right to determine conformity with the Run Rules.             }
{     3b. Results based on modified source code or on runs not in       }
{         accordance with the STREAM Run Rules must be clearly          }
{         labelled whenever they are published.  Examples of            }
{         proper labelling include:                                     }
{         "tuned STREAM benchmark results"                              }
{         "based on a variant of the STREAM benchmark code"             }
{         Other comparable, clear and reasonable labelling is           }
{         acceptable.                                                   }
{     3c. Submission of results to the STREAM benchmark web site        }
{         is encouraged, but not required.                              }
{  4. Use of this program or creation of derived works based on this    }
{     program constitutes acceptance of these licensing restrictions.   }
{  5. Absolutely no warranty is expressed or implied.                   }
{-----------------------------------------------------------------------}

{ INSTRUCTIONS:
 *
 *	1) Stream requires a good bit of memory to run.  Adjust the
 *          value of 'N' (below) to give a 'timing calibration' of 
 *          at least 20 clock-ticks.  This will provide rate estimates
 *          that should be good to about 5% precision.
 }

const N	        = 2000000;
      NTIMES	= 10;
      OFFSET	= 0;

{
 *	3) Compile the code with full optimization.  Many compilers
 *	   generate unreasonably bad code before the optimizer tightens
 *	   things up.  If the results are unreasonably good, on the
 *	   other hand, the optimizer might be too smart for me!
 *
 *         Try compiling with:
 *               cc -O stream_omp.c -o stream_omp
 *
 *         This is known to work on Cray, SGI, IBM, and Sun machines.
 *
 *
 *	4) Mail the results to mccalpin@cs.virginia.edu
 *	   Be sure to include:
 *		a) computer hardware model number and software revision
 *		b) the compiler flags
 *		c) all of the output from the test case.
 * Thanks!
 *
 }

const HLINE = '-------------------------------------------------------------';

      inf = 1/0;

var a,b,c:array[0..N+OFFSET-1] of double;

    avgtime:array[0..3] of double = (0,0,0,0);
    maxtime:array[0..3] of double = (0,0,0,0);
	mintime:array[0..3] of double = (inf,inf,inf,inf);

    labels:array[0..3] of string[16]= ('Copy:',
                                       'Scale:',
                                       'Add:',
                                       'Triad:');

    bytes:array[0..3] of cardinal = (
      2 * sizeof(double) * N,
      2 * sizeof(double) * N,
      3 * sizeof(double) * N,
      3 * sizeof(double) * N
    );

const	M=20;

function min(a,b:longint):longint;inline;

begin
  if a>b then
    min:=b
  else
    min:=a;
end;

function max(a,b:longint):longint;inline;

begin
  if a>b then
    max:=a
  else
    max:=b;
end;

function min(a,b:double):double;inline;

begin
  if a>b then
    min:=b
  else
    min:=a;
end;

function max(a,b:double):double;inline;

begin
  if a>b then
    max:=a
  else
    max:=b;
end;

procedure tuned_STREAM_Copy;

var j:longint;

begin
  for j:=0 to N-1 do
    c[j]:=a[j];
end;

procedure tuned_STREAM_Scale(scalar:double);

var j:longint;

begin
  for j:=0 to N-1 do
    b[j]:=scalar*c[j];
end;

procedure tuned_STREAM_Add;

var j:longint;

begin
  for j:=0 to N-1 do
   c[j]:=a[j]+b[j];
end;

procedure tuned_STREAM_Triad(scalar:double);

var j:longint;

begin
  for j:=0 to N-1 do
    a[j]:=b[j]+scalar*c[j];
end;

{$ifdef unix}
{$define have_mysecond}
function mysecond:double;

var tp:timeval;
    tzp:timezone;

begin
  fpgettimeofday(@tp,@tzp);
  mysecond:=double(tp.tv_sec)+double(tp.tv_usec)*1e-6;
end;
{$endif}

{$ifdef windows}
{$define have_mysecond}
function mysecond:double;

begin
  mysecond:=gettickcount*1e-3;
end;
{$endif}

{$ifndef have_mysecond}
{$error Please implement a mysecond for your platform.}
{$endif}

function checktick:longint;

var i,minDelta,Delta:longint;
    t1,t2:double;
    timesfound:array[0..M-1] of double;

begin
  {  Collect a sequence of M unique time values from the system. }
  for i:=0 to M-1 do
    begin
      t1:=mysecond;
      t2:=t1;
      while t2-t1<1E-6 do
        t2:=mysecond;
      t1:=t2;
      timesfound[i]:=t1;
    end;

  {
   * Determine the minimum difference between these M values.
   * This result will be our estimate (in microseconds) for the
   * clock granularity.
  }

   minDelta:=1000000;
   for i:=1 to M-1 do
     begin
       Delta:=trunc(1E6*(timesfound[i]-timesfound[i-1]));
       minDelta:=MIN(minDelta,MAX(Delta,0));
     end;

   checktick:=minDelta;
end;

procedure checkSTREAMresults;

var aj,bj,cj,scalar:double;
	asum,bsum,csum:double;
	epsilon:double;
	j,k:longint;

begin
    { reproduce initialization }
	aj:=1;
	bj:=2;
	cj:=0;
    { a[] is modified during timing check }
	aj:=2*aj;
    { now execute timing loop }
	scalar:=3;
    for k:=0 to NTIMES-1 do
      begin
        cj:=aj;
        bj:=scalar*cj;
        cj:=aj+bj;
        aj:=bj+scalar*cj;
       end;
	aj:=aj*N;
	bj:=bj*N;
	cj:=cj*N;

	asum:=0;
	bsum:=0;
	csum:=0;
    for j:=0 to N-1 do
      begin
		asum:=asum+a[j];
		bsum:=bsum+b[j];
		csum:=csum+c[j];
      end;
{$ifdef VERBOSE}
	writeln('Results Comparison: ');
	writeln('        Expected  : ',aj,' ',bj,' ',cj);
	writeln('        Observed  : ',asum,' ',bsum,' ',csum);
{$endif}

	epsilon:=1e-8;

	if abs(aj-asum)/asum>epsilon then
      begin
        writeln('Failed Validation on array a');
        writeln('        Expected  : ',aj);
        writeln('        Observed  : ',asum);
      end
	else if abs(bj-bsum)/bsum>epsilon then
      begin
        writeln('Failed Validation on array b');
        writeln('        Expected  : ',bj);
        writeln('        Observed  : ',bsum);
      end
	else if abs(cj-csum)/csum>epsilon then
      begin
        writeln('Failed Validation on array c');
        writeln('        Expected  : ',cj);
        writeln('        Observed  : ',csum);
      end
	else
      writeln('Solution Validates');
end;

var quantum:longint;
    BytesPerWord:longint;
    j,k:longint;
    scalar,t:double;
    times:array[0..3,0..NTIMES-1] of double;
    
begin
    { --- SETUP --- determine precision and check timing --- }
    writeln(HLINE);
    writeln('STREAM version Revision: 5.6');
    writeln(HLINE);
    BytesPerWord:=sizeof(double);
    writeln('This system uses ',BytesPerWord,' bytes per DOUBLE PRECISION word.');

    writeln(HLINE);
    writeln('Array size = ',N,', Offset = ',OFFSET);
    writeln('Total memory required = ',3*BytesPerWord*(N/1048576),' MB.');
    writeln('Each test is run ',NTIMES,' times, but only');
    writeln('the *best* time for each is used.');

    writeln(HLINE);
    writeln('writelning one line per active thread....');

    { Get initial value for system clock. }
    for j:=0 to N-1 do
      begin
        a[j]:=1;
        b[j]:=2;
        c[j]:=0;
      end;

    writeln(HLINE);
    
    quantum:=checktick;
    if quantum>=1 then 
      writeln('Your clock granularity/precision appears to be ',quantum,
	          ' microseconds.')
    else
      writeln('Your clock granularity appears to be '+
              'less than one microsecond.');

    t:=mysecond;
    for j:=0 to N-1 do 
	  a[j]:=2*a[j];
    t:=1E6*(mysecond-t);

    writeln('Each test below will take on the order of ',t,
	        ' microseconds.');
    writeln('   (= ',t/quantum,' clock ticks)');
    writeln('Increase the size of the arrays if this shows that');
    writeln('you are not getting at least 20 clock ticks per test.');

    writeln(HLINE);

    writeln('WARNING -- The above is only a rough guideline.');
    writeln('For best results, please be sure you know the');
    writeln('precision of your system timer.');
    writeln(HLINE);
    
    {	--- MAIN LOOP --- repeat test cases NTIMES times --- }

    scalar:=3;
    for k:=0 to NTIMES-1 do
      begin
        times[0,k]:=mysecond();
{$ifdef TUNED}
        tuned_STREAM_Copy();
{$else}
        for j:=0 to N-1 do
          c[j]:=a[j];
{$endif}
        times[0,k]:=mysecond-times[0,k];
	
        times[1,k]:=mysecond;
{$ifdef TUNED}
        tuned_STREAM_Scale(scalar);
{$else}
        for j:=0 to N-1 do
	      b[j]:=scalar*c[j];
{$endif}
        times[1,k]:=mysecond-times[1,k];
        times[2,k]:=mysecond;
{$ifdef TUNED}
        tuned_STREAM_Add();
{$else}
        for j:=0 to N-1 do
          c[j]:=a[j]+b[j];
{$endif}
        times[2,k]:=mysecond-times[2,k];
        times[3,k]:=mysecond;
{$ifdef TUNED}
        tuned_STREAM_Triad(scalar);
{$else}
        for j:=0 to N-1 do
          a[j]:=b[j]+scalar*c[j];
{$endif}
        times[3,k]:=mysecond-times[3,k];
      end;

    {	--- SUMMARY --- }
    for k:=1 to NTIMES-1 do { note -- skip first iteration }
      for j:=0 to 3 do
        begin
          avgtime[j]:=avgtime[j] + times[j,k];
          mintime[j]:=MIN(mintime[j], times[j,k]);
          maxtime[j]:=MAX(maxtime[j], times[j,k]);
        end;
    
    writeln('Function      Rate (MB/s)   Avg time     Min time     Max time');
    for j:=0 to 3 do
      begin
        avgtime[j]:=avgtime[j]/(NTIMES-1);
        writeln(labels[j]:11,
                1E-6*bytes[j]/mintime[j]:11:4,
                avgtime[j]:11:4,
                mintime[j]:11:4,
                maxtime[j]:11:4);
      end;
    writeln(HLINE);

    { --- Check Results --- }
    checkSTREAMresults;
    writeln(HLINE);
end.

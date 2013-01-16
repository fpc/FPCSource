#!/usr/bin/env bash

# Script to check for errno strings in errnostr.inc 
# compared to constant values in errno.inc

temps="check_errnostr_list.sh testerrnostr* errnostrlst.inc"

if [ "$1" == "clean" ] ; then
  echo Removing $temps
  rm -f $temps
  exit
fi

# Use gsed if present
SED=`which gsed`
if [ "$SED" == "" ] ; then
  SED=sed
fi

$SED -n "s:ESysE\(.*\)[[:space:]]*=[[:space:]]*\([[:space:]0-9]*\);: test_errnostr E\1 \2 :p" errno.inc | \
  $SED "s:':'':g" > check_errnostr_list.sh 

if [ "$1" == "verbose" ] ; then
  verbose=1
  fpcopt=-gl
else
  verbose=0
  fpcopt=
fi

# Reverse 'error string', { ENUMBER }
# to ENUMBER string 
$SED -n -e "s|[^']*\('.*'\)[[:space:]]*,*[[:space:]]*{[[:space:]]*\(E[A-Za-z_0-9]*\).*|(Number : ESys\2; NumberStr : '\2'; Str : \1),|p" errnostr.inc > errnostrlst.inc


# Use temp directory to avoid
# re-compilation of system unit
mkdir .testtmp
cd .testtmp

# Free Pascal source including
# errnostr.inc file
# to test if strings are correct
cat > testerrnostr.pp <<EOF

uses
  Dos;

{\$i errnostr.inc}
{\$i errno.inc}

type
  TNumberString = record
    Number : longint;
    NumberStr : String;
    Str : String;
  end;

const
  ErrStringArray :
   Array [0..sys_errn] of TNumberString= ( 
   (Number :0; NumberStr : '0' ; str : ''),
{\$i errnostrlst.inc}
   (Number :-1; NumberStr : ''; str : ''));

var
  ErrorName : string;
  value,i,j : longint;
  verbose : boolean;
  str : string;
function Quote (s : string) : string;
var
  i : longint;
begin
  Quote:='';
  for i:=1 to length(s) do
    if (s[i]='''') then
      Quote:=quote+''''''
    else
      Quote:=quote+s[i];
end;

begin
  if (paramcount=1) and (paramstr(1)='--write') then
    begin
      for i:=0 to sys_errn-1 do
        for j:=0 to sys_errn do
          if (ErrStringArray[j].Number=i) then
            with ErrStringArray[j] do 
        writeln('  ''',Quote(Str), ''' { ',NumberStr, ' ',Number,' }');

      exit;
    end;
  if paramcount < 3 then
    begin
      writeln('Usage: testerrnostr ENAME value "Comment"');
      exit;
    end;
  val(paramstr(2),value);
  if (value>=0) and (value<sys_errn) then
    str:=sys_errlist[value] 
  else
    str:='';
  verbose:=(GetEnv('verbose')<>'0');
  while (pos('''',str)>0) do
    delete(str,pos('''',str),1);
  
  if pos(str,paramstr(3))>0 then
    begin
      if verbose then
        writeln('String for ',paramstr(1),' is "',str,'" contained in ',paramstr(3));
    end
  else if (value>=sys_errn) then
    writeln('String for ',paramstr(1),' index ',value,'not in errnostr.inc, comment is ',paramstr(3))
  else
    begin
      write('String for ',paramstr(1),' index ',value, ' is "',sys_errlist[value],'"');
      writeln(', comment in errno.inc is ',paramstr(3));
    end;
end.
EOF

fpc $fpcopt  -Fi.. -o../testerrnostr ./testerrnostr.pp
res=$?
cd ..
if [ $res -ne 0 ] ; then
  echo "Compilation of testerrnostr.pp failed"
  exit
else
  rm -Rf .testtmp
fi

export verbose

function test_errnostr ()
{
  if [ $verbose -eq 1 ] ; then
    echo "Testing errno \"$1\""
  fi
  errno=$1
  shift
  value=$1
  shift
  comment="$@"
  comment2=`grep $errno errnostrlst.inc`
  ./testerrnostr $errno $value "$comment"
}

. ./check_errnostr_list.sh

# ./testerrnostr --write

{$ifdef Delphi}
const fpcfg : array[0..3] of string[240]=(
{$else Delphi}
const fpcfg : array[0..3,1..240] of char=(
{$endif Delphi}
  '#IFDEF NORMAL'#013#010+
  ' -Ci'#013#010+
  ' -XS'#013#010+
  ' -T%fpctargetos%'#013#010+
  ' -Sg'#013#010+
  ' -O1'#013#010+
  ' -Oppentium3'#013#010+
  ' -Ratt'#013#010+
  ' -Fu%basepath%\units\%fpctarget%-%fpctargetos%'#013#010+
  ' -Fu%basepath%\units\%fpctarget%-%fpctargetos%\*'#013#010+
  ' -Fu%basepath%\units\%fpctarget%-%fpctargetos%\rtl'#013#010+
  ' -g-'#013#010+
  ' -p-'#013,#010+
  ' -b-'#013#010+
  '#ENDIF'#013#010+
  #013#010+
  '#IFDEF DEBUG'#013#010+
  ' -Ci'#013#010+
  ' -XS'#013#010+
  ' -T%fpctargetos%'#013#010+
  ' -Sg'#013#010+
  ' -Cr'#013#010+
  ' -Co'#013#010+
  ' -Oppentium3'#013#010+
  ' -Ratt'#013#010+
  ' -Fu%basepath%\units\%fpctarget%-%fpctargetos%'#013#010+
  ' -Fu%basepath%\units\%fpctarget%-%fpctargetos%\*'#013#010+
  ' -Fu%basepath%\units\%fpctarget%-%fpctarg','etos%\rtl'#013#010+
  ' -g'#013#010+
  ' -p-'#013#010+
  ' -b-'#013#010+
  '#ENDIF'#013#010+
  #013#010+
  '#IFDEF RELEASE'#013#010+
  ' -XS'#013#010+
  ' -T%fpctargetos%'#013#010+
  ' -Sg'#013#010+
  ' -O2'#013#010+
  ' -Oppentium3'#013#010+
  ' -Ratt'#013#010+
  ' -Fu%basepath%\units\%fpctarget%-%fpctargetos%'#013#010+
  ' -Fu%basepath%\units\%fpctarget%-%fpctargetos%\*'#013#010+
  ' -Fu%basepath%\units\%fpctarge','t%-%fpctargetos%\rtl'#013#010+
  ' -g-'#013#010+
  ' -p-'#013#010+
  ' -b-'#013#010+
  '#ENDIF'#013#010
);

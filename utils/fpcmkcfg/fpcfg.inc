{$ifdef Delphi}
const fpcfg : array[0..2] of string[240]=(
{$else Delphi}
const fpcfg : array[0..2,1..240] of char=(
{$endif Delphi}
  '#IFDEF NORMAL'#010+
  ' -Ci'#010+
  ' -XS'#010+
  ' -T%fpctargetos%'#010+
  ' -Sg'#010+
  ' -O1'#010+
  ' -Fu%basepath%\units\$fpctarget'#010+
  ' -Fu%basepath%\units\$fpctarget\*'#010+
  ' -Fu%basepath%\units\$fpctarget\rtl'#010+
  '%GCCLIBPATH%'#010+
  ' -g-'#010+
  ' -p-'#010+
  ' -b-'#010+
  '#ENDIF'#010+
  #010+
  '#IFDEF DEBUG'#010+
  ' -Ci'#010+
  ' -XS'#010+
  ' -T%fpctargetos%'#010+
  ' -Sg'#010+
  ' -Cr'#010+
  ' ','-Co'#010+
  ' -Fu%basepath%\units\$fpctarget'#010+
  ' -Fu%basepath%\units\$fpctarget\*'#010+
  ' -Fu%basepath%\units\$fpctarget\rtl'#010+
  '%GCCLIBPATH%'#010+
  ' -g'#010+
  ' -p-'#010+
  ' -b-'#010+
  '#ENDIF'#010+
  #010+
  '#IFDEF RELEASE'#010+
  ' -XS'#010+
  ' -T%fpctargetos%'#010+
  ' -Sg'#010+
  ' -O2'#010+
  ' -Fu%basepath%\units\$fpctarget'#010+
  ' -Fu%basepath%\units','\$fpctarget\*'#010+
  ' -Fu%basepath%\units\$fpctarget\rtl'#010+
  '%GCCLIBPATH%'#010+
  ' -g-'#010+
  ' -p-'#010+
  ' -b-'#010+
  '#ENDIF'#010
);

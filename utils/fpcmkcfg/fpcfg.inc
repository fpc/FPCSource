{$ifdef Delphi}
const fpcfg : array[0..2] of string[240]=(
{$else Delphi}
const fpcfg : array[0..2,1..240] of char=(
{$endif Delphi}
  '#IFDEF NORMAL'#013#010+
  ' -Ci'#013#010+
  ' -XS'#013#010+
  ' -T%fpctargetos%'#013#010+
  ' -Sg'#013#010+
  ' -O1'#013#010+
  ' -Fu%basepath%\units\$fpctarget'#013#010+
  ' -Fu%basepath%\units\$fpctarget\*'#013#010+
  ' -Fu%basepath%\units\$fpctarget\rtl'#013#010+
  '%GCCLIBPATH%'#013#010+
  ' -g-'#013#010+
  ' -p-'#013#010+
  ' -b-'#013#010+
  '#ENDIF'#013#010+
  #013#010+
  '#IFDEF DEBUG'#013#010+
  ' -Ci'#013#010+
  ' -XS'#013#010+
  ' -T%fpctar','getos%'#013#010+
  ' -Sg'#013#010+
  ' -Cr'#013#010+
  ' -Co'#013#010+
  ' -Fu%basepath%\units\$fpctarget'#013#010+
  ' -Fu%basepath%\units\$fpctarget\*'#013#010+
  ' -Fu%basepath%\units\$fpctarget\rtl'#013#010+
  '%GCCLIBPATH%'#013#010+
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
  ' -Fu%basepath%\u','nits\$fpctarget'#013#010+
  ' -Fu%basepath%\units\$fpctarget\*'#013#010+
  ' -Fu%basepath%\units\$fpctarget\rtl'#013#010+
  '%GCCLIBPATH%'#013#010+
  ' -g-'#013#010+
  ' -p-'#013#010+
  ' -b-'#013#010+
  '#ENDIF'#013#010
);

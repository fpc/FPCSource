{$ifdef Delphi}
const fppkg_default : array[0..0] of string[240]=(
{$else Delphi}
const fppkg_default : array[0..0,1..240] of char=(
{$endif Delphi}
  '[Defaults]'#010+
  'ConfigVersion=4'#010+
  'GlobalPrefix=%GlobalPrefix%'#010+
  'LocalPrefix={LocalRepository}'#010+
  'Compiler=%fpcbin%'#010+
  'OS=%fpctargetos%'#010+
  'CPU=%fpctarget%'#010
);

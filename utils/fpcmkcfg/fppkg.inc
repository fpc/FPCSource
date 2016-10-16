{$ifdef Delphi}
const fppkg : array[0..2] of string[240]=(
{$else Delphi}
const fppkg : array[0..2,1..240] of char=(
{$endif Delphi}
  '[Defaults]'#010+
  'ConfigVersion=5'#010+
  'LocalRepository=%LocalRepository%'#010+
  'BuildDir={LocalRepository}build/'#010+
  'ArchivesDir={LocalRepository}archives/'#010+
  'CompilerConfigDir=%CompilerConfigDir%'#010+
  'RemoteMirrors=http://www.freepascal.org/repository/mirrors.xml'#010+
  'Remote','Repository=auto'#010+
  'CompilerConfig=default'#010+
  'FPMakeCompilerConfig=default'#010+
  'Downloader=lnet'#010+
  'InstallRepository=user'#010+
  #010+
  '[Repository]'#010+
  'Name=fpc'#010+
  'Description=Packages which are installed along with the Free Pascal Co'+
  'mpiler'#010+
  'Path=%GlobalPath%/{CompilerVersio','n}/'#010+
  'Prefix=%GlobalPrefix%'#010+
  #010+
  '[Repository]'#010+
  'Name=user'#010+
  'Description=User-installed packages'#010+
  'Path={LocalRepository}lib/fpc/{CompilerVersion}/'#010+
  'Prefix={LocalRepository}'#010+
  #010+
  '[IncludeFiles]'#010+
  'FileMask=%CompilerConfigDir%conf.d/*.conf'#010+
  #010
);

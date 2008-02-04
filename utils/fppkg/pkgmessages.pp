unit pkgmessages;

{$mode objfpc}{$H+}

interface


Resourcestring
  SError                     = 'Error: ';
  SWarning                   = 'Warning: ';
  SDebug                     = 'Debug: ';

  SErrInValidArgument        = 'Invalid command-line argument at position %d : %s';
  SErrNeedArgument           = 'Option at position %d (%s) needs an argument';
  SErrMissingFPC             = 'Could not find a fpc executable in the PATH';
  SErrInvalidFPCInfo         = 'Compiler returns invalid information, check if fpc -iV works';
  SErrMissingFPMake          = 'Missing configuration fpmake.pp';
  SErrMissingMakefilefpc     = 'Missing configuration Makefile.fpc';
  SErrMissingDirectory       = 'Missing directory "%s"';
  SErrMissingCompilerConfig  = 'Could not find compiler configuration "%s"';
  SErrMissingInstallPackage  = 'Could not find package "%s"';
  SErrNoPackageSpecified     = 'No package specified';
  SErrNoPackageAvailable     = 'Package %s %s is not available';
  SErrOnlyLocalDir           = 'The speficied command "%s" works only on current dir, not on a (remote) package';
  SErrExecutionFPMake        = 'Execution of FPMake %s failed';
  SErrException              = 'The FPC Package tool encountered the following error:';
  SErrActionAlreadyRegistered= 'Action "%s" is already registered';
  SErrActionNotFound         = 'Action "%s" is not supported';
  SErrFailedToCompileFPCMake = 'Could not compile fpmake driver program';
  SErrNoFTPDownload          = 'This binary has no support for FTP downloads.';
  SErrNoHTTPDownload         = 'This binary has no support for HTTP downloads.';
  SErrBackupFailed           = 'Backup of file "%s" to file "%s" failed.';
  SErrUnknownProtocol        = 'Unknown download protocol: "%s"';
  SErrNoSuchFile             = 'File "%s" does not exist.';
  SErrDownloadFailed         = '%s Download of "%s" failed: %s';
  SErrInvalidLogLevels       = 'Invalid verbosity string: "%s"';
  SErrInvalidCommand         = 'Invalid command: %s';
  SErrChangeDirFailed        = 'Could not change directory to "%s"';
  SErrCorruptPackagesFile    = 'Packages file "%s" is corrupt, delete file manual and retry';
  SErrCorruptMirrorsFile     = 'Mirrors file "%s" is corrupt, delete file manual and retry';
  SErrPackageIsLocal         = 'Operation not supported for local packages';
  SErrConvertFPMakeExists    = 'Found existing fpmake.pp, aborting conversion';
  SErrFailedToSelectMirror   = 'Could not select a mirror, run update and retry';
  SErrUnsupportedConfigVersion = 'Configuration file "%s" is too old, delete file manual and retry';
  SErrPackageDoesNotSupportTarget = 'Package %s does not support %s';
  SErrHTTPGetFailed          = 'HTTP Download failed.';
  SErrLoginFailed            = 'FTP LOGIN command failed.';
  SErrCWDFailed              = 'FTP CWD "%s" command failed.';
  SErrGETFailed              = 'FTP GET "%s" command failed.';

  SLogGeneratingFPMake       = 'Generating fpmake.pp';
  SLogNotCompilingFPMake     = 'Skipping compiling of fpmake.pp, fpmake executable already exists';
  SLogCommandLineAction      = 'Adding action from commandline: "%s %s"';
  SLogRunAction              = 'Action: "%s %s"';
  SLogExecute                = 'Executing: "%s %s"';
  SLogChangeDir              = 'CurrentDir: "%s"';
  SLogDownloading            = 'Downloading "%s" to "%s"';
  SLogUnzippping             = 'Unzipping "%s"';
  SLogZippping               = 'Zipping "%s"';
  SLogLoadingGlobalConfig    = 'Loading global configuration from "%s"';
  SLogLoadingCompilerConfig  = 'Loading compiler configuration from "%s"';
  SLogLoadingFPMakeCompilerConfig = 'Loading compiler configuration for fpmake building from "%s"';
  SLogGeneratingGlobalConfig = 'Generating default global configuration in "%s"';
  SLogDetectedCompiler       = 'Detected compiler "%s" (version %s for %s)';
  SLogDetectedFPCDIR         = 'Detected %s FPCDIR "%s"';
  SLogGeneratingCompilerConfig  = 'Generating default compiler configuration in "%s"';
  SLogLoadingPackagesFile    = 'Loading available packages from "%s"';
  SLogLoadingMirrorsFile     = 'Loading available mirrors from "%s"';
  SLogFindInstalledPackages  = 'Finding installed packages in "%s"';
  SLogSavingStatusFile       = 'Saving local status to "%s"';
  SLogFPMKUnitDepVersion     = 'Checking for %s %s, installed %s, available %s';
  SLogFPMKUnitDepTooOld      = 'Minimum version of %s is not installed, using internal fpmkunit with limited functionality';
  SLogSelectedMirror         = 'Selected mirror "%s"';
  SLogUpgradingConfig        = 'Configuration file "%s" is updated with new configuration settings';

  SDbgFound                  = 'Found';
  SDbgNotFound               = 'Not Found';
  SDbgDirectoryExists        = 'Directory "%s" %s';
  SDbgFileExists             = 'File "%s" %s';
  SDbgBackupFile             = 'Creating Backup File "%s"';
  SDbgPackageDependency      = 'Dependency on package %s %s, installed %s, available %s  (%s)';

implementation

end.


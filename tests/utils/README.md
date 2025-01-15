# DBDigest tool.

## Configuration

There are 2 kinds of configuration data for DBDigest.
- global configuration
- Test run data

### Global configuration
This includes database configuration and run mode.
the database configuration can be specified in 3 different ways: 
- in the global `/etc/dbdigest.ini`
- the `dbigest.cfg` run file (deprecated)
- the command-line. (deprecated)

The global file is read first, if it exists. The `dbdigest.cfg` file is read
next, and the command-line options are list:

The recommended way is to put it in the global config file, which has the
following format (the values are examples):
```ini
[Database]
Name=testsuite
Host=localhost
username=user
password=secret
port=5432
```
In the 'dbdigest.cfg' file, the format is backwards-compatible:
```text
databasename=NAME
host=HOST
password=PWD
username=USER
```
On the command-line the options are:
```
  -d --databasename=NAME            database name
  -h --host=HOST                    database hostname
  -p --password=PWD                 database user password
  -P --port=NNN                     database connection port
  -u --username=USER                database user name
```
Other than the database connection, the following global options can be
given:
- `-r --relsrcdir=DIR` the relative source dir for getting test files.
- `-S --testsrcdir=DIR` the absolute test source dir
- `-T --threadlist=FILE`  file with configuration file names to import.
- `-j --threadcount=N` Maximum number of threads to use when importing.
- `-V --verbose` be more verbose (writes lots of debug info)
- `-f --config=FILENAME` in case a single digest file is imported, the name of the config file. 
    If not set, dbdigest.cfg is used.

If the -T --threadlist option is given, then -f/--config is ignored: no
default file will be read. Only the files in the threadlist file will be
treated.

Example of a thread list file:
```
2025-05-01-i386/gcc-dbdigest.cfg
2025-05-01-arm/llvm-dbdigest.cfg
```
The `logfile` and `longlogfile` will be treated as relative to the dbdigest.cfg files
if they are relative filenames. If they are absolute filenames, they're used
as-is.

### Test Run data
Run data describes one test run: basically, one dbdigest.cfg.
For a single test run, the dbdigest.cfg file is read and the command-line
options are examined to compose all data for a test run.
- `-l --logfile=FILE` set log file to analyse
- `-L --longlogfile=FILE` set long log filename (logs of run tests)
- `-o --os=OS` set OS for testrun
- `-c --cpu=CPU` set CPU
- `-a --category=CAT` set category
- `-v --version=VER` set compiler version
- `-t --date=DATE` date in YYYMMDD(hhmmnn) format (only the date part is retained)
- `-s --submitter=NAME` submitter name
- `-m --machine=NAME` set machine name on which testsuite was run
- `-C --compile-flags=FLAGS` set used compilation flags
- `   --comment=FLAGS` backwards compatible way to set compilation flags (deprecated)
- `-D --description=DESC` set config description (helpful comment)
- `   --compilerdate=DATE` set compiler date
- `   --compilerfullversion=VERSION` set full compiler version
- `   --svncompilerrevision=REV` set revision of used compiler
- `   --svntestsrevision=REV` set revision of testsuite files
- `   --svnrtlrevision=REV` set revision of RTL
- `   --svnpackagesrevision=REV` set revison of packages

The preferred way to specify the options is in a `dbdigest.cfg` file. The
name of this file is settable using the -f or --config command-line option.

The `dbdigest.cfg` accepts all long versions of the command-line options,
and you can specify comments using the usual # sign.

## Examples
Import data from a single testrun, with testrun data in `mytest.cfg`:
```text
dbdigest -f mytest.cfg
```
The database connection data will be read from the global configuration.

Import data from a list of testruns in `mytests.lst` (4 threads):
```text
dbdigest -T mytests.cfg
```
Import data from a list of testruns in `mytests.lst` (8 threads):
```text
dbdigest -T mytests.cfg -j 8
```

# DBAdd tool.

To add new CPUs or OSes to the database, use the `dbadd` tool. It will use
the global `dbdigest.ini` file to connect to the database,  and will add the
new record in the appropriate table. The tool accepts 3 command-line options:
- `-t --type=TYPE`  where `TYPE` is one of `os`, `cpu`, `category` or `version`
- `-v --value=value` the value to add
- `-d --date=YYYYMMDD` only used when adding a version: the release date of
    the version (if not specified, today is used).

Test definitions are added automatically during import.



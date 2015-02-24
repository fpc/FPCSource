Compiling FreeTDS DB-Lib with MS Visual C++ 2005/2008/2010:
===========================================================
1.   Download sources from www.freetds.org
2.1  Using Perl interpreter generate files:
     perl.exe encodings.pl > encodings.h
     perl.exe num_limits.pl > num_limits.h
     perl.exe tds_willconvert.pl > tds_willconvert.h
     perl.exe types.pl ../../misc/types.csv ../../include/freetds/proto.h > tds_types.h
2.2  Copy include/tds_sysdep_public.h.in to tds_sysdep_public.h and replace "@dblib_define@" with "#define MSDBLIB"
     Copy include/freetds/version.h.in to version.h and replace "@..@" variables
     Copy win32/config.h.in to config.h
3.   Open FreeTDS.sln from root source directory
3.1  in TDS / Header Files edit config.h and comment "HAVE_INTTYPES_H":
       /* #define HAVE_INTTYPES_H 1 */
       (http://www.freetds.org/userguide/osissues.htm#WINDOWS)
3.2  In Build / Configuration Manager select "Release"
     Right-click on project "db-lib" and select "Properties". Note: select Configuration Properties first if in Visual C++ 2010.
       General / Configuration Type = "Dynamic library (.dll)"
       C/C++ / Preprocesor / Preprocessor Definitions add "MSDBLIB" (optionally default TDS version "TDS71")
       Linker / Input / Additional Dependencies add "ws2_32.lib"
                      / Module Definition File = "..\..\win32\dblib.def"
       Linker / General / Output File change from "$(OutDir)\$(ProjectName).dll" to "$(OutDir)\dblib.dll"
     Right-click on project "replacements" and select "Properties".
       C/C++ / Preprocesor / Preprocessor Definitions add "_WIN32_WINNT=0x0400"
4.   Build "db-lib"
5.   The dblib.dll will appear in the Release\ subdirectory
     Note: To avoid dependency on msvc*.dll you can set in C/C++ / Code Generation / Runtime Library : "Multi-threaded (/MT)" in all projects

     To build dblib.dll under MS Visual C++ 2010 Express for Win64 you must:
     - download and install Microsoft Windows Software Development Kit 7.1 (install before Visual Studio 2010 SP1 !)
     - if you upgrade to Visual Studio 2010 SP1 then you must install Microsoft Visual C++ 2010 Service Pack 1 Compiler Update for the Windows SDK 7.1 (http://support.microsoft.com/kb/2519277/en-us)
     - setup FreeTDS project to target 64-bit platform (http://msdn.microsoft.com/en-us/library/9yb4317s.aspx)
     - Right-click on project "dblib" and select "Properties".
       Linker / General / Additional Library Directories add path to "Microsoft SDKs\Windows\v7.1\Lib\x64"


Compiling FreeTDS with iconv support:
=====================================
(not required when you don't use char/varchar/text datatypes or if you use character set (SBCS) ISO-8859-1 (Latin1) for your char/varchar/text columns)
1.  Download libiconv developer files and binaries for Windows from http://gnuwin32.sourceforge.net/packages/libiconv.htm
	Setup program: http://gnuwin32.sourceforge.net/downlinks/libiconv.php
	- or -
	Developer files: http://gnuwin32.sourceforge.net/downlinks/libiconv-lib-zip.php (include/iconv.h and lib/libiconv.lib)
	Binaries: http://gnuwin32.sourceforge.net/downlinks/libiconv-bin-zip.php  (bin/libiconv2.dll)
    and extract them to a directory, e.g. the directory iconv below your root FreeTDS folder	
2.  in TDS / Header Files edit config.h and change /* #undef HAVE_ICONV */ to #define HAVE_ICONV 1
3.  in TDS Project properties:
      C/C++ / General / Additional Include Directories add path to "include/iconv.h" (e.g. "$(SolutionDir)..\iconv\include"
    in db-lib Project properties:
      C/C++ / General / Additional Include Directories add path to "include/iconv.h" (e.g. "$(SolutionDir)..\iconv\include"
      Linker / Input / Additional Dependencies add "lib/libiconv.lib" (e.g. "$(SolutionDir)..\iconv\lib\libiconv.lib"
4.  Follow regular compilation instructions above
5.  Distribute libiconv2.dll with your dblib.dll


Using in Lazarus:
=================
1. Put on the form TMSSQLConnection or TSQLConnector and set property ConnectorType=MSSQLServer
2. Put into uses clause mssqlconn unit


Known problems:
===============
- CHAR/VARCHAR data truncated to column length when encoding to UTF-8 (use NCHAR/NVARCHAR instead or CAST char/varchar to nchar/nvarchar)
- Multiple result sets (for example when SP returns more than 1 result set only 1st is processed)
- Output parameters of stored procedure are not returned. See FreeTDS FAQ: "I'm not getting my output parameters returned, but I seem to be doing everything right!"
- DB-Library error 10038 "Results Pending" - set TSQLQuery.PacketRecords=-1 to fetch all pendings rows
- BLOB data (IMAGE/TEXT columns) larger than 16MB are truncated to 16MB - (set TMSSQLConnection.Params: 'TEXTSIZE=2147483647' or execute 'SET TEXTSIZE 2147483647')
  (create temporary stored procedures for prepared statements)


Manuals for DB-Library API:
===========================
http://msdn.microsoft.com/en-us/library/aa936988(v=sql.80).aspx
http://manuals.sybase.com/onlinebooks/group-cnarc/cng1110e/dblib/

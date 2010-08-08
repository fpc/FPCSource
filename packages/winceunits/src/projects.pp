{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 ********************************************************************** }

//
//  File: project.h
//
//  Abstract:
//
//  Contents:
//    Project APIs defines
//

//
//  Microsoft Windows Mobile 6.0 for PocketPC SDK.
//

unit projects;

{$CALLING cdecl}

interface

uses Windows;

const
      Note_prjDLL = 'note_prj.dll';

type
     tagENUM = (PRJ_ENUM_MEMORY		    := $01,	// enumerate main memory projects only
	               PRJ_ENUM_FLASH		     := $02,	// enumerate flash card projects only
	               PRJ_ENUM_ALL_DEVICES := $04,	// enumerate main memory projects & Flash card,
	               PRJ_ENUM_ALL_PROJ	   := $10,	// enumerate in all projects
	               PRJ_ENUM_HOME_PROJ	  := $0100 // add 'My Documents' home folder
               );
     PRJ_ENUM = tagENUM;


//////////////////////////////////////////////////////////////////////////////
// THE "OID" PROBLEM
//
// The OS changed from returning faked OIDs for
// storage cards and other FAT file systems to returning invalid / -1.  This
// seriously messes up anything that wants to uses OIDs to identify objects
// on a storage card.
//

type
     PFNCOLCUSTOK = function(iParam1:HWND; iParam2:LPVOID):BOOL; cdecl;
     PROJECTS_ENUMPROC = function(iParam1:DWORD; iParam2:LPARAM):BOOL; cdecl;
     PROJECTSFILES_ENUMPROC = function(idwOID:DWORD; ilParam:LPARAM):BOOL; cdecl;


{*****************************************************************************
EnumProjects	Exported API: Enumerates all projects on the specified file
				system.


PARAMETERS:
pfnEnumProc	pointer to callback function. If NULL, this function simply
			returns the number of projects, without enumerating through them.
			If the callback function ever returns FALSE, the enumeration will
			halt.  The callback has the following prototype:
			BOOL CALLBACK EnumProjectsCallback(DWORD dwOid, LPARAM lParam);

dwOidFlash	Only used for (dwFlags == PRJ_ENUM_FLASH).  This is the OID of
			the flash card to look at; the value returned by
			FindFirstFlashCard or FindNextFlashCard.

dwFlags
			Any combo of the following (specifying the LOCATION to check):
			PRJ_ENUM_MEMORY - check all main memory projects/folders,
							  dwOidFlash is not used
			PRJ_ENUM_FLASH  - check all projects on the specified flash card,
							  where dwOidFlash specifies the flash card to
							  examine.
			PRJ_ENUM_ALL_DEVICES - check all projects/folders from main memory,
							  and from *every* flash card.  (dwOid is not used)

lParam		user defined parameter passed to pfnEnumProc


RETURN:
The total number of projects.  NOTE: this is actually the total number of
projects in MAIN MEMORY ONLY (see below).


THIS IS PROVIDED FOR BACKWARD COMPATIBILITY.  It will silently skip over any
project that isn't on an OID-based filesystem (it enumerates everything in
main memory, but will silently skip everything on storage cards).  The modern
function is EnumProjectsEx.
******************************************************************************}

function EnumProjects(lpEnumProc:PROJECTS_ENUMPROC; dwOid:DWORD; dwFlags:DWORD; lParam:LPARAM):longint; external Note_prjDLL name 'EnumProjects';

{*****************************************************************************
EnumProjectsFiles	Exported API: Enumaretes all files in a project on the
					specified file system.

PARAMETERS:
pfnEnumProc	pointer to callback function. If NULL, this function simply
			returns the number of files without enumerating through them.
			If the callback function ever returns FALSE, the enumeration will
			halt.  The callback has the following prototype:
 (normal)	BOOL CALLBACK EnumProjectFilesCallback(DWORD dwOID, LPARAM lParam);
 (ex ver)	BOOL CALLBACK EnumProjectFilesExCallback(PAstruct* pPA, LPARAM lParam);

dwOidFlash	Only used for (dwFlags & PRJ_ENUM_FLASH).  This is the OID of
			the flash card to look at; the value returned by
			FindFirstFlashCard or FindNextFlashCard.

dwFlags	
			Any combo of the following (specifying the LOCATION to check):
			PRJ_ENUM_MEMORY - check all main memory projects/folders,
							  dwOidFlash is not used
			PRJ_ENUM_FLASH  - check all projects on the specified flash card,
							  where dwOidFlash specifies the flash card to
							  examine.
			PRJ_ENUM_ALL_DEVICES - check all projects/folders from main memory,
							  and from *every* flash card.  (dwOid is not used)

			Plus any of the following (specifying which PROJECTS to check):
			PRJ_ENUM_ALL_PROJ- enumerate in all project (szProj is not used)

szProj		Only used for when (dwFlags & PRJ_ENUM_ALL_PROJ) is NOT set.  This
			specifies which project to search; NULL specifies to look for files
			that aren't in a project/folder (i.e., they're at the top level,
			under "\My Documents").

szFileName	pointer to name of file to search for. (i.e. '*.wav' or '*.*')

lParam		user defined parameter passed to pfnEnumProc 


RETURN:
The total number of matching files found.  NOTE: in the regular version,
this is actually the total number of files in MAIN MEMORY ONLY (see below).


The regular version is provided for backward compatibility.  It will silently
skip over any file that isn't on an OID-based filesystem (it enumerates
everything in main memory, but will silently skip everything on storage cards).

The modern version, EnumProjectsFilesEx, requires the extended callback
function, too.
******************************************************************************}
function EnumProjectsFiles(lpEnumProc:PROJECTSFILES_ENUMPROC;
                           dwOidFlash:DWORD;
                           dwFlags:DWORD;
                           lpszProj:LPTSTR;
                           lpszFileName:LPTSTR;
                           lParam:LPARAM):longint; external Note_prjDLL name 'EnumProjectsFiles';

{**************************************************************************************************
FindFirstFlashCard
Find the first mountable file system
	lpFindProjData -  pointer to returned information
**************************************************************************************************}
function FindFirstFlashCard(lpFindFlashData:LPWIN32_FIND_DATA):HANDLE; external Note_prjDLL name 'FindFirstFlashCard';

{**************************************************************************************************
FindNextFlashCard
Find the next mountable file system
	lpFindProjData	-  pointer to returned information
	hFindFlash		- Identifies a search handle returned by a previous call to the FindFirstFlashCard function.
**************************************************************************************************}
function FindNextFlashCard(hFindFlash:HANDLE; lpFindFlashData:LPWIN32_FIND_DATA):BOOL; external Note_prjDLL name 'FindNextFlashCard';

{**************************************************************************************************
FindFirstProjectFile
Find the first file in a project, on the desired mountable file system
	lpFileName		- pointer to name of file to search for	(i.e: '*.wav')
	lpFindFileData	- pointer to returned information
	dwOidFlash		- Oid to the desired mountable file system (FindFirstFlashCard or FindNextFlashCard)
					  or 0 if main memory
	lpszProj		- desired project, or NULL (or 'All') to search for files that do not
					  have a project (under '\My Documents')
**************************************************************************************************}
function FindFirstProjectFile(lpFileName:LPCTSTR;
                              lpFindFileData:LPWIN32_FIND_DATA;
                              dwOidFlash:DWORD;
                              szProject:LPTSTR):HANDLE; external Note_prjDLL name 'FindFirstProjectFile';

{**************************************************************************************************
FindNextProjectFile
Find the next file in a project
	lpFindProjData	- pointer to returned information
	hFind			- Identifies a search handle returned by a previous call to the FindFirstProjectFile function.
**************************************************************************************************}
function FindNextProjectFile(hFindFlash:HANDLE; lpFindFlashData:LPWIN32_FIND_DATA):BOOL; external Note_prjDLL name 'FindNextProjectFile';

// new code, it solves the "OID Problem"

//////////////////////////////////////////////////////////////////////////////
// PAstruct:
// This is pretty much a drop-in replacement for the spot where OIDs are
// currently used.  Instead of passing around a DWORD, we now pass a pointer
// to this struct.  IMPORTANT: callers are responsible for allocating and
// freeing the string memory for file paths.

type
     _EFileIDType = (FILE_ID_TYPE_OID	 := 0,	// This file is specified through an OID
	                    FILE_ID_TYPE_PATH	:= 1,	// This file is specified through its pathname
	                    FILE_ID_LAST		    := 2	 // last value in the list (invalid)
                    );
     EFileIDType = _EFileIDType;

const
      PA_MAX_PATHNAME = 96;		// including null terminator, practically speaking,
                             // might be " \storage card 3\My Documents\document
                             // folder 7\very long filename.wav" or 69 char's max

// Pure-NT machines don't necessarily define CEOID.  In the future, we'll want
// to replace this define with the actual typedef (JParks).
{$IFNDEF CEOID} // #ifndef CEOID
type
     CEOID = DWORD;
{$ENDIF CEOID} // #endif

type
     _PAstruct	= record	// "PA" (Pathname Array) OID-NUMBER REPLACEMENT STRUCTURE
	      m_IDtype:EFileIDType;	// is this an OID or a pathname?
       case longint of
         0: (m_fileOID:CEOID);	// we're storing the OID
		       1: (m_szPathname:array[0..PA_MAX_PATHNAME-1] of TCHAR);	// we're storing the full pathname\filename
     end;
     PAstruct = _PAstruct;
     PPAstruct = ^_PAstruct;


type
     PROJECTS_ENUMPROC_EX =function(pPA:PPAstruct; lParam:LPARAM):BOOL; cdecl;
     PROJECTSFILES_ENUMPROC_EX = function(pPA:PPAstruct; lParam:LPARAM):BOOL; cdecl;

{*****************************************************************************
EnumProjectsEx	Exported API: Enumerates all projects on the specified file
				system.

PARAMETERS:
pfnEnumProc	pointer to callback function. If NULL, this function simply
			returns the number of projects, without enumerating through them.
			If the callback function ever returns FALSE, the enumeration will
			halt.  The callback has the following prototype:
			BOOL CALLBACK EnumProjectsExCallback(PAstruct* pPA, LPARAM lParam);

dwOidFlash	Only used for (dwFlags == PRJ_ENUM_FLASH).  This is the OID of
			the flash card to look at; the value returned by
			FindFirstFlashCard or FindNextFlashCard.

dwFlags
			Any combo of the following (specifying the LOCATION to check):
			PRJ_ENUM_MEMORY - check all main memory projects/folders,
							  dwOidFlash is not used
			PRJ_ENUM_FLASH  - check all projects on the specified flash card,
							  where dwOidFlash specifies the flash card to
							  examine.
			PRJ_ENUM_ALL_DEVICES - check all projects/folders from main memory,
							  and from *every* flash card.  (dwOid is not used)

lParam		user defined parameter passed to pfnEnumProc


RETURN:
The total number of projects *including* storage cards.


This enhanced version can enumerate through both new (OID-based) file systems
and older (DOS-style) filesystems -- including storage cards.
******************************************************************************}
function EnumProjectsEx(pfnEnumProc:PROJECTS_ENUMPROC_EX;
                        dwOidFlash:DWORD;
                        dwFlags:DWORD;
                        lParam:LPARAM):longint; external Note_prjDLL name 'EnumProjectsEx';


{*****************************************************************************
EnumProjectsFiles	Exported API: Enumaretes all files in a project on the
					specified file system.

PARAMETERS:
pfnEnumProc	pointer to callback function. If NULL, this function simply
			returns the number of files without enumerating through them.
			If the callback function ever returns FALSE, the enumeration will
			halt.  The callback has the following prototype:
 (normal)	BOOL CALLBACK EnumProjectFilesCallback(DWORD dwOID, LPARAM lParam);
 (ex ver)	BOOL CALLBACK EnumProjectFilesExCallback(PAstruct* pPA, LPARAM lParam);

dwOidFlash	Only used for (dwFlags & PRJ_ENUM_FLASH).  This is the OID of
			the flash card to look at; the value returned by
			FindFirstFlashCard or FindNextFlashCard.

dwFlags
			Any combo of the following (specifying the LOCATION to check):
			PRJ_ENUM_MEMORY - check all main memory projects/folders,
							  dwOidFlash is not used
			PRJ_ENUM_FLASH  - check all projects on the specified flash card,
							  where dwOidFlash specifies the flash card to
							  examine.
			PRJ_ENUM_ALL_DEVICES - check all projects/folders from main memory,
							  and from *every* flash card.  (dwOid is not used)

			Plus any of the following (specifying which PROJECTS to check):
			PRJ_ENUM_ALL_PROJ- enumerate in all project (szProj is not used)

szProj		Only used for when (dwFlags & PRJ_ENUM_ALL_PROJ) is NOT set.  This
			specifies which project to search; NULL specifies to look for files
			that aren't in a project/folder (i.e., they're at the top level,
			under "\My Documents").

szFileName	pointer to name of file to search for. (i.e. '*.wav' or '*.*')

lParam		user defined parameter passed to pfnEnumProc


RETURN:
The total number of matching files found.  NOTE: in the regular version,
this is actually the total number of files in MAIN MEMORY ONLY (see below).


The regular version is provided for backward compatibility.  It will silently
skip over any file that isn't on an OID-based filesystem (it enumerates
everything in main memory, but will silently skip everything on storage cards).

The modern version, EnumProjectsFilesEx, requires the extended callback
function, too.
******************************************************************************}
function EnumProjectsFilesEx(pfnEnumProc:PROJECTSFILES_ENUMPROC_EX;
                             dwOidFlash:DWORD;
                             dwFlags:DWORD;
                             szProj:LPTSTR;
                             szFileName:LPTSTR;
                             lParam:LPARAM):longint; external Note_prjDLL name 'EnumProjectsFilesEx';

implementation

end.
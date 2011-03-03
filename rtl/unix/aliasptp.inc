{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2004 by Marco van de Voort
    Member of the Free Pascal development team

    Aliases for Unix base types and constants, to import them into
    multiple units in a typesafe way.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$i aliasctp.inc}

type
    dev_t    = UnixType.dev_t;
    TDev     = UnixType.TDev;
    pDev     = UnixType.pDev;
    gid_t    = UnixType.gid_t;
    TGid     = UnixType.TGid;
    TIOCtlRequest = UnixType.TIOCtlRequest;
    pGid     = UnixType.pGid;
    ino_t    = UnixType.ino_t;
    TIno     = UnixType.TIno;
    pIno     = UnixType.pIno;
    mode_t   = UnixType.mode_t;
    TMode    = UnixType.TMode;
    pMode    = UnixType.pMode;
    nlink_t  = UnixType.nlink_t;
    TnLink   = UnixType.TnLink;
    pnLink   = UnixType.pnLink;
    off_t    = UnixType.off_t;
    TOff     = UnixType.TOff;
    pOff     = UnixType.pOff;
    pid_t    = UnixType.pid_t;
    TPid     = UnixType.TPid;
    pPid     = UnixType.pPid;
    size_t   = UnixType.size_t;
    TSize    = UnixType.TSize;
    pSize    = UnixType.pSize;
    pSize_t  = UnixType.pSize_t;
    ssize_t  = UnixType.ssize_t;
    TsSize   = UnixType.TsSize;
    psSize   = UnixType.psSize;
    uid_t    = UnixType.uid_t;
    TUid     = UnixType.TUid;
    pUid     = UnixType.pUid;
    clock_t  = UnixType.clock_t;
    TClock   = UnixType.TClock;
    pClock   = UnixType.pClock;
    time_t   = UnixType.time_t;
    TTime    = UnixType.TTime;
    pTime    = UnixType.pTime;
    ptime_t  = UnixType.ptime_t;

    socklen_t= UnixType.socklen_t;
    TSocklen = UnixType.TSocklen;
    pSocklen = UnixType.pSocklen;

    timeval  = UnixType.timeval;
    ptimeval = UnixType.ptimeval;
    TTimeVal = UnixType.TTimeVal;
    timespec = UnixType.timespec;
    ptimespec= UnixType.ptimespec;
    Ttimespec= UnixType.Ttimespec;
    
    pthread_mutex_t   = UnixType.pthread_mutex_t;
    pthread_cond_t    = UnixType.pthread_cond_t;
    pthread_t         = UnixType.pthread_t;

    tstatfs  = UnixType.TStatFs;
    pstatfs  = UnixType.PStatFs;

CONST
    ARG_MAX       = UnixType.ARG_MAX;
    NAME_MAX      = UnixType.NAME_MAX;
    PATH_MAX      = UnixType.PATH_MAX;
    SYS_NMLN      = UnixType.SYS_NMLN;
    SIG_MAXSIG    = UnixType.SIG_MAXSIG;
//   wordsinsigset = UnixType.wordsinsigset;

    PRIO_PROCESS  = UnixType.PRIO_PROCESS;
    PRIO_PGRP	  = UnixType.PRIO_PGRP;
    PRIO_USER	  = UnixType.PRIO_USER;


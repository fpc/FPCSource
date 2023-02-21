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
    dev_t    = UT.dev_t;
    TDev     = UT.TDev;
    pDev     = UT.pDev;
    gid_t    = UT.gid_t;
    TGid     = UT.TGid;
    TIOCtlRequest = UT.TIOCtlRequest;
    pGid     = UT.pGid;
    ino_t    = UT.ino_t;
    TIno     = UT.TIno;
    pIno     = UT.pIno;
    mode_t   = UT.mode_t;
    TMode    = UT.TMode;
    pMode    = UT.pMode;
    nlink_t  = UT.nlink_t;
    TnLink   = UT.TnLink;
    pnLink   = UT.pnLink;
    off_t    = UT.off_t;
    TOff     = UT.TOff;
    pOff     = UT.pOff;
    pid_t    = UT.pid_t;
    TPid     = UT.TPid;
    pPid     = UT.pPid;
    size_t   = UT.size_t;
    TSize    = UT.TSize;
    pSize    = UT.pSize;
    pSize_t  = UT.pSize_t;
    ssize_t  = UT.ssize_t;
    TsSize   = UT.TsSize;
    psSize   = UT.psSize;
    uid_t    = UT.uid_t;
    TUid     = UT.TUid;
    pUid     = UT.pUid;
    clock_t  = UT.clock_t;
    TClock   = UT.TClock;
    pClock   = UT.pClock;
    time_t   = UT.time_t;
    TTime    = UT.TTime;
    pTime    = UT.pTime;
    ptime_t  = UT.ptime_t;

    socklen_t= UT.socklen_t;
    TSocklen = UT.TSocklen;
    pSocklen = UT.pSocklen;

    timeval  = UT.timeval;
    ptimeval = UT.ptimeval;
    TTimeVal = UT.TTimeVal;
    timespec = UT.timespec;
    ptimespec= UT.ptimespec;
    Ttimespec= UT.Ttimespec;
    
    pthread_mutex_t   = UT.pthread_mutex_t;
    pthread_cond_t    = UT.pthread_cond_t;
    pthread_t         = UT.pthread_t;

    tstatfs  = UT.TStatFs;
    pstatfs  = UT.PStatFs;

CONST
    ARG_MAX       = UT.ARG_MAX;
    NAME_MAX      = UT.NAME_MAX;
    PATH_MAX      = UT.PATH_MAX;
    SYS_NMLN      = UT.SYS_NMLN;
    SIG_MAXSIG    = UT.SIG_MAXSIG;
//   wordsinsigset = UT.wordsinsigset;

    PRIO_PROCESS  = UT.PRIO_PROCESS;
    PRIO_PGRP	  = UT.PRIO_PGRP;
    PRIO_USER	  = UT.PRIO_USER;


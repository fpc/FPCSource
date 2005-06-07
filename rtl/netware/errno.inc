  { -------------------------- Base POSIX-mandated constants ---------------  }
  { no such file or directory                      }
  const
     SYS_ENOENT = 1;      // arg list too big
     SYS_E2BIG = 2;       // arg list too big
     SYS_ENOEXEC = 3;     // exec format error
     SYS_EBADF = 4;       // bad file number
     SYS_ENOMEM = 5;      // not enough memory
     SYS_EACCES = 6;      // permission denied
     SYS_EEXIST = 7;      // file exists
     SYS_EXDEV = 8;       // cross-device link
     SYS_EINVAL = 9;      // invalid argument
     SYS_ENFILE = 10;     // file table overflow
     SYS_EMFILE = 11;     // too many open files
     SYS_ENOSPC = 12;     // no space left on device
     SYS_EDOM = 13;       // argument too large
     SYS_ERANGE = 14;     // result too large
     SYS_EDEADLK = 15;    // resource deadlock would occur
  { -------------------------- Miscellaneous NLM Library constants ---------  }
     SYS_EINUSE    = 16;  // resource(s) in use
     SYS_ESERVER   = 17;  // server error (memory out, I/O error, etc.)
     SYS_ENOSERVR  = 18;  // no server (queue server, file server, etc.)
     SYS_EWRNGKND  = 19;  // wrong kind--an operation is being...
                          // ...attempted on the wrong kind of object
     SYS_ETRNREST  = 20;  // transaction restarted
     SYS_ERESOURCE = 21;  // resources unavailable (maybe permanently)
     SYS_EBADHNDL  = 22;  // bad non-file handle (screen, semaphore, etc)
     SYS_ENO_SCRNS = 23;  // screen I/O attempted when no screens
  { -------------------------- Additional POSIX / traditional UNIX constants  }
     SYS_EAGAIN    = 24;  // resource temporarily unavailable
     SYS_ENXIO     = 25;  // no such device or address
     SYS_EBADMSG   = 26;  // not a data message
     SYS_EFAULT    = 27;  // bad address
     SYS_EIO       = 28;  // physical I/O error
     SYS_ENODATA   = 29;  // no data
     SYS_ENOSTRMS  = 30;  // streams not available
  { Berkeley sockets constants ------------------  }
     SYS_EPROTO    = 31;  // fatal protocol error
     SYS_EPIPE     = 32;  // broken pipe
     SYS_ESPIPE    = 33;  // illegal seek
  { Non-blocking and interrupt I/O constants ----  }
     SYS_ETIME     = 34;  // ioctl acknowledge timeout
  { operation would block                          }
     SYS_EWOULDBLOCK=35;  // operation would block
     SYS_EINPROGRESS=36;  // operation now in progress
     SYS_EALREADY  = 37;  // operation already in progress
  { IPC network argument constants --------------  }
     SYS_ENOTSOCK  = 38;  // socket operation on non-socket
     SYS_EDESTADDRREQ=39; // destination address required
     SYS_EMSGSIZE  = 40;  // message too long
     SYS_EPROTOTYPE= 41;  // protocol wrong type for socket
     SYS_ENOPROTOOPT=42;  // protocol not available
     SYS_EPROTONOSUPPORT = 43;  // protocol not supported
     SYS_ESOCKTNOSUPPORT = 44;  // socket type not supported
     SYS_EOPNOTSUPP      = 45;  // operation not supported on socket
     SYS_EPFNOSUPPORT    = 46;  // protocol family not supported
     SYS_EAFNOSUPPORT    = 47;  // address family unsupported by protocol family
     SYS_EADDRINUSE      = 48;  // address already in use
     SYS_EADDRNOTAVAIL   = 49;  // can't assign requested address
  { Operational constants -----------------------  }
     SYS_ENETDOWN        = 50;  // Network is down
  { network is unreachable                         }
     SYS_ENETUNREACH = 51;
  { network dropped connection on reset            }
     SYS_ENETRESET = 52;
  { software caused connection abort               }
     SYS_ECONNABORTED = 53;
  { connection reset by peer                       }
     SYS_ECONNRESET = 54;
  { no buffer space available                      }
     SYS_ENOBUFS = 55;
  { socket is already connected                    }
     SYS_EISCONN = 56;
  { socket is not connected                        }
     SYS_ENOTCONN = 57;
  { can't send after socket shutdown               }
     SYS_ESHUTDOWN = 58;
  { too many references: can't splice              }
     SYS_ETOOMANYREFS = 59;
  { connection timed out                           }
     SYS_ETIMEDOUT = 60;
  { connection refused                             }
     SYS_ECONNREFUSED = 61;
  { -------------------------- Additional POSIX-mandated constants ---------  }
  { resource busy                                  }
     SYS_EBUSY = 62;
  { interrupted function call                      }
     SYS_EINTR = 63;
  { is a directory                                 }
     SYS_EISDIR = 64;
  { filename too long                              }
     SYS_ENAMETOOLONG = 65;
  { function not implemented                       }
     SYS_ENOSYS = 66;
  { not a directory                                }
     SYS_ENOTDIR = 67;
  { directory not empty                            }
     SYS_ENOTEMPTY = 68;
  { operation not permitted                        }
     SYS_EPERM = 69;
  { no child process                               }
     SYS_ECHILD = 70;
  { file too large                                 }
     SYS_EFBIG = 71;
  { too many links                                 }
     SYS_EMLINK = 72;
     SYS_ELOOP  = SYS_EMLINK;
  { no such device                                 }
     SYS_ENODEV = 73;
  { no locks available                             }
     SYS_ENOLCK = 74;
  { inappropriate I/O control operation            }
     SYS_ENOTTY = 75;
  { inappropriate operation for file type      }
     SYS_EFTYPE = SYS_ENOTTY;
  { read-only file system                          }
     SYS_EROFS = 76;
  { no such process                                }
     SYS_ESRCH = 77;
  { operation was cancelled                        }
     SYS_ECANCELED = 78;
  { this optional functionality not supported      }
     SYS_ENOTSUP = 79;
  { -------------------------- CLib-implementation-specific constants ------  }
     SYS_ECANCELLED = SYS_ECANCELED;
  { anomaly in NLM data structure                  }
     SYS_ENLMDATA = 100;
  { illegal character sequence in multibyte        }
     SYS_EILSEQ = 101;
  { internal library inconsistency                 }
     SYS_EINCONSIS = 102;
  { DOS-text file inconsistency--no newline...     }
     SYS_EDOSTEXTEOL = 103;
  { ...after carriage return                       }
  { object doesn't exist                           }
     SYS_ENONEXTANT = 104;
     SYS_ENOCONTEXT = 105;        // no thread library context present
     SYS_ELASTERR = SYS_ENOCONTEXT;

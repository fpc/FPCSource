
// copy paste of Solaris checked constants from Unix.pp. Not checked.

{********************
      Process
********************}

const
  { For getting/setting priority }
  Prio_Process = 0;
  Prio_PGrp    = 1;
  Prio_User    = 2;

  WNOHANG      = $100;
  WUNTRACED    = $4;

  F_GetFd  = 1;
  F_SetFd  = 2;
  F_GetFl  = 3;
  F_SetFl  = 4;

  // zlowaris specific
  F_DupFd      = 0;
  F_Dup2Fd     = 9;
  F_GetOwn     = 23;
  F_SetOwn     = 24;
  F_GetLk      = 14;
  F_SetLk      = 6;
  F_SetLkW     = 7;
  F_FreeSp     = 11;


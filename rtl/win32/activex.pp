{$MODE OBJFPC}
unit activex;

  interface

{$ifdef HASINTERFACES}

    uses
       windows;

    type
       polestr = PWideChar;
       largeint = int64;

       tagSTATSTG = record
          pwcsName : POleStr;
          dwType : DWord;
          cbSize : Largeint;
          mtime : TFileTime;
          ctime : TFileTime;
          atime : TFileTime;
          grfMode : DWord;
          grfLocksSupported : DWord;
          clsid : TCLSID;
          grfStateBits : DWord;
          reserved : DWord;
       end;

       TStatStg = tagSTATSTG;
       PStatStg = ^TStatStg;
       STATSTG = TStatStg;

       ISequentialStream = interface(IUnknown)
          ['{0c733a30-2a1c-11ce-ade5-00aa0044773d}']
          function Read(pv : Pointer;cb : DWord;pcbRead : PDWord) : HRESULT;stdcall;
          function Write(pv : Pointer;cb : DWord;pcbWritten : PDWord): HRESULT;stdcall;
       end;

       IStream = interface(ISequentialStream)
          ['{0000000C-0000-0000-C000-000000000046}']
          function Seek(dlibMove : Largeint; dwOrigin: Longint;
            out libNewPosition : Largeint): HResult; stdcall;
          function SetSize(libNewSize : Largeint) : HRESULT;stdcall;
          function CopyTo(stm: IStream;cb : Largeint;out cbRead : Largeint;
            out cbWritten: Largeint) : HRESULT;stdcall;
          function Commit(grfCommitFlags : Longint) : HRESULT; stdcall;
          function Revert : HRESULT; stdcall;
          function LockRegion(libOffset : Largeint;cb : Largeint;
            dwLockType: Longint) : HRESULT;stdcall;
          function UnlockRegion(libOffset: Largeint;cb: Largeint;
            dwLockType: Longint) : HRESULT;stdcall;
          function Stat(out statstg : TStatStg; grfStatFlag: Longint): HRESULT;stdcall;
          function Clone(out stm : IStream) : HRESULT; stdcall;
       end;

{$endif HASINTERFACES}

   implementation

end.
{
  $Log$
  Revision 1.1  2001-08-19 21:02:02  florian
    * fixed and added a lot of stuff to get the Jedi DX( headers
      compiled

}

{%MainUnit ndk.pas}
{
    Native Development Kit for Native NT

    This file is part of the Free Pascal run time library.
    This units contains PEB and TEB structures used by NT
    Copyright (c) 2010 by Sven Barth

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

const
  GDI_HANDLE_BUFFER_SIZE_32 = 34;
  GDI_HANDLE_BUFFER_SIZE_64 = 60;
{$ifdef cpu64}
  GDI_HANDLE_BUFFER_SIZE = GDI_HANDLE_BUFFER_SIZE_64;
{$else}
  GDI_HANDLE_BUFFER_SIZE = GDI_HANDLE_BUFFER_SIZE_32;
{$endif}

{ TODO : add bitness specific records }
{ TODO : add OS version specific records }
{ TODO : define remaining types }

type
  { The PEB comes in three flavors: the platform dependant one, the 32 bit one
    and the 64 bit one. Only the first one should be used by natve processes.
    The others are needed if one e.g. wants to examine the PEB of a 32 bit
    process on a 64 bit system.
  }

  _PEB = packed record
      InheritedAddressSpace: NT_BOOLEAN;
      ReadImageFileExecOptions: NT_BOOLEAN;
      BeingDebugged: NT_BOOLEAN;
(*  #if (NTDDI_VERSION >= NTDDI_WS03)
      union
      {
          BOOLEAN BitField;
          struct
          {
              BOOLEAN ImageUsesLargePages:1;
  #if (NTDDI_VERSION >= NTDDI_LONGHORN)
              BOOLEAN IsProtectedProcess:1;
              BOOLEAN IsLegacyProcess:1;
              BOOLEAN IsImageDynamicallyRelocated:1;
              BOOLEAN SkipPatchingUser32Forwarders:1;
              BOOLEAN SpareBits:3;
  #else
              BOOLEAN SpareBits:7;
  #endif
          };
      };
  #else*)
      SpareBool: NT_BOOLEAN;
//  #endif
      Mutant: PHANDLE;
      ImageBaseAddress: PVOID;
      Ldr: Pointer;//PPEB_LDR_DATA;
      ProcessParameters: PRTL_USER_PROCESS_PARAMETERS;
      SubSystemData: PVOID;
      ProcessHeap: PVOID;
      FastPebLock: Pointer; //PRTL_CRITICAL_SECTION;
(*  #if (NTDDI_VERSION >= NTDDI_LONGHORN)
      PTR(PVOID) AltThunkSListPtr;
      PTR(PVOID) IFEOKey;
      union
      {
          ULONG CrossProcessFlags;
          struct
          {
              ULONG ProcessInJob:1;
              ULONG ProcessInitializing:1;
              ULONG ProcessUsingVEH:1;
              ULONG ProcessUsingVCH:1;
              ULONG ReservedBits0:28;
          };
      };
      union
      {
          PTR(PVOID) KernelCallbackTable;
          PTR(PVOID) UserSharedInfoPtr;
      };
  #elif (NTDDI_VERSION >= NTDDI_WS03)
      PTR(PVOID) AltThunkSListPtr;
      PTR(PVOID) SparePtr2;
      ULONG EnvironmentUpdateCount;
      PTR(PVOID) KernelCallbackTable;
  #else*)
      FastPebLockRoutine: Pointer; //PPEBLOCKROUTINE;
      FastPebUnlockRoutine: Pointer; //PPEBLOCKROUTINE;
      EnvironmentUpdateCount: ULONG;
      KernelCallbackTable: PVOID;
//  #endif
      SystemReserved: array[0..0] of ULONG;
      SpareUlong: ULONG; // AtlThunkSListPtr32
      FreeList: Pointer; //PPEB_FREE_BLOCK;
      TlsExpansionCounter: ULONG;
      TlsBitmap: PVOID;
      TlsBitmapBits: array[0..1] of ULONG;
      ReadOnlySharedMemoryBase: PVOID;
(*  #if (NTDDI_VERSION >= NTDDI_LONGHORN)
      PTR(PVOID) HotpatchInformation;
  #else*)
      ReadOnlySharedMemoryHeap: PVOID;
//  #endif
      ReadOnlyStaticServerData: PPVOID;
      AnsiCodePageData: PVOID;
      OemCodePageData: PVOID;
      UnicodeCaseTableData: PVOID;
      NumberOfProcessors: ULONG;
      NtGlobalFlag: ULONG;
      CriticalSectionTimeout: LARGE_INTEGER;
      HeapSegmentReserve: ULONG_PTR;
      HeapSegmentCommit: ULONG_PTR;
      HeapDeCommitTotalFreeThreshold: ULONG_PTR;
      HeapDeCommitFreeBlockThreshold: ULONG_PTR;
      NumberOfHeaps: ULONG;
      MaximumNumberOfHeaps: ULONG;
      ProcessHeaps: PPVOID;
      GdiSharedHandleTable: PVOID;
      ProcessStarterHelper: PVOID;
      GdiDCAttributeList: ULONG;
      LoaderLock: Pointer; //PRTL_CRITICAL_SECTION;
      OSMajorVersion: ULONG;
      OSMinorVersion: ULONG;
      OSBuildNumber: USHORT;
      OSCSDVersion: USHORT;
      OSPlatformId: ULONG;
      ImageSubsystem: ULONG;
      ImageSubsystemMajorVersion: ULONG;
      ImageSubsystemMinorVersion: ULONG;
      ImageProcessAffinityMask: ULONG_PTR;
      GdiHandleBuffer: array[0..GDI_HANDLE_BUFFER_SIZE-1] of ULONG;
      PostProcessInitRoutine: Pointer; //PPOST_PROCESS_INIT_ROUTINE;
      TlsExpansionBitmap: PVOID;
      TlsExpansionBitmapBits: array[0..31] of ULONG;
      SessionId: ULONG;
{  #if (NTDDI_VERSION >= NTDDI_WINXP)
      ULARGE_INTEGER AppCompatFlags;
      ULARGE_INTEGER AppCompatFlagsUser;
      PTR(PVOID) pShimData;
      PTR(PVOID) AppCompatInfo;
      STRUCT(UNICODE_STRING) CSDVersion;
      PTR(struct _ACTIVATION_CONTEXT_DATA*) ActivationContextData;
      PTR(struct _ASSEMBLY_STORAGE_MAP*) ProcessAssemblyStorageMap;
      PTR(struct _ACTIVATION_CONTEXT_DATA*) SystemDefaultActivationContextData;
      PTR(struct _ASSEMBLY_STORAGE_MAP*) SystemAssemblyStorageMap;
      PTR(ULONG_PTR) MinimumStackCommit;
  #endif
  #if (NTDDI_VERSION >= NTDDI_WS03)
      PTR(PVOID*) FlsCallback;
      STRUCT(LIST_ENTRY) FlsListHead;
      PTR(PVOID) FlsBitmap;
      ULONG FlsBitmapBits[4];
      ULONG FlsHighIndex;
  #endif
  #if (NTDDI_VERSION >= NTDDI_LONGHORN)
      PTR(PVOID) WerRegistrationData;
      PTR(PVOID) WerShipAssertPtr;
  #endif}
  end;
  PEB = _PEB;
  PPEB = ^PEB;


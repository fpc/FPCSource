{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2005 Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
  commctrl.h

  Declarations for commctrl WinCE API

  Changes :

  09-28-2005 : orinaudo@gmail.com,  WCE 4.21 SE, First release
  09-02-2006 : updated
  
}

{exported functions list = to do,
 * please remove functions done *
 
     Exports

       ordinal    name

            73    AddMRUData
            66    AddMRUStringA
            67    AddMRUStringW
            35    CenterWindow
            39    CommandBands_AddAdornments
            37    CommandBands_AddBands
            36    CommandBands_Create
            38    CommandBands_GetCommandBar
            41    CommandBands_GetRestoreInformation
            40    CommandBands_Show
            10    CommandBar_AddAdornments
             5    CommandBar_AddBitmap
             3    CommandBar_Create
            43    CommandBar_DrawMenuBar
            11    CommandBar_GetItemWindow
             9    CommandBar_GetMenu
            12    CommandBar_Height
             6    CommandBar_InsertComboBox
             7    CommandBar_InsertControl
             8    CommandBar_InsertMenubar
            42    CommandBar_InsertMenubarEx
             4    CommandBar_Show
            63    CreateMRUListA
            64    CreateMRUListW
            19    CreatePropertySheetPageW
            17    CreateStatusWindowW
            15    CreateToolbar
            16    CreateToolbarEx
            14    CreateUpDownControl
            50    DPA_Create
            61    DPA_DeleteAllPtrs
            60    DPA_DeletePtr
            31    DPA_Destroy
            56    DPA_DestroyCallback
            32    DPA_GetPtr
            62    DPA_GetPtrIndex
            54    DPA_Grow
            51    DPA_InsertPtr
            59    DPA_Search
            53    DPA_SetPtr
            52    DPA_Sort
            46    DSA_Clone
            23    DSA_Create
            30    DSA_DeleteAllItems
            29    DSA_DeleteItem
            24    DSA_Destroy
            47    DSA_DestroyCallback
            48    DSA_EnumCallback
            25    DSA_GetItem
            26    DSA_GetItemPtr
            27    DSA_InsertItem
            45    DSA_Search
            28    DSA_SetItem
            58    DSA_SetRange
            57    DSA_Sort
            68    DelMRUString
            20    DestroyPropertySheetPage
            49    DoReaderMode
            21    DrawStatusTextW
            71    EnumMRUListA
            72    EnumMRUListW
            74    FindMRUData
            69    FindMRUStringA
            70    FindMRUStringW
            65    FreeMRUList
            22    InvertRect
            55    IsCapEditAvailable
            13    IsCommandBarMessage
                  ListView_SetItemSpacing
            75    PopulateComboWithMRU
            76    PopulateMenuWithMRU
            18    PropertySheetW
            80    SHCreateDefaultGradient
            79    SHDrawGradient
            81    SHDrawText
            77    SHGetSysColor
            78    SHGetSystemMetrics
            82    SHSetSysColors
            83    SHSetSystemMetrics
            34    StrToIntW
            33    Str_SetPtrW
}

{$ifdef read_interface}

//*****************************************************************************
// consts
//*****************************************************************************

//*****************************************************************************
// types
//*****************************************************************************

//*****************************************************************************
// functions
//*****************************************************************************


{$endif read_interface}

{ already defined in func.inc
procedure InitCommonControls; external ComctlDLL name 'InitCommonControls';
function InitCommonControlsEx(LPINITCOMMONCONTROLSEX):WINBOOL; external ComctlDLL name 'InitCommonControlsEx';
}

{$ifdef read_implementation}

{$endif read_implementation}



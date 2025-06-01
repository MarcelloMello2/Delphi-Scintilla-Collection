unit IconsEx;

interface

uses Windows, Classes, SysUtils;

type
  PByte = ^Byte;
  PBitmapInfo = ^BitmapInfo;

/// These first two structs represent how the icon information is stored
/// when it is bound into a EXE or DLL file. Structure members are WORD
/// aligned and the last member of the structure is the ID instead of
/// the imageoffset.

type
  PMEMICONDIRENTRY = ^TMEMICONDIRENTRY;
  TMEMICONDIRENTRY = packed record
    bWidth: Byte;
    bHeight: Byte;
    bColorCount: Byte;
    bReserved: Byte;
    wPlanes: Word;
    wBitCount: Word;
    dwBytesInRes: DWORD;
    nID: Word;
  end;

type
  PMEMICONDIR = ^TMEMICONDIR;
  TMEMICONDIR = packed record
    idReserved: Word;
    idType: Word;
    idCount: Word;
    idEntries: Array[0..15] of TMEMICONDIRENTRY;
  end;

/// These next two structs represent how the icon information is stored
/// in an ICO file.

type
  PICONDIRENTRY = ^TICONDIRENTRY;
  TICONDIRENTRY = packed record
    bWidth: Byte;
    bHeight: Byte;
    bColorCount: Byte;
    bReserved: Byte;
    wPlanes: Word;
    wBitCount: Word;
    dwBytesInRes: DWORD;
    dwImageOffset: DWORD;
  end;

type
  PICONDIR = ^TICONDIR;
  TICONDIR = packed record
    idReserved: Word;
    idType: Word;
    idCount: Word;
    idEntries: Array[0..0] of TICONDIRENTRY;
  end;

/// The following two structs are for the use of this program in
/// manipulating icons. They are more closely tied to the operation
/// of this program than the structures listed above. One of the
/// main differences is that they provide a pointer to the DIB
/// information of the masks.

type
  PICONIMAGE = ^TICONIMAGE;
  TICONIMAGE = packed record
    Width,
    Height,
    Colors: UINT;
    lpBits: Pointer;
    dwNumBytes: DWORD;
    pBmpInfo: PBitmapInfo;
  end;


type
  PICONRESOURCE = ^TICONRESOURCE;
  TICONRESOURCE = packed record
    nNumImages: UINT;
    IconImages: Array[0..15] of TICONIMAGE;
  end;

type
  TPageInfo = packed record
    Width: Byte;
    Height: Byte;
    ColorQuantity: Integer;
    Reserved: DWORD;
    PageSize: DWORD;
    PageOffSet: DWORD;
  end;

type
  TPageDataHeader = packed record
    PageHeadSize: DWORD;
    XSize: DWORD;
    YSize: DWORD;
    SpeDataPerPixSize: Integer;
    ColorDataPerPixSize: Integer;
    Reserved: DWORD;
    DataAreaSize: DWORD;
    ReservedArray: Array[0..15] of char;
  end;

type
  TIcoFileHeader = packed record
    FileFlag: Array[0..3] of byte;
    PageQuartity: Integer;
    PageInfo: TPageInfo;
  end;
var
  ResourceName: String;
  
function ExtractIconToStream(ResFileName: string; Stream: TStream; nIndex: string): Boolean;
function WriteIconResourceToStream(Stream: TStream; lpIR: PICONRESOURCE): Boolean;
function GetResourceList(FileName: String; List: TStrings): Integer;

implementation

function WriteICOHeader(Stream: TStream; nNumEntries: UINT): Boolean;
type
  TFIcoHeader = record
    wReserved: WORD;
    wType: WORD;
    wNumEntries: WORD;
  end;
var
  IcoHeader: TFIcoHeader;
begin
  IcoHeader.wReserved := 0;
  IcoHeader.wType := 1;
  IcoHeader.wNumEntries := WORD(nNumEntries);
  Stream.Write(IcoHeader, SizeOf(IcoHeader));
  Result := True;
end;

function CalculateImageOffset(lpIR: PICONRESOURCE; nIndex: UINT): DWORD;
var
  dwSize: DWORD;
  i: Integer;
begin
  dwSize := 3 * SizeOf(WORD);
  inc(dwSize, lpIR.nNumImages * SizeOf(TICONDIRENTRY));
  for i := 0 to nIndex - 1 do
    inc(dwSize, lpIR.IconImages[i].dwNumBytes);
  Result := dwSize;
end;

function WriteIconResourceToStream(Stream: TStream; lpIR: PICONRESOURCE): Boolean;
var
  i: UINT;
  ide: TICONDIRENTRY;
  dwTemp: DWORD;
begin
  for i := 0 to lpIR^.nNumImages - 1 do
  begin
    /// Convert internal format to ICONDIRENTRY
    ide.bWidth := lpIR^.IconImages[i].Width;
    ide.bHeight := lpIR^.IconImages[i].Height;
    ide.bReserved := 0;
    ide.wPlanes := lpIR^.IconImages[i].pBmpInfo.bmiHeader.biPlanes;
    ide.wBitCount := lpIR^.IconImages[i].pBmpInfo.bmiHeader.biBitCount;
    if ide.wPlanes * ide.wBitCount >= 8 then
      ide.bColorCount := 0
    else
      ide.bColorCount := 1 shl (ide.wPlanes * ide.wBitCount);
    ide.dwBytesInRes := lpIR^.IconImages[i].dwNumBytes;
    ide.dwImageOffset := CalculateImageOffset(lpIR, i);
    Stream.Write(ide, sizeof(TICONDIRENTRY));
  end;
  for i := 0 to lpIR^.nNumImages - 1 do
  begin
    dwTemp := lpIR^.IconImages[i].pBmpInfo^.bmiHeader.biSizeImage;
    lpIR^.IconImages[i].pBmpInfo^.bmiHeader.biSizeImage := 0;
    Stream.Write(lpIR^.IconImages[i].lpBits^, lpIR^.IconImages[i].dwNumBytes);
    lpIR^.IconImages[i].pBmpInfo^.bmiHeader.biSizeImage := dwTemp;
  end;
  Result := True;
end;

function AdjustIconImagePointers(lpImage: PICONIMAGE): Bool;
begin
  if lpImage = nil then
  begin
    Result := False;
    exit;
  end;
  lpImage.pBmpInfo := PBitMapInfo(lpImage^.lpBits);
  lpImage.Width := lpImage^.pBmpInfo^.bmiHeader.biWidth;
  lpImage.Height := (lpImage^.pBmpInfo^.bmiHeader.biHeight) div 2;
  lpImage.Colors := lpImage^.pBmpInfo^.bmiHeader.biPlanes * lpImage^.pBmpInfo^.bmiHeader.biBitCount;
  Result := true;
end;

function ExpandEnvironment(const strValue: string): string;
var
  chrResult: array[0..1023] of Char;
  wrdReturn: DWORD;
begin
  wrdReturn := ExpandEnvironmentStrings(PChar(strValue), chrResult, 1024);
  if wrdReturn = 0 then
    Result := strValue
  else 
  begin
    Result := Trim(chrResult);
  end;
end;

function EnumResourceNamesProc(Module: HMODULE; ResType: PChar; ResName: PChar;
  lParam: TStringList): Integer; stdcall;
begin
  if hiword(Cardinal(ResName)) = 0 then
  begin
    ResourceName := IntToStr(loword(Cardinal(ResName)));
  end else
  begin
    ResourceName := ResName;
  end;
  lParam.Add(ResourceName);
  Result := 1;
end;

function GetResourceList(FileName: String; List: TStrings): Integer;
var
  hExe: THandle;
begin
  List.Clear;
  Result := List.Count;
  FileName := ExpandEnvironment(FileName);
  hExe := LoadLibraryEx(PChar(Filename), 0, LOAD_LIBRARY_AS_DATAFILE);
  if hExe = 0 then Exit;
  EnumResourceNames(hExe, RT_GROUP_ICON, @EnumResourceNamesProc, Integer(List));
  FreeLibrary(hExe);
  Result := List.Count;
end;

function ExtractIconToStream(ResFileName: string; Stream: TStream; nIndex: string): Boolean;
var
  h: HMODULE;
  lpMemIcon: PMEMICONDIR;
  lpIR: TICONRESOURCE;
  src: HRSRC;
  Global: HGLOBAL;
  i: integer;
begin
  Result := False;
  ResFileName := ExpandEnvironment(ResFileName);
  h := LoadLibraryEx(pchar(ResFileName), 0, LOAD_LIBRARY_AS_DATAFILE);
  if h = 0 then exit;
  try
    src := FindResource(h, pchar(nIndex), RT_GROUP_ICON);
    if src = 0 then
      Src := FindResource(h, Pointer(StrToInt(nIndex)), RT_GROUP_ICON);
    if src <> 0 then
    begin
      Global := LoadResource(h, src);
      if Global <> 0 then
      begin
        lpMemIcon := LockResource(Global);
        if Global <> 0 then
        begin
          try
            lpIR.nNumImages := lpMemIcon.idCount;
            // Write the header
            for i := 0 to lpMemIcon^.idCount - 1 do
            begin
              src := FindResource(h, MakeIntResource(lpMemIcon^.idEntries[i].nID), RT_ICON);
              if src <> 0 then
              begin
                Global := LoadResource(h, src);
                if Global <> 0 then
                begin
                  try
                    lpIR.IconImages[i].dwNumBytes := SizeofResource(h, src);
                  except
                    MessageBox(0, PChar('Unable to Read Icon'), 'NTPacker', MB_ICONERROR);
                    Result := False;
                    ExitProcess(0);
                  end;
                  GetMem(lpIR.IconImages[i].lpBits, lpIR.IconImages[i].dwNumBytes);
                  CopyMemory(lpIR.IconImages[i].lpBits, LockResource(Global), lpIR.IconImages[i].dwNumBytes);
                  if not AdjustIconImagePointers(@(lpIR.IconImages[i])) then exit;
                end;
              end;
            end;
            if WriteICOHeader(Stream, lpIR.nNumImages) then
              if WriteIconResourceToStream(Stream, @lpIR) then
                Result := True;
          finally
            for i := 0 to lpIR.nNumImages - 1 do
              if assigned(lpIR.IconImages[i].lpBits) then
                FreeMem(lpIR.IconImages[i].lpBits);
          end;
        end;
      end;
    end;
  finally
    FreeLibrary(h);
  end;
end;

end.

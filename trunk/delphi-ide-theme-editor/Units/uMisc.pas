//**************************************************************************************************
//
// Unit uMisc
// unit uMisc  for the Delphi IDE Theme Editor
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is uMisc.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2015 Rodrigo Ruz V.
// All Rights Reserved.
//
//**************************************************************************************************
unit uMisc;

interface
{$I ..\Common\Jedi.inc}

uses
 Windows,
 Classes,
 Graphics,
 PngImage,
 ImgList;


procedure ExtractIconFileToImageList(ImageList: TCustomImageList; const Filename: string);
procedure ExtractIconFile(Icon: TIcon; const Filename: string;IconType : Cardinal);
function  GetFileVersion(const FileName: string): string;
function  IsAppRunning(const FileName: string): boolean;
function  GetLocalAppDataFolder: string;
function  GetAppDataFolder: string;
function  GetSpecialFolderLocation(nFolder: Integer): string;
function  GetTempDirectory: string;
procedure MsgBox(const Msg: string);
function  EnumFixedFontsProc(var LogFont: TLogFont; var TextMetric: TTextMetric;  FontType: integer; Data: Pointer): integer; stdcall;
function  EnumFontsProc(var LogFont: TLogFont; var TextMetric: TTextMetric;  FontType: integer; Data: Pointer): integer; stdcall;
procedure CreateArrayBitmap(Width,Height:Word;Colors: Array of TColor;var Bitmap : TBitmap);
function  GetSpecialFolder(const CSIDL: integer) : string;
function  IsUACEnabled: Boolean;
procedure RunAsAdmin(const FileName, Params: string; hWnd: HWND = 0);
function  CurrentUserIsAdmin: Boolean;
function  RunAndWait(hWnd: HWND; const FileName, Params: string;RunAs:Boolean=False) : Boolean;
function  MakeValidTagName(const s: string): string;
function  GetModuleLocation : string;
function  WM_To_String(const WM_Message: Integer): string;
function  GetWindowClassName(Window: HWND): String;
function  TryStrToColor(const StrColor : string; Default : TColor) : TColor;
procedure GetLoadedModules(List : TStrings;Const OnlyNames:Boolean);
procedure CropPNG(Source: TPngImage; Left, Top, Width, Height: Integer; out Target: TPngImage);
procedure CheckForUpdates(Silent : Boolean);
function  IsHighlightColor(Color: TColor): Boolean;
function  GetFontHeight(const FontName : string; FontSize: Integer) : integer;

implementation

uses
  Forms,
  ActiveX,
  ShlObj,
  PsAPI,
  tlhelp32,
  ComObj,
  CommCtrl,
  StrUtils,
  ShellAPI,
  Controls,
  Dialogs,
 {$IFDEF DELPHIXE2_UP}
  System.UITypes,
 {$ENDIF}
  SHFolder,
  Registry,
  SysUtils;

Const
 SECURITY_NT_AUTHORITY: TSIDIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 5));
 SECURITY_BUILTIN_DOMAIN_RID = $00000020;
 DOMAIN_ALIAS_RID_ADMINS     = $00000220;
 DOMAIN_ALIAS_RID_USERS      = $00000221;
 DOMAIN_ALIAS_RID_GUESTS     = $00000222;
 DOMAIN_ALIAS_RID_POWER_USERS= $00000223;




function CheckTokenMembership(TokenHandle: THandle; SidToCheck: PSID; var IsMember: BOOL): BOOL; stdcall; external advapi32;

function GetFontHeight(const FontName : string; FontSize: Integer) : integer;
var
  LBitmap : TBitmap;
begin
    LBitmap:=TBitmap.Create;
    try
      LBitmap.SetSize(32,32);
      LBitmap.Canvas.Font.Name:=FontName;
      LBitmap.Canvas.Font.Size:=FontSize;
      Result:=LBitmap.Canvas.TextHeight('Wq');
    finally
      LBitmap.free;
    end;
end;


function IsHighlightColor(Color: TColor): Boolean;
var
  R, G, B : Byte;
begin
  R:= GetRValue(Color);
  G:= GetGValue(Color);
  B:= GetBValue(Color);
  Result:= Sqrt( (R*R*0.241)+(G*G*0.691)+(B*B*0.068))>130;
end;


function  TryStrToColor(const StrColor : string; Default : TColor) : TColor;
begin
   try
    Result:= StringToColor(StrColor);
   except
    Result:= Default;
   end;
end;

procedure CheckForUpdates(Silent : Boolean);
var
  LBinaryPath : string;
begin
  LBinaryPath:=GetModuleLocation();
  if Silent then
   ShellExecute(0, 'open', PChar(ExtractFilePath(LBinaryPath)+'Updater.exe'), PChar(Format('"%s" -Silent', [GetModuleLocation])), '', SW_SHOWNORMAL)
  else
   ShellExecute(0, 'open', PChar(ExtractFilePath(LBinaryPath)+'Updater.exe'), PChar(Format('"%s"', [GetModuleLocation])), '', SW_SHOWNORMAL);
end;



procedure CropPNG(Source: TPngImage; Left, Top, Width, Height: Integer; out Target: TPngImage);

  function ColorToTriple(Color: TColor): TRGBTriple;
  begin
    Color := ColorToRGB(Color);
    Result.rgbtBlue := Color shr 16 and $FF;
    Result.rgbtGreen := Color shr 8 and $FF;
    Result.rgbtRed := Color and $FF;
  end;

var
   X, Y: Integer;
   LBitmap: TBitmap;
   LRGBLine: PRGBLine;
   AlphaLineA, AlphaLineB: PngImage.PByteArray;
begin
  if (Source.Width < (Left + Width)) or (Source.Height < (Top + Height)) then
    raise Exception.Create('Invalid position/size');

  LBitmap := TBitmap.Create;
  try
    LBitmap.Width := Width;
    LBitmap.Height := Height;
    LBitmap.PixelFormat := pf24bit;

    for Y := 0 to LBitmap.Height - 1 do
    begin
      LRGBLine := LBitmap.Scanline[Y];
      for X := 0 to LBitmap.Width - 1 do
        LRGBLine^[X] := ColorToTriple(Source.Pixels[Left + X, Top + Y]);
    end;

    Target := TPngImage.Create;
    Target.Assign(LBitmap);
  finally
    LBitmap.Free;
  end;

  if Source.Header.ColorType in [COLOR_GRAYSCALEALPHA, COLOR_RGBALPHA] then begin
    Target.CreateAlpha;
    for Y := 0 to Target.Height - 1 do begin
      AlphaLineA := Source.AlphaScanline[Top + Y];
      AlphaLineB := Target.AlphaScanline[Y];
      for X := 0 to Target.Width - 1 do
        AlphaLineB^[X] := AlphaLineA^[X + Left];
    end;
  end;
end;

procedure GetLoadedModules(List : TStrings;Const OnlyNames:Boolean);
var
  hSnapshot: THandle;
  lpme: TModuleEntry32;
  Exists: Boolean;
begin
  List.Clear;
  hSnapshot := CreateToolhelp32Snapshot(TH32CS_SNAPMODULE,  GetCurrentProcessId());
  try
    if hSnapshot =  INVALID_HANDLE_VALUE then Exit;
    lpme.dwSize := SizeOf(lpme);
    Exists := Module32First(hSnapshot, lpme);
    while Exists do
    begin

      if OnlyNames then
       List.Add(ExtractFileName(lpme.szExePath))
      else
       List.Add(lpme.szExePath);

      Exists := Module32Next(hSnapshot, lpme);
    end;
  finally
    if hSnapshot <> INVALID_HANDLE_VALUE then
      CloseHandle(hSnapshot);
  end;
end;

function GetWindowClassName(Window: HWND): String;
var
  sClassName: PChar;
begin
  GetMem(sClassName, 256);
  try
    if GetClassName(Window, sClassName, 256)<>0 then
      Result := String(sClassName)
    else
      Result:='';
  finally
    FreeMem(sClassName, 256);
  end;
end;


function WM_To_String(const WM_Message: Integer): string;
begin
  case WM_Message of
    $0000: Result := 'WM_NULL';
    $0001: Result := 'WM_CREATE';
    $0002: Result := 'WM_DESTROY';
    $0003: Result := 'WM_MOVE';
    $0005: Result := 'WM_SIZE';
    $0006: Result := 'WM_ACTIVATE';
    $0007: Result := 'WM_SETFOCUS';
    $0008: Result := 'WM_KILLFOCUS';
    $000A: Result := 'WM_ENABLE';
    $000B: Result := 'WM_SETREDRAW';
    $000C: Result := 'WM_SETTEXT';
    $000D: Result := 'WM_GETTEXT';
    $000E: Result := 'WM_GETTEXTLENGTH';
    $000F: Result := 'WM_PAINT';
    $0010: Result := 'WM_CLOSE';
    $0011: Result := 'WM_QUERYENDSESSION';
    $0012: Result := 'WM_QUIT';
    $0013: Result := 'WM_QUERYOPEN';
    $0014: Result := 'WM_ERASEBKGND';
    $0015: Result := 'WM_SYSCOLORCHANGE';
    $0016: Result := 'WM_EndSESSION';
    $0017: Result := 'WM_SYSTEMERROR';
    $0018: Result := 'WM_SHOWWINDOW';
    $0019: Result := 'WM_CTLCOLOR';
    $001A: Result := 'WM_WININICHANGE or WM_SETTINGCHANGE';
    $001B: Result := 'WM_DEVMODECHANGE';
    $001C: Result := 'WM_ACTIVATEAPP';
    $001D: Result := 'WM_FONTCHANGE';
    $001E: Result := 'WM_TIMECHANGE';
    $001F: Result := 'WM_CANCELMODE';
    $0020: Result := 'WM_SETCURSOR';
    $0021: Result := 'WM_MOUSEACTIVATE';
    $0022: Result := 'WM_CHILDACTIVATE';
    $0023: Result := 'WM_QUEUESYNC';
    $0024: Result := 'WM_GETMINMAXINFO';
    $0026: Result := 'WM_PAINTICON';
    $0027: Result := 'WM_ICONERASEBKGND';
    $0028: Result := 'WM_NEXTDLGCTL';
    $002A: Result := 'WM_SPOOLERSTATUS';
    $002B: Result := 'WM_DRAWITEM';
    $002C: Result := 'WM_MEASUREITEM';
    $002D: Result := 'WM_DELETEITEM';
    $002E: Result := 'WM_VKEYTOITEM';
    $002F: Result := 'WM_CHARTOITEM';
    $0030: Result := 'WM_SETFONT';
    $0031: Result := 'WM_GETFONT';
    $0032: Result := 'WM_SETHOTKEY';
    $0033: Result := 'WM_GETHOTKEY';
    $0037: Result := 'WM_QUERYDRAGICON';
    $0039: Result := 'WM_COMPAREITEM';
    $003D: Result := 'WM_GETOBJECT';
    $0041: Result := 'WM_COMPACTING';
    $0044: Result := 'WM_COMMNOTIFY { obsolete in Win32}';
    $0046: Result := 'WM_WINDOWPOSCHANGING';
    $0047: Result := 'WM_WINDOWPOSCHANGED';
    $0048: Result := 'WM_POWER';
    $004A: Result := 'WM_COPYDATA';
    $004B: Result := 'WM_CANCELJOURNAL';
    $004E: Result := 'WM_NOTIFY';
    $0050: Result := 'WM_INPUTLANGCHANGEREQUEST';
    $0051: Result := 'WM_INPUTLANGCHANGE';
    $0052: Result := 'WM_TCARD';
    $0053: Result := 'WM_HELP';
    $0054: Result := 'WM_USERCHANGED';
    $0055: Result := 'WM_NOTIFYFORMAT';
    $007B: Result := 'WM_CONTEXTMENU';
    $007C: Result := 'WM_STYLECHANGING';
    $007D: Result := 'WM_STYLECHANGED';
    $007E: Result := 'WM_DISPLAYCHANGE';
    $007F: Result := 'WM_GETICON';
    $0080: Result := 'WM_SETICON';
    $0081: Result := 'WM_NCCREATE';
    $0082: Result := 'WM_NCDESTROY';
    $0083: Result := 'WM_NCCALCSIZE';
    $0084: Result := 'WM_NCHITTEST';
    $0085: Result := 'WM_NCPAINT';
    $0086: Result := 'WM_NCACTIVATE';
    $0087: Result := 'WM_GETDLGCODE';
    $0088: Result := 'WM_SYNCPAINT';
    $00A0: Result := 'WM_NCMOUSEMOVE';
    $00A1: Result := 'WM_NCLBUTTONDOWN';
    $00A2: Result := 'WM_NCLBUTTONUP';
    $00A3: Result := 'WM_NCLBUTTONDBLCLK';
    $00A4: Result := 'WM_NCRBUTTONDOWN';
    $00A5: Result := 'WM_NCRBUTTONUP';
    $00A6: Result := 'WM_NCRBUTTONDBLCLK';
    $00A7: Result := 'WM_NCMBUTTONDOWN';
    $00A8: Result := 'WM_NCMBUTTONUP';
    $00A9: Result := 'WM_NCMBUTTONDBLCLK';
    // edit control messages start (todo: add more if needed)
    $00B0: Result := 'EM_GETSEL';
    $00B1: Result := 'EM_SETSEL';
    $00B2: Result := 'EM_GETRECT';
    $00B3: Result := 'EM_SETRECT';
    $00B4: Result := 'EM_SETRECTNP';
    $00B5: Result := 'EM_SCROLL';
    $00B6: Result := 'EM_LINESCROLL';
    $00B7: Result := 'EM_SCROLLCARET';
    $00B8: Result := 'EM_GETMODIFY';
    $00B9: Result := 'EM_SETMODIFY';
    $00BA: Result := 'EM_GETLINECOUNT';
    $00BB: Result := 'EM_LINEINDEX';
    $00BC: Result := 'EM_SETHANDLE';
    $00BD: Result := 'EM_GETHANDLE';
    $00BE: Result := 'EM_GETTHUMB';
    $00C1: Result := 'EM_LINELENGTH';
    $00C2: Result := 'EM_REPLACESEL';
    $00C4: Result := 'EM_GETLINE';
    $00C5: Result := 'EM_LIMITTEXT';
    $00C6: Result := 'EM_CANUNDO';
    $00C7: Result := 'EM_UNDO';
    $00C8: Result := 'EM_FMTLINES';
    $00C9: Result := 'EM_LINEFROMCHAR';
    $00CB: Result := 'EM_SETTABSTOPS';
    $00CC: Result := 'EM_SETPASSWORDCHAR';
    $00CD: Result := 'EM_EMPTYUNDOBUFFER';
    $00CE: Result := 'EM_GETFIRSTVISIBLELINE';
    $00CF: Result := 'EM_SETREADONLY';
    $00D0: Result := 'EM_SETWORDBREAKPROC';
    $00D1: Result := 'EM_GETWORDBREAKPROC';
    $00D2: Result := 'EM_GETPASSWORDCHAR';
    $00D3: Result := 'EM_SETMARGINS';
    $00D4: Result := 'EM_GETMARGINS';
    $00D5: Result := 'EM_GETLIMITTEXT';
    $00D6: Result := 'EM_POSFROMCHAR';
    $00D7: Result := 'EM_CHARFROMPOS';
    // edit control messages end
    // scrollbar control messages start
    $00E0: Result := 'SBM_SETPOS';
    $00E1: Result := 'SBM_GETPOS';
    $00E2: Result := 'SBM_SETRANGE';
    $00E3: Result := 'SBM_GETRANGE';
    $00E4: Result := 'SBM_ENABLE_ARROWS';
    $00E6: Result := 'SBM_SETRANGEREDRAW';
    $00E9: Result := 'SBM_SETSCROLLINFO';
    $00EA: Result := 'SBM_GETSCROLLINFO';
    $00EB: Result := 'SBM_GETSCROLLBARINFO';
    // scrollbar control messages end
    // button control messages start
    $00F0: Result := 'BM_GETCHECK';
    $00F1: Result := 'BM_SETCHECK';
    $00F2: Result := 'BM_GETSTATE';
    $00F3: Result := 'BM_SETSTATE';
    $00F4: Result := 'BM_SETSTYLE';
    $00F5: Result := 'BM_CLICK';
    $00F6: Result := 'BM_GETIMAGE';
    $00F7: Result := 'BM_SETIMAGE';
    $00F8: Result := 'BM_SETDONTCLICK';
    // button control messages end
    $0100: Result := 'WM_KEYFIRST or WM_KEYDOWN';
    $0101: Result := 'WM_KEYUP';
    $0102: Result := 'WM_CHAR';
    $0103: Result := 'WM_DEADCHAR';
    $0104: Result := 'WM_SYSKEYDOWN';
    $0105: Result := 'WM_SYSKEYUP';
    $0106: Result := 'WM_SYSCHAR';
    $0107: Result := 'WM_SYSDEADCHAR';
    $0108: Result := 'WM_KEYLAST';
    $010D: Result := 'WM_IME_STARTCOMPOSITION';
    $010E: Result := 'WM_IME_ENDCOMPOSITION';
    $010F: Result := 'WM_IME_COMPOSITION or WM_IME_KEYLAST';
    $0110: Result := 'WM_INITDIALOG';
    $0111: Result := 'WM_COMMAND';
    $0112: Result := 'WM_SYSCOMMAND';
    $0113: Result := 'WM_TIMER';
    $0114: Result := 'WM_HSCROLL';
    $0115: Result := 'WM_VSCROLL';
    $0116: Result := 'WM_INITMENU';
    $0117: Result := 'WM_INITMENUPOPUP';
    $011F: Result := 'WM_MENUSELECT';
    $0120: Result := 'WM_MENUCHAR';
    $0121: Result := 'WM_ENTERIDLE';
    $0122: Result := 'WM_MENURBUTTONUP';
    $0123: Result := 'WM_MENUDRAG';
    $0124: Result := 'WM_MENUGETOBJECT';
    $0125: Result := 'WM_UNINITMENUPOPUP';
    $0126: Result := 'WM_MENUCOMMAND';
    $0127: Result := 'WM_CHANGEUISTATE';
    $0128: Result := 'WM_UPDATEUISTATE';
    $0129: Result := 'WM_QUERYUISTATE';
    $0132: Result := 'WM_CTLCOLORMSGBOX';
    $0133: Result := 'WM_CTLCOLOREDIT';
    $0134: Result := 'WM_CTLCOLORLISTBOX';
    $0135: Result := 'WM_CTLCOLORBTN';
    $0136: Result := 'WM_CTLCOLORDLG';
    $0137: Result := 'WM_CTLCOLORSCROLLBAR';
    $0138: Result := 'WM_CTLCOLORSTATIC';
    $0140: Result := 'CB_GETEDITSEL';
    $0141: Result := 'CB_LIMITTEXT';
    $0142: Result := 'CB_SETEDITSEL';
    $0143: Result := 'CB_ADDSTRING';
    $0144: Result := 'CB_DELETESTRING';
    $0145: Result := 'CB_DIR';
    $0146: Result := 'CB_GETCOUNT';
    $0147: Result := 'CB_GETCURSEL';
    $0148: Result := 'CB_GETLBTEXT';
    $0149: Result := 'CB_GETLBTEXTLEN';
    $014A: Result := 'CB_INSERTSTRING';
    $014B: Result := 'CB_RESETCONTENT';
    $014C: Result := 'CB_FINDSTRING';
    $014D: Result := 'CB_SELECTSTRING';
    $014E: Result := 'CB_SETCURSEL';
    $014F: Result := 'CB_SHOWDROPDOWN';
    $0150: Result := 'CB_GETITEMDATA';
    $0151: Result := 'CB_SETITEMDATA';
    $0152: Result := 'CB_GETDROPPEDCONTROLRECT';
    $0153: Result := 'CB_SETITEMHEIGHT';
    $0154: Result := 'CB_GETITEMHEIGHT';
    $0155: Result := 'CB_SETEXTENDEDUI';
    $0156: Result := 'CB_GETEXTENDEDUI';
    $0157: Result := 'CB_GETDROPPEDSTATE';
    $0158: Result := 'CB_FINDSTRINGEXACT';
    $0159: Result := 'CB_SETLOCALE';
    $015A: Result := 'CB_GETLOCALE';
    $015B: Result := 'CB_GETTOPINDEX';
    $015C: Result := 'CB_SETTOPINDEX';
    $015D: Result := 'CB_GETHORIZONTALEXTENT';
    $015E: Result := 'CB_SETHORIZONTALEXTENT';
    $015F: Result := 'CB_GETDROPPEDWIDTH';
    $0160: Result := 'CB_SETDROPPEDWIDTH';
    $0161: Result := 'CB_INITSTORAGE';
    $0163: Result := 'CB_MULTIPLEADDSTRING';
    $0164: Result := 'CB_GETCOMBOBOXINFO';
    $0200: Result := 'WM_MOUSEFIRST or WM_MOUSEMOVE';
    $0201: Result := 'WM_LBUTTONDOWN';
    $0202: Result := 'WM_LBUTTONUP';
    $0203: Result := 'WM_LBUTTONDBLCLK';
    $0204: Result := 'WM_RBUTTONDOWN';
    $0205: Result := 'WM_RBUTTONUP';
    $0206: Result := 'WM_RBUTTONDBLCLK';
    $0207: Result := 'WM_MBUTTONDOWN';
    $0208: Result := 'WM_MBUTTONUP';
    $0209: Result := 'WM_MBUTTONDBLCLK';
    $020A: Result := 'WM_MOUSEWHEEL or WM_MOUSELAST';
    $0210: Result := 'WM_PARENTNOTIFY';
    $0211: Result := 'WM_ENTERMENULOOP';
    $0212: Result := 'WM_EXITMENULOOP';
    $0213: Result := 'WM_NEXTMENU';
    $0214: Result := 'WM_SIZING';
    $0215: Result := 'WM_CAPTURECHANGED';
    $0216: Result := 'WM_MOVING';
    $0218: Result := 'WM_POWERBROADCAST';
    $0219: Result := 'WM_DEVICECHANGE';
    $0220: Result := 'WM_MDICREATE';
    $0221: Result := 'WM_MDIDESTROY';
    $0222: Result := 'WM_MDIACTIVATE';
    $0223: Result := 'WM_MDIRESTORE';
    $0224: Result := 'WM_MDINEXT';
    $0225: Result := 'WM_MDIMAXIMIZE';
    $0226: Result := 'WM_MDITILE';
    $0227: Result := 'WM_MDICASCADE';
    $0228: Result := 'WM_MDIICONARRANGE';
    $0229: Result := 'WM_MDIGETACTIVE';
    $0230: Result := 'WM_MDISETMENU';
    $0231: Result := 'WM_ENTERSIZEMOVE';
    $0232: Result := 'WM_EXITSIZEMOVE';
    $0233: Result := 'WM_DROPFILES';
    $0234: Result := 'WM_MDIREFRESHMENU';
    $0281: Result := 'WM_IME_SETCONTEXT';
    $0282: Result := 'WM_IME_NOTIFY';
    $0283: Result := 'WM_IME_CONTROL';
    $0284: Result := 'WM_IME_COMPOSITIONFULL';
    $0285: Result := 'WM_IME_SELECT';
    $0286: Result := 'WM_IME_CHAR';
    $0287: Result := 'WM_IME_SYSTEM';
    $0288: Result := 'WM_IME_REQUEST';
    $0290: Result := 'WM_IME_KEYDOWN';
    $0291: Result := 'WM_IME_KEYUP';
    $02A1: Result := 'WM_MOUSEHOVER';
    $02A2: Result := 'WM_NCMOUSELEAVE';
    $02A3: Result := 'WM_MOUSELEAVE';
    $0300: Result := 'WM_CUT';
    $0301: Result := 'WM_COPY';
    $0302: Result := 'WM_PASTE';
    $0303: Result := 'WM_CLEAR';
    $0304: Result := 'WM_UNDO';
    $0305: Result := 'WM_RENDERFORMAT';
    $0306: Result := 'WM_RENDERALLFORMATS';
    $0307: Result := 'WM_DESTROYCLIPBOARD';
    $0308: Result := 'WM_DRAWCLIPBOARD';
    $0309: Result := 'WM_PAINTCLIPBOARD';
    $030A: Result := 'WM_VSCROLLCLIPBOARD';
    $030B: Result := 'WM_SIZECLIPBOARD';
    $030C: Result := 'WM_ASKCBFORMATNAME';
    $030D: Result := 'WM_CHANGECBCHAIN';
    $030E: Result := 'WM_HSCROLLCLIPBOARD';
    $030F: Result := 'WM_QUERYNEWPALETTE';
    $0310: Result := 'WM_PALETTEISCHANGING';
    $0311: Result := 'WM_PALETTECHANGED';
    $0312: Result := 'WM_HOTKEY';
    $0317: Result := 'WM_PRINT';
    $0318: Result := 'WM_PRINTCLIENT';
    $031F: Result := 'WM_DWMNCRENDERINGCHANGED';
    $0358: Result := 'WM_HANDHELDFIRST';
    $035F: Result := 'WM_HANDHELDLAST';
    $0380: Result := 'WM_PENWINFIRST';
    $038F: Result := 'WM_PENWINLAST';
    $0390: Result := 'WM_COALESCE_FIRST';
    $039F: Result := 'WM_COALESCE_LAST';
    $03E0: Result := 'WM_DDE_FIRST or WM_DDE_INITIATE';
    $03E1: Result := 'WM_DDE_TERMINATE';
    $03E2: Result := 'WM_DDE_ADVISE';
    $03E3: Result := 'WM_DDE_UNADVISE';
    $03E4: Result := 'WM_DDE_ACK';
    $03E5: Result := 'WM_DDE_DATA';
    $03E6: Result := 'WM_DDE_REQUEST';
    $03E7: Result := 'WM_DDE_POKE';
    $03E8: Result := 'WM_DDE_EXECUTE or WM_DDE_LAST';
    $0400: Result := 'WM_USER';
    // progress bar
    $0401: Result := 'PBM_SETRANGE';
    $0402: Result := 'PBM_SETPOS';
    $0403: Result := 'PBM_DELTAPOS';
    $0404: Result := 'PBM_SETSTEP';
    $0405: Result := 'PBM_STEPIT';
    $0406: Result := 'PBM_SETRANGE32';
    $0407: Result := 'PBM_GETRANGE';
    $0408: Result := 'PBM_GETPOS';
    $0409: Result := 'PBM_SETBARCOLOR';
    $040A: Result := 'PBM_SETMARQUEE';
    $040D: Result := 'PBM_GETSTEP';
    $040E: Result := 'PBM_GETBKCOLOR';
    $040F: Result := 'PBM_GETBARCOLOR';
    $0410: Result := 'PBM_SETSTATE';
    $0411: Result := 'PBM_GETSTATE';
    // misc
    $0469: Result := 'UDM_SETBUDDY';
    $046A: Result := 'UDM_GETBUDDY';
    $102C: Result := 'LVM_GETITEMSTATE';
    $8000: Result := 'WM_APP';
    //menus
    $01E0 : Result:= 'MN_SETHMENU';
    $01E1 : Result:= 'MN_GETHMENU';
    $01E2 : Result:= 'MN_SIZEWINDOW';
    $01E3 : Result:= 'MN_OPENHIERARCHY';
    $01E4 : Result:= 'MN_CLOSEHIERARCHY';
    $01E5 : Result:= 'MN_SELECTITEM';
    $01E6 : Result:= 'MN_CANCELMENUS';
    $01E7 : Result:= 'MN_SELECTFIRSTVALIDITEM';
    $01EA : Result:= 'MN_GETPPOPUPMENU';
    $01EB : Result:= 'MN_FINDMENUWINDOWFROMPOINT';
    $01EC : Result:= 'MN_SHOWPOPUPWINDOW';
    $01ED : Result:= 'MN_BUTTONDOWN';
    $01EE : Result:= 'MN_MOUSEMOVE';
    $01EF : Result:= 'MN_BUTTONUP';
    $01F0 : Result:= 'MN_SETTIMERTOOPENHIERARCHY';
    $01F1 : Result:= 'MN_DBLCLK';
    $01F2 : Result:= 'MN_ENDMENU';
    $01F3 : Result:= 'MN_DODRAGDROP';
    $01F4 : Result:= 'MN_ENDMENU';

    $0090: Result:= 'WM_UAHDESTROYWINDOW';
    $0091: Result:= 'WM_UAHDRAWMENU';
    $0092: Result:= 'WM_UAHDRAWMENUITEM';
    $0093: Result:= 'WM_UAHINITMENU';
    $0094: Result:= 'WM_UAHMEASUREMENUITEM';
    $0095: Result:= 'WM_UAHNCPAINTMENUPOPUP';

    $00AE : Result:= 'WM_NCUAHDRAWCAPTION'; //undocumented
    $00AF : Result:= 'WM_NCUAHDRAWFRAME'; //undocumented
    //VCL control notifications
    CN_CHARTOITEM        : Result := 'CN_CHARTOITEM';
    CN_COMMAND           : Result := 'CN_COMMAND';
    CN_COMPAREITEM       : Result := 'CN_COMPAREITEM';
    CN_CTLCOLORBTN       : Result := 'CN_CTLCOLORBTN';
    CN_CTLCOLORDLG       : Result := 'CN_CTLCOLORDLG';
    CN_CTLCOLOREDIT      : Result := 'CN_CTLCOLOREDIT';
    CN_CTLCOLORLISTBOX   : Result := 'CN_CTLCOLORLISTBOX';
    CN_CTLCOLORMSGBOX    : Result := 'CN_CTLCOLORMSGBOX';
    CN_CTLCOLORSCROLLBAR : Result := 'CN_CTLCOLORSCROLLBAR';
    CN_CTLCOLORSTATIC    : Result := 'CN_CTLCOLORSTATIC';
    CN_DELETEITEM        : Result := 'CN_DELETEITEM';
    CN_DRAWITEM          : Result := 'CN_DRAWITEM';
    CN_HSCROLL           : Result := 'CN_HSCROLL';
    CN_MEASUREITEM       : Result := 'CN_MEASUREITEM';
    CN_PARENTNOTIFY      : Result := 'CN_PARENTNOTIFY';
    CN_VKEYTOITEM        : Result := 'CN_VKEYTOITEM';
    CN_VSCROLL           : Result := 'CN_VSCROLL';
    CN_KEYDOWN           : Result := 'CN_KEYDOWN';
    CN_KEYUP             : Result := 'CN_KEYUP';
    CN_CHAR              : Result := 'CN_CHAR';
    CN_SYSKEYDOWN        : Result := 'CN_SYSKEYDOWN';
    CN_SYSCHAR           : Result := 'CN_SYSCHAR';
    CN_NOTIFY            : Result := 'CN_NOTIFY';
  else Exit('Unknown(' + IntToHex(WM_Message, 4) + ')');
  end; { Case }

  Result:= Result +' (' + IntToHex(WM_Message, 4) + ')';
end;


function  GetModuleLocation : string;
begin
  SetLength(Result, MAX_PATH);
  GetModuleFileName(HInstance, PChar(Result), MAX_PATH);
  Result:=PChar(Result);
end;

function MakeValidTagName(const s: string): string;
var
  c: Char;
  i: Integer;
begin
  SetLength(Result, Length(s));
  i:=0;
  for c in s do
  begin
   Inc(i);
    if CharInSet(c,['A'..'Z', 'a'..'z', '0'..'9']) then
      Result[i] := c
    else
      Result[i] := '_';
  end;
end;


function IsUACEnabled: Boolean;
var
  LRegistry: TRegistry;
begin
  Result := False;
  if CheckWin32Version(6, 0) then
  begin
    LRegistry := TRegistry.Create;
    try
      LRegistry.RootKey := HKEY_LOCAL_MACHINE;
      if LRegistry.OpenKeyReadOnly('SOFTWARE\Microsoft\Windows\CurrentVersion\Policies\System') then
        Exit(LRegistry.ValueExists('EnableLUA') and LRegistry.ReadBool('EnableLUA'));
    finally
      LRegistry.Free;
    end;
  end;
end;


function  UserInGroup(Group :DWORD) : Boolean;
 var
  pIdentifierAuthority :TSIDIdentifierAuthority;
  pSid : Windows.PSID;
  IsMember    : BOOL;
 begin
  pIdentifierAuthority := SECURITY_NT_AUTHORITY;
  Result := AllocateAndInitializeSid(pIdentifierAuthority,2, SECURITY_BUILTIN_DOMAIN_RID, Group, 0, 0, 0, 0, 0, 0, pSid);
  try
    if Result then
      if not CheckTokenMembership(0, pSid, IsMember) then //passing 0 means which the function will be use the token of the calling thread.
         Result:= False
      else
         Result:=IsMember;
  finally
     FreeSid(pSid);
  end;
 end;

function  CurrentUserIsAdmin: Boolean;
begin
 Result:=UserInGroup(DOMAIN_ALIAS_RID_ADMINS);
end;

procedure RunAsAdmin(const FileName, Params: string; hWnd: HWND = 0);
var
  sei: TShellExecuteInfo;
begin
  ZeroMemory(@sei, SizeOf(sei));
  sei.cbSize := SizeOf(sei);
  sei.Wnd := hWnd;
  sei.fMask := SEE_MASK_FLAG_DDEWAIT or SEE_MASK_FLAG_NO_UI;
  sei.lpVerb := 'runas';
  sei.lpFile := PChar(FileName);
  sei.lpParameters := PChar(Params);
  sei.nShow := SW_SHOWNORMAL;
  if not ShellExecuteEx(@sei) then
    RaiseLastOSError;
end;

function RunAndWait(hWnd: HWND; const FileName, Params: string;RunAs:Boolean=False):Boolean;
var
  sei: TShellExecuteInfo;
  lpExitCode: DWORD;
begin
  Result:=False;
  FillChar(sei, SizeOf(sei), 0);
  sei.cbSize := SizeOf(sei);
  sei.Wnd := hWnd;
  sei.fMask := SEE_MASK_FLAG_NO_UI or SEE_MASK_NOCLOSEPROCESS;
  if RunAs then
    sei.lpVerb := 'runas';
  sei.lpFile := PChar(FileName);
  sei.lpParameters := PChar(Params);
  sei.nShow := SW_SHOWNORMAL;

  if not ShellExecuteEx(@sei) then
    RaiseLastOSError;

  if sei.hProcess <> 0 then
  begin
    while WaitForSingleObject(sei.hProcess, 50) = WAIT_TIMEOUT do
      Application.ProcessMessages;
    GetExitCodeProcess(sei.hProcess, lpExitCode);
    Result:=lpExitCode=0;
    CloseHandle(sei.hProcess);
  end;
end;

function GetSpecialFolder(const CSIDL: integer) : string;
var
  lpszPath : PWideChar;
begin
  lpszPath := StrAlloc(MAX_PATH);
  try
     ZeroMemory(lpszPath, MAX_PATH);
    if SHGetSpecialFolderPath(0, lpszPath, CSIDL, False)  then
      Result := lpszPath
    else
      Result := '';
  finally
    StrDispose(lpszPath);
  end;
end;

function EnumFixedFontsProc(var LogFont: TLogFont; var TextMetric: TTextMetric;
  FontType: integer; Data: Pointer): integer; stdcall;
var
  List : TStrings;
begin
  //  if ((FontType and TrueType_FontType) <> 0) and  ((LogFont.lfPitchAndFamily and VARIABLE_PITCH) = 0) then
   List := TStrings(Data);
  if ((LogFont.lfPitchAndFamily and FIXED_PITCH) <> 0) then
    if not StartsText('@', LogFont.lfFaceName) and
      (List.IndexOf(LogFont.lfFaceName) < 0) then
      List.Add(LogFont.lfFaceName);

  Result := 1;
end;

function  EnumFontsProc(var LogFont: TLogFont; var TextMetric: TTextMetric;  FontType: integer; Data: Pointer): integer; stdcall;
var
  List : TStrings;
begin
   List := TStrings(Data);
    if not StartsText('@', LogFont.lfFaceName) and
      (List.IndexOf(LogFont.lfFaceName) < 0) then
      List.Add(LogFont.lfFaceName);
  Result := 1;
end;


procedure MsgBox(const Msg: string);
begin
  MessageDlg(Msg, mtInformation, [mbOK], 0);
end;

function GetTempDirectory: string;
var
  lpBuffer: array[0..MAX_PATH] of Char;
begin
  GetTempPath(MAX_PATH, @lpBuffer);
  Result := StrPas(lpBuffer);
end;

function  GetSpecialFolderLocation(nFolder: Integer): string;
var
  ppMalloc: IMalloc;
  ppidl:    PItemIdList;
begin
  ppidl := nil;
  try
    if SHGetMalloc(ppMalloc) = S_OK then
    begin
      SHGetSpecialFolderLocation(0, nFolder, ppidl);
      SetLength(Result, MAX_PATH);
      if not SHGetPathFromIDList(ppidl, PChar(Result)) then
        RaiseLastOSError;
      SetLength(Result, lStrLen(PChar(Result)));
    end;
  finally
    if ppidl <> nil then
      ppMalloc.Free(ppidl);
  end;
end;

function GetLocalAppDataFolder: string;
begin
  Result:=GetSpecialFolderLocation(CSIDL_LOCAL_APPDATA);
end;

function GetAppDataFolder: string;
begin
  Result:=GetSpecialFolderLocation(CSIDL_APPDATA);
end;

function ProcessFileName(dwProcessId: DWORD): string;
var
  hModule: Cardinal;
begin
  Result := '';
  hModule := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, dwProcessId);
  if hModule <> 0 then
    try
      SetLength(Result, MAX_PATH);
      if GetModuleFileNameEx(hModule, 0, PChar(Result), MAX_PATH) > 0 then
        SetLength(Result, StrLen(PChar(Result)))
      else
        Result := '';
    finally
      CloseHandle(hModule);
    end;
end;

function IsAppRunning(const FileName: string): boolean;
var
  hSnapshot      : Cardinal;
  EntryParentProc: TProcessEntry32;
begin
  Result := False;
  hSnapshot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if hSnapshot = INVALID_HANDLE_VALUE then
    exit;
  try
    EntryParentProc.dwSize := SizeOf(EntryParentProc);
    if Process32First(hSnapshot, EntryParentProc) then
      repeat
        if CompareText(ExtractFileName(FileName), EntryParentProc.szExeFile) = 0 then
          if CompareText(ProcessFileName(EntryParentProc.th32ProcessID),  FileName) = 0 then
          begin
            Result := True;
            break;
          end;
      until not Process32Next(hSnapshot, EntryParentProc);
  finally
    CloseHandle(hSnapshot);
  end;
end;



function GetFileVersion(const FileName: string): string;
var
  FSO  : OleVariant;
begin
  FSO    := CreateOleObject('Scripting.FileSystemObject');
  Result := FSO.GetFileVersion(FileName);
end;

procedure ExtractIconFile(Icon: TIcon; const Filename: string;IconType : Cardinal);
var
  FileInfo: TShFileInfo;
begin
  if FileExists(Filename) then
  begin
    FillChar(FileInfo, SizeOf(FileInfo), 0);
    if SHGetFileInfo(PChar(Filename), 0, FileInfo, SizeOf(FileInfo),
      SHGFI_ICON or IconType)=0 then RaiseLastOSError;
    if FileInfo.hIcon <> 0 then
      Icon.Handle:=FileInfo.hIcon;
  end;
end;

procedure ExtractIconFileToImageList(ImageList: TCustomImageList; const Filename: string);
var
  FileInfo: TShFileInfo;
begin
  if FileExists(Filename) then
  begin
    FillChar(FileInfo, SizeOf(FileInfo), 0);
    SHGetFileInfo(PChar(Filename), 0, FileInfo, SizeOf(FileInfo),
      SHGFI_ICON or SHGFI_SMALLICON);
    if FileInfo.hIcon <> 0 then
    begin
      ImageList_AddIcon(ImageList.Handle, FileInfo.hIcon);
      DestroyIcon(FileInfo.hIcon);
    end;
  end;
end;


procedure CreateArrayBitmap(Width,Height:Word;Colors: Array of TColor;var Bitmap : TBitmap);
Var
 i : integer;
 w : integer;
begin
  Bitmap.PixelFormat:=pf24bit;
  Bitmap.Width:=Width;
  Bitmap.Height:=Height;
  Bitmap.Canvas.Brush.Color := clBlack;
  Bitmap.Canvas.FillRect(Rect(0,0, Width, Height));


  w :=(Width-2) div (High(Colors)+1);
  for i:=0 to High(Colors) do
  begin
   Bitmap.Canvas.Brush.Color := Colors[i];
   //bmp.Canvas.FillRect(Rect((w*i),0, w*(i+1), Height));
   Bitmap.Canvas.FillRect(Rect((w*i)+1,1, w*(i+1)+1, Height-1))
  end;
end;


end.

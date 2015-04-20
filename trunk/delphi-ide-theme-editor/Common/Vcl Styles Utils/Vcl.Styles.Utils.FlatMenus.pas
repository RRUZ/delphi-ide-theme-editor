// **************************************************************************************************
//
// Unit Vcl.Styles.Utils.FlatMenus
// unit for the VCL Styles Utils
// http://code.google.com/p/vcl-styles-utils/
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
//
// Portions created by Mahdi Safsafi [SMP3]   e-mail SMP@LIVE.FR
// Portions created by Rodrigo Ruz V. are Copyright (C) 2013-2015 Rodrigo Ruz V.
// All Rights Reserved.
//
// **************************************************************************************************
unit Vcl.Styles.Utils.FlatMenus;

interface
{$I ..\Common\Jedi.inc}

uses
  Classes,
  Types,
  SysUtils,
  Windows,
  Messages,
  Graphics,
  UxTheme,
  {$IFDEF DELPHIXE2_UP}
  Colorizer.Vcl.Styles,
  Vcl.Styles,
  Vcl.Themes,
  {$ELSE}
  Colorizer.uxThemeHelper,
  Themes,
  {$ENDIF}
  Forms,
  ImgList,
  GraphUtil,
  Controls,
  Menus,
  uMisc,
  Colorizer.Utils, //remove in the future
  Math,
  Vcl.Styles.Utils.FlatStyleHook;

const
  { The Undocumented Messages }
  MN_SETHMENU = $01E0;
  MN_GETHMENU = $01E1;
  MN_SIZEWINDOW = $01E2;
  MN_OPENHIERARCHY = $01E3;
  MN_CLOSEHIERARCHY = $01E4;
  MN_SELECTITEM = $01E5;
  MN_CANCELMENUS = $01E6;
  MN_SELECTFIRSTVALIDITEM = $01E7;
  MN_GETPPOPUPMENU = $01EA;
  MN_FINDMENUWINDOWFROMPOINT = $01EB;
  MN_SHOWPOPUPWINDOW = $01EC;
  MN_BUTTONDOWN = $01ED;
  MN_MOUSEMOVE = $01EE;
  MN_BUTTONUP = $01EF;
  MN_SETTIMERTOOPENHIERARCHY = $01F0;
  MN_DBLCLK = $001F1;

  { MARLETT Font Char Const }
  MARLETT_RESTORE_CHAR = Char(50);
  MARLETT_MINIMIZE_CHAR = Char(48);
  MARLETT_CLOSE_CHAR = Char(114);
  MARLETT_MAXIMIZE_CHAR = Char(49);

type
  TColorizerPopupStyleHook = class;
  TSysPopupItemState = set of (isHot, isDisabled, isChecked, isDefault);
  TSysPopupItemStyle = (isNormal, isSep, isDropDown);

  TColorizerPopupStyleHook = class(TSysStyleHook)
  private type
    TSysPopupItem = class
    private
      FIndex: integer;
      FMenu: HMENU;
      FHandle: HWND;
      FSysParent: TSysControl;
      FSysPopupStyleHook: TColorizerPopupStyleHook;
      function GetItemRect: TRect;
      function IsItemDisabled: Boolean;
      function IsItemContainsSubMenu: Boolean;
      function IsItemSeparator: Boolean;
      function IsItemChecked: Boolean;
      function IsItemDefault: Boolean;
      function GetItemText: String;
      function GetVCLMenuItems: TMenuItem;
      function GetVCLMenuItemsFast: TMenuItem;
      function GetItemBitmap: HBITMAP;
      function IsItemRadioCheck: Boolean;
      // function isItemVisible: Boolean;
      function IsItemOwnerDraw: Boolean;
      function GetItemID: WORD;
      function GetVCLRealItem: TMenuItem;
    public
      constructor Create(SysPopupStyleHook: TColorizerPopupStyleHook; SysParent: TSysControl; const Index: integer; const Menu: HMENU); virtual;
      Destructor Destroy; override;
      property ID: WORD read GetItemID;
      property ItemRect: TRect read GetItemRect;
      property Disabled: Boolean read IsItemDisabled;
      property Separator: Boolean read IsItemSeparator;
      property HasSubMenu: Boolean read IsItemContainsSubMenu;
      property Checked: Boolean read IsItemChecked;
      // property Visible: Boolean read isItemVisible;
      property RadioCheck: Boolean read IsItemRadioCheck;
      property DefaultItem: Boolean read IsItemDefault;
      property Text: String read GetItemText;
      property OwnerDraw: Boolean read IsItemOwnerDraw;
      property VCLMenuItems: TMenuItem read GetVCLMenuItemsFast;
      property VCLItem: TMenuItem read GetVCLRealItem;
      property Bitmap: HBITMAP read GetItemBitmap;
    end;

  var
    FItemsPainted: Boolean;
    FParentSubItemPainted: Boolean;
    FPreviousHotItemIndex: integer;
    FPaintFirstItemFromMenu: Boolean;
    FKeyIndex: integer;
    FSysPopupItem: TSysPopupItem;
    FCount: integer;
    FMenu: HMENU;
    FVCLMenuItems: TMenuItem;
    function GetMenuFromHandle(AHandle: HWND): HMENU;
    function GetItemsCount: integer;
    procedure MNSELECTITEM(var Message: TMessage); message MN_SELECTITEM;
    procedure WMPRINT(var Message: TMessage); message WM_PRINT;
    function GetSysPopupItem(Index: integer): TSysPopupItem;
    function GetRightToLeft: Boolean;
  protected
    procedure EraseItem(Canvas: TCanvas; const Index: integer; const ItemRect: TRect); virtual;
    procedure DoDrawItem(Canvas: TCanvas; const Index: integer);
    procedure DrawItem(Canvas: TCanvas; const Index: integer; const ItemRect: TRect; const ItemText: String; const State: TSysPopupItemState; const Style: TSysPopupItemStyle); Virtual;
    procedure PaintBackground(Canvas: TCanvas); override;
    procedure WndProc(var Message: TMessage); override;
    procedure UpdateColors; override;
  public
    constructor Create(AHandle: THandle); override;
    Destructor Destroy; override;
    property Menu: HMENU read FMenu;
    property Items[Index: integer]: TSysPopupItem read GetSysPopupItem;
    property Count: integer read FCount;
    property RightToLeft: Boolean read GetRightToLeft;
  end;

  procedure RegisterFlatMenusHooks;
  procedure UnregisterFlatMenusHooks;


implementation

uses Vcl.Styles.Utils.FlatControls;

type
  TControlClass = Class(TControl);

function GetBmpInfo(hBmp: HBITMAP): Bitmap;
begin
  ZeroMemory(@Result, sizeof(Bitmap));
  GetObject(hBmp, sizeof(Result), @Result);
end;

function GetBitmapHeight(hBmp: HBITMAP): integer;
begin
  Result := GetBmpInfo(hBmp).bmHeight;
end;

function GetBitmapWidth(hBmp: HBITMAP): integer;
begin
  Result := GetBmpInfo(hBmp).bmWidth;
end;

function BmpToIcon(hBmp: HBITMAP): HICON;
var
  Bmp: Bitmap;
  hbmMask: HBITMAP;
  DC: HDC;
  ii: ICONINFO;
  Icon: HICON;
begin
  FillChar(Bmp, sizeof(Bitmap), Char(0));
  GetObject(hBmp, sizeof(Bitmap), @Bmp);
  DC := GetDC(0);
  hbmMask := CreateCompatibleBitmap(DC, Bmp.bmWidth, Bmp.bmHeight);
  ii.fIcon := True;
  ii.hbmColor := hBmp;
  ii.hbmMask := hbmMask;
  Icon := CreateIconIndirect(ii);
  DeleteObject(hbmMask);
  ReleaseDC(0, DC);
  Result := Icon;
end;

procedure RotateBitmap(Bmp: TBitmap; Rads: Single; AdjustSize: Boolean; BkColor: TColor = clNone);
var
  C: Single;
  S: Single;
  XForm: tagXFORM;
  Tmp: TBitmap;
begin
  C := Cos(Rads);
  S := Sin(Rads);
  XForm.eM11 := C;
  XForm.eM12 := S;
  XForm.eM21 := -S;
  XForm.eM22 := C;
  Tmp := TBitmap.Create;
  try
    Tmp.TransparentColor := Bmp.TransparentColor;
    Tmp.TransparentMode := Bmp.TransparentMode;
    Tmp.Transparent := Bmp.Transparent;
    Tmp.Canvas.Brush.Color := BkColor;
    if AdjustSize then
    begin
      Tmp.Width := Round(Bmp.Width * Abs(C) + Bmp.Height * Abs(S));
      Tmp.Height := Round(Bmp.Width * Abs(S) + Bmp.Height * Abs(C));
      XForm.eDx := (Tmp.Width - Bmp.Width * C + Bmp.Height * S) / 2;
      XForm.eDy := (Tmp.Height - Bmp.Width * S - Bmp.Height * C) / 2;
    end
    else
    begin
      Tmp.Width := Bmp.Width;
      Tmp.Height := Bmp.Height;
      XForm.eDx := (Bmp.Width - Bmp.Width * C + Bmp.Height * S) / 2;
      XForm.eDy := (Bmp.Height - Bmp.Width * S - Bmp.Height * C) / 2;
    end;
    SetGraphicsMode(Tmp.Canvas.Handle, GM_ADVANCED);
    SetWorldTransform(Tmp.Canvas.Handle, XForm);
    BitBlt(Tmp.Canvas.Handle, 0, 0, Tmp.Width, Tmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
    Bmp.Assign(Tmp);
  finally
    Tmp.Free;
  end;
end;

function GetMenuItemPos(Menu: HMENU; ID: integer): integer;
var
  i: integer;
  mii: MENUITEMINFO;
begin
  Result := -1;
  if Menu = 0 then
    Exit;
  for i := 0 to GetMenuItemCount(Menu) do
  begin
    FillChar(mii, sizeof(mii), Char(0));
    mii.cbSize := sizeof(mii);
    mii.fMask := MIIM_ID;
    if (GetMenuItemInfo(Menu, i, True, mii)) then
      if mii.wID = Cardinal(ID) then
        Exit(i);
  end;
end;

{ TSysPopupStyleHook }
constructor TColorizerPopupStyleHook.Create(AHandle: THandle);
begin
  inherited;
{$IF CompilerVersion > 23}
  StyleElements := [seFont, seClient, seBorder];
{$ELSE}
  OverridePaint := True;
  OverridePaintNC := True;
  OverrideFont := True;
{$IFEND}
  FPreviousHotItemIndex := -1;
  FKeyIndex := -1;
  FItemsPainted := False;
  FSysPopupItem := nil;
  FVCLMenuItems := nil;
end;

destructor TColorizerPopupStyleHook.Destroy;
begin
  if Assigned(FSysPopupItem) then
    FreeAndNil(FSysPopupItem);
  inherited;
end;

procedure TColorizerPopupStyleHook.DoDrawItem(Canvas: TCanvas; const Index: integer);
var
  LItemRect: TRect;
  P: TPoint;
  State: TSysPopupItemState;
  Style: TSysPopupItemStyle;
  LText: String;
  SaveIndex: integer;
  Item: TSysPopupItem;
begin
  if (Index < 0) or (Index > Count - 1) then
    Exit;
  Item := Items[Index];
  LItemRect := Item.ItemRect;
  P := Point(LItemRect.Left, LItemRect.Top);
  ScreenToClient(Handle, P);
  LItemRect := Rect(P.X, P.Y, P.X + (LItemRect.Right - LItemRect.Left), P.Y + (LItemRect.Bottom - LItemRect.Top));
  if LItemRect.Left < 2 then
    LItemRect.Left := 2;
  inc(LItemRect.Right, 4);
  if LItemRect.Top < 2 then
    inc(LItemRect.Top, 2);
  { Item State }
  State := [];
  if index <> FPreviousHotItemIndex then
    Include(State, isHot);
  if Item.Disabled then
    Include(State, isDisabled);
  if Item.Checked then
    Include(State, isChecked);
  if Item.DefaultItem then
    Include(State, isDefault);
  { Item Style }
  Style := isNormal;
  if Item.Separator then
    Style := isSep;
  if Item.HasSubMenu then
    Style := isDropDown;

  LText := '';
  if Style <> isSep then
    LText := Item.Text;

  SaveIndex := SaveDC(Canvas.Handle);
  try
    EraseItem(Canvas, Index, LItemRect);
  finally
    RestoreDC(Canvas.Handle, SaveIndex)
  end;

  SaveIndex := SaveDC(Canvas.Handle);
  try
    if Style = isSep then
    begin
       Canvas.Pen.Color:=TColorizerLocalSettings.ColorMap.DisabledFontColor;
       Canvas.Rectangle(LItemRect.Left + 30, LItemRect.Top, LItemRect.Right, LItemRect.Top + 1);
    end
    else
     DrawItem(Canvas, Index, LItemRect, LText, State, Style);
  finally
    RestoreDC(Canvas.Handle, SaveIndex)
  end;
end;


{$IF CompilerVersion >= 23}

{$ELSE}
function RectCenter(var R: TRect; const Bounds: TRect): TRect;
begin
  OffsetRect(R, -R.Left, -R.Top);
  OffsetRect(R, (RectWidth(Bounds) - RectWidth(R)) div 2, (RectHeight(Bounds) - RectHeight(R)) div 2);
  OffsetRect(R, Bounds.Left, Bounds.Top);
  Result := R;
end;
{$IFEND}


procedure TColorizerPopupStyleHook.DrawItem(Canvas: TCanvas; const Index: integer; const ItemRect: TRect; const ItemText: String; const State: TSysPopupItemState; const Style: TSysPopupItemStyle);
type
  tmPopupType = (tmPopupItemNormal, tmPopupItemHot, tmPopupItemDisabled, tmPopupSeparator);
var
  LTextFormat: TTextFormat;
  LPopupType : tmPopupType;
  DC: HDC;
  LSize: TSize;
  MI: TMenuItem;
  ImageIndex: integer;
  LTextRect, LImageRect, R, LRect: TRect;
  LImageWidth: integer;
  hBmp: HBITMAP;
  BmpHeight, BmpWidth: integer;
  Icon: HICON;
  DisplayCheckedGlyph: Boolean;
  Sign: Char;
  SysItem: TSysPopupItem;
  sShortCut: String;
  Bmp, LBitmapCheck: TBitmap;
  LColor : TColor;
  {$IFDEF DELPHIXE2_UP}
  Detail: TThemedMenu;
  LDetails: TThemedElementDetails;
  {$ENDIF}

  procedure DrawSubMenu(const ItemRect: TRect);
  const
    ArrowPos= 11;
  var
    SubMenuSize: TSize;
    LSubMenuRect: TRect;
  {$IFDEF DELPHIXE2_UP}
    LSubMenuDetails: TThemedElementDetails;
    LSubMenuDetail: TThemedMenu;
  {$ENDIF}
  begin
    LSubMenuRect := Rect(0, 0, 0, 0);
    SubMenuSize.cx:= 3;
    SubMenuSize.cy:= 3;

    if not RightToLeft then
      LSubMenuRect := Rect(ItemRect.Right - SubMenuSize.cx, ItemRect.Top, ItemRect.Right, ItemRect.Top + SubMenuSize.cy)
    else
      LSubMenuRect := Rect(ItemRect.Left + 4, ItemRect.Top, ItemRect.Left + 4 + SubMenuSize.cx, ItemRect.Bottom);

    {$IFDEF DELPHIXE2_UP}
    if TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesMenusColors then
    begin
      LSubMenuDetail := tmPopupSubMenuNormal;
      if isDisabled in State then
        LSubMenuDetail := tmPopupSubMenuDisabled;
      LSubMenuDetails := ColorizerStyleServices.GetElementDetails(LSubMenuDetail);
      ColorizerStyleServices.GetElementSize(DC, LSubMenuDetails, esActual, SubMenuSize);
      if not RightToLeft then
        LSubMenuRect := Rect(ItemRect.Right - SubMenuSize.cx, ItemRect.Top, ItemRect.Right, ItemRect.Top + SubMenuSize.cy)
      else
        LSubMenuRect := Rect(ItemRect.Left + 4, ItemRect.Top, ItemRect.Left + 4 + SubMenuSize.Width, ItemRect.Bottom);
      Bmp := TBitmap.Create;
      try
        Bmp.SetSize(SubMenuSize.Width, SubMenuSize.Height);
        Bmp.Canvas.Brush.Color := clFuchsia;
        Bmp.Canvas.FillRect(Rect(0, 0, SubMenuSize.Width, SubMenuSize.Height));
        ColorizerStyleServices.DrawElement(Bmp.Canvas.Handle, LSubMenuDetails, Rect(0, 0, SubMenuSize.Width, SubMenuSize.Height));
        if RightToLeft then
        begin
          RotateBitmap(Bmp, DegToRad(180), False, clFuchsia);
          inc(LSubMenuRect.Top, (Bmp.Height div 2) - 2);
        End
        else
          Dec(LSubMenuRect.Left, 4);

        TransparentBlt(DC, LSubMenuRect.Left, LSubMenuRect.Top, SubMenuSize.Width, SubMenuSize.Height, Bmp.Canvas.Handle, 0, 0, SubMenuSize.Width, SubMenuSize.Height, clFuchsia);
      finally
        Bmp.Free;
      end;
      Dec(LTextRect.Right, LSubMenuRect.Width);
    end
    else
    {$ENDIF}
    begin
      Canvas.Brush.Color:=TColorizerLocalSettings.ColorMap.FontColor;
      Canvas.Pen.Color  :=TColorizerLocalSettings.ColorMap.FontColor;
      if RightToLeft then
        DrawArrow(Canvas, TScrollDirection.sdLeft, Point(LSubMenuRect.Left - ArrowPos, LSubMenuRect.Top + 7), 3)
      else
        DrawArrow(Canvas, TScrollDirection.sdRight, Point(LSubMenuRect.Left - ArrowPos, LSubMenuRect.Top + 7), 3);

      Dec(LTextRect.Right, (LSubMenuRect.Right - LSubMenuRect.Left));
    end;
  end;


  procedure DrawSpecialChar(DC: HDC; const Sign: Char; DestRect: TRect; const Bold: Boolean = False; const Disabled: Boolean = False; Hot : Boolean = False);
  var
    LogFont: TLogFont;
    pOldFont: HGDIOBJ;
    AFont: HFONT;
    oldColor: COLORREF;
    OldMode: integer;
  begin

    LogFont.lfHeight := DestRect.Bottom - DestRect.Top;
    LogFont.lfWidth := 0;
    LogFont.lfEscapement := 0;
    LogFont.lfOrientation := 0;
    if Bold then
      LogFont.lfWeight := FW_BOLD
    else
      LogFont.lfWeight := FW_NORMAL;
    LogFont.lfItalic := 0;
    LogFont.lfUnderline := 0;
    LogFont.lfStrikeOut := 0;
    LogFont.lfCharSet := DEFAULT_CHARSET;
    LogFont.lfOutPrecision := OUT_DEFAULT_PRECIS;
    LogFont.lfClipPrecision := CLIP_DEFAULT_PRECIS;
    LogFont.lfQuality := DEFAULT_QUALITY;
    LogFont.lfPitchAndFamily := DEFAULT_PITCH;
    LogFont.lfFaceName := 'Marlett';
    AFont := CreateFontIndirect(LogFont);

    {$IFDEF DELPHIXE2_UP}
    if TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesMenusColors then
    begin
      oldColor := ColorizerStyleServices.GetStyleFontColor(sfMenuItemTextNormal);

      if hot then
        oldColor := ColorizerStyleServices.GetStyleFontColor(sfMenuItemTextHot);

      if Disabled then
        oldColor := ColorizerStyleServices.GetStyleFontColor(sfMenuItemTextDisabled);
    end
    else
    {$ENDIF}
    begin
      oldColor := ColorToRGB(TColorizerLocalSettings.ColorMap.FontColor);
      if hot then
        oldColor := ColorToRGB(TColorizerLocalSettings.ColorMap.SelectedFontColor);

      if Disabled then
        oldColor := ColorToRGB(TColorizerLocalSettings.ColorMap.DisabledFontColor);
    end;

    oldColor := SetTextColor(DC, oldColor);
    pOldFont := SelectObject(DC, AFont);
    OldMode := SetBkMode(DC, Transparent);

    Windows.DrawText(DC, Sign, 1, DestRect, DT_LEFT or DT_SINGLELINE);
    SetBkMode(DC, OldMode);
    SelectObject(DC, pOldFont);
    SelectObject(DC, oldColor);
    DeleteObject(AFont);
  end;

begin
  DisplayCheckedGlyph := True;
  LTextRect := ItemRect;
  { Fast access . }
  SysItem := Items[Index]; // Do not destroy !!
  DC := Canvas.Handle;
  R := ItemRect;

  LPopupType := tmPopupItemNormal;
  if isHot in State then
    LPopupType := tmPopupItemHot;
  if isDisabled in State then
    LPopupType := tmPopupItemDisabled;
  if Style = isSep then
  begin
    LPopupType := tmPopupSeparator;
    inc(R.Left, 25);
  end;


  {$IFDEF DELPHIXE2_UP}
  if TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesMenusColors then
  begin
    Detail := TThemedMenu.tmPopupItemNormal;
    if isHot in State then
      Detail := TThemedMenu.tmPopupItemHot;
    if isDisabled in State then
      Detail := TThemedMenu.tmPopupItemDisabled;
    if Style = isSep then
      Detail := TThemedMenu.tmPopupSeparator;

    LDetails := ColorizerStyleServices.GetElementDetails(Detail);
    if (Detail <> TThemedMenu.tmPopupItemNormal) and (Detail <> TThemedMenu.tmPopupItemDisabled) then
      ColorizerStyleServices.DrawElement(DC, LDetails, R);
  end
  else
  {$ENDIF}
  begin
    if (LPopupType <> tmPopupItemNormal) and (LPopupType <> tmPopupItemDisabled) then
    begin
       Canvas.Brush.Color:=TColorizerLocalSettings.ColorMap.SelectedColor;
       Canvas.Pen.Color:=TColorizerLocalSettings.ColorMap.FrameTopLeftOuter;
       Canvas.Rectangle(R);
    end;
  end;

  if Style = isDropDown then
    DrawSubMenu(ItemRect);

  LImageWidth := 0;
  MI := SysItem.VCLMenuItems;
  if MI <> nil then
    MI := SysItem.VCLItem;

  if MI <> nil then
  begin
    { Draw Vcl PopupMenu Bitmap }
    ImageIndex := MI.ImageIndex;
    with MI.GetParentMenu do
    begin
      if (ImageIndex < 0) and (MI.Bitmap <> nil) then
      begin
        Bmp := MI.Bitmap;
        if (Bmp.Width = 16) and (Bmp.Height = 16) then
        begin
          LImageWidth := Bmp.Width;
          LImageRect := Rect(0, 0, Bmp.Width, Bmp.Height);
          RectVCenter(LImageRect, ItemRect);
          if not RightToLeft then
            OffsetRect(LImageRect, 4, 0)
          else
          begin
            LImageRect.Left := ItemRect.Right - Bmp.Width - 4;
            LImageRect.Right := ItemRect.Right;
          end;

          Canvas.Draw(LImageRect.Left, LImageRect.Top, Bmp)
        end
        else
        begin
          LImageWidth := 16;
          LImageRect := Rect(0, 0, 16, 16);
          RectVCenter(LImageRect, ItemRect);
          if not RightToLeft then
            OffsetRect(LImageRect, 4, 0)
          else
          begin
            LImageRect.Left := ItemRect.Right - 16 - 4;
            LImageRect.Right := ItemRect.Right;
          end;

          Canvas.StretchDraw(LImageRect, Bmp);
        end;

      end
      else if (MI.GetParentMenu.Images <> nil) and (ImageIndex > -1) then
      begin
        LImageWidth := Images.Width;
        DisplayCheckedGlyph := False;
        LImageRect := Rect(0, 0, Images.Width, Images.Height);
        RectVCenter(LImageRect, ItemRect);
        if not RightToLeft then
          OffsetRect(LImageRect, 4, 0)
        else
        begin
          LImageRect.Left := ItemRect.Right - Images.Width - 4;
          LImageRect.Right := ItemRect.Right;
        end;
        Images.Draw(Canvas, LImageRect.Left, LImageRect.Top, ImageIndex);
      end;
    end;
  end
  else if SysItem.Bitmap > 0 then
  begin
    hBmp := SysItem.Bitmap;
    if hBmp < HBMMENU_POPUP_MINIMIZE + 1 then
    begin
      { Draw System PopupMenu Bitmap }
      DisplayCheckedGlyph := False;

      case hBmp of
        HBMMENU_POPUP_RESTORE: Sign := MARLETT_RESTORE_CHAR;
        HBMMENU_POPUP_MINIMIZE, HBMMENU_MBAR_MINIMIZE_D: Sign := MARLETT_MINIMIZE_CHAR;
        HBMMENU_POPUP_MAXIMIZE: Sign := MARLETT_MAXIMIZE_CHAR;
        HBMMENU_POPUP_CLOSE, HBMMENU_MBAR_CLOSE_D: Sign := MARLETT_CLOSE_CHAR;
      else Sign := Char(0);
      end;
      if Sign <> #0 then
      begin
        LImageRect := Rect(0, 0, 10, 10);
        R := Rect(ItemRect.Left, ItemRect.Top, ItemRect.Left + 30, ItemRect.Bottom);
        RectCenter(LImageRect, ItemRect);
        if not RightToLeft then
          LImageRect.Left := ItemRect.Left + 8
        else
        begin
          LImageRect.Left := ItemRect.Right - 8 - 4;
          LImageRect.Right := ItemRect.Right;
        end;
        DrawSpecialChar(DC, Sign, LImageRect, False, (isDisabled in State), (isHot in State));

      end;
    end
    else
    begin
      { Draw PopupMenu Bitmap }
      BmpWidth := GetBitmapWidth(hBmp);
      BmpHeight := GetBitmapHeight(hBmp);
      if (BmpWidth > 0) and (BmpHeight > 0) then
      begin
        DisplayCheckedGlyph := False;
        LImageRect := Rect(0, 0, BmpWidth, BmpHeight);
        RectVCenter(LImageRect, ItemRect);
        if not RightToLeft then
          OffsetRect(LImageRect, 4, 0)
        else
        begin
          LImageRect.Left := ItemRect.Right - BmpWidth - 4;
          LImageRect.Right := ItemRect.Right;
        end;
        Icon := BmpToIcon(hBmp);

        DrawIconEX(DC, LImageRect.Left, LImageRect.Top, Icon, BmpWidth, BmpHeight, 0, 0, DI_NORMAL);
        DeleteObject(Icon);
      end;
    end;
  end;

  if (SysItem.Checked) and (DisplayCheckedGlyph) then
  begin
    {$IFDEF DELPHIXE2_UP}
    if TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesMenusColors then
    begin
      Detail := TThemedMenu(integer(tmPopupCheckNormal) + integer(SysItem.Disabled));
      if SysItem.RadioCheck then
        Detail := TThemedMenu(integer(tmPopupBulletNormal) + integer(SysItem.Disabled));
      LDetails := ColorizerStyleServices.GetElementDetails(Detail);
      ColorizerStyleServices.GetElementSize(DC, LDetails, esActual, LSize);
      LImageRect := Rect(0, 0, LSize.Width, LSize.Height);
      RectVCenter(LImageRect, ItemRect);
      if not RightToLeft then
        OffsetRect(LImageRect, 4, 0)
      else
      begin
        LImageRect.Left := ItemRect.Right - LSize.Width - 4;
        LImageRect.Right := ItemRect.Right;
      end;
      ColorizerStyleServices.DrawElement(DC, LDetails, LImageRect);
    end
    else
    {$ENDIF}
    begin
      LSize.cx:=2;
      LSize.cy:=2;
      LImageRect := Rect(0, 0, LSize.cx, LSize.cy);
      RectVCenter(LImageRect, ItemRect);
      if not RightToLeft then
        OffsetRect(LImageRect, 4, 0)
      else
      begin
        LImageRect.Left := ItemRect.Right - LSize.cx - 4;
        LImageRect.Right := ItemRect.Right;
      end;

      LBitmapCheck:=TBitmap.Create;
      try
        LRect:=Rect(ItemRect.Left, ItemRect.Top+1, ItemRect.Left+20, ItemRect.Bottom-1);
        LBitmapCheck.SetSize(LRect.Right- LRect.Left, LRect.Bottom - LRect.Top);
        LBitmapCheck.Canvas.Brush.Color:=TColorizerLocalSettings.ColorMap.SelectedColor;
        LBitmapCheck.Canvas.Pen.Color  :=TColorizerLocalSettings.ColorMap.FrameTopLeftInner;
        LBitmapCheck.Canvas.Rectangle(0, 0 , LBitmapCheck.Width, LBitmapCheck.Height);
        LBitmapCheck.Canvas.Pen.Color:=TColorizerLocalSettings.ColorMap.FontColor;
        DrawCheck(LBitmapCheck.Canvas, Point(6, 8), 2, False);
        Canvas.Draw(ItemRect.Left, ItemRect.Top+1, LBitmapCheck);
      finally
        LBitmapCheck.Free;
      end;
    end;

  end;

  { Draw Text }
  LTextFormat := [tfLeft, tfVerticalCenter, tfSingleLine, tfExpandTabs, tfHidePrefix];
  if not RightToLeft then
    inc(LTextRect.Left, 30)
  else
  begin
    LTextRect.Left := ItemRect.Left;
    LTextRect.Right := ItemRect.Right - 30;
    Exclude(LTextFormat, tfLeft);
    Include(LTextFormat, tfRtlReading);
    Include(LTextFormat, tfRight);
  end;

  if LImageWidth > 0 then
  begin
    if not RightToLeft then
      LTextRect.Left := ItemRect.Left + LImageWidth + 4 + 4
    else
    begin
      LTextRect.Left := ItemRect.Left;
      LTextRect.Right := ItemRect.Right - LImageWidth - 8;
    end;
  end;

  LColor   := TColorizerLocalSettings.ColorMap.FontColor;
  if isHot in State then
    LColor   := TColorizerLocalSettings.ColorMap.SelectedFontColor;

  if isDisabled in State then
    LColor   := TColorizerLocalSettings.ColorMap.DisabledFontColor;

  if SysItem.DefaultItem then
    Canvas.Font.Style := [fsBold];

  {$IFDEF DELPHIXE2_UP}
  if TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesMenusColors then
  begin
    LDetails := ColorizerStyleServices.GetElementDetails(TThemedMenu.tmPopupItemNormal);
    if isHot in State then
      LDetails := ColorizerStyleServices.GetElementDetails(TThemedMenu.tmPopupItemHot);
    if isDisabled in State then
      LDetails := ColorizerStyleServices.GetElementDetails(TThemedMenu.tmPopupItemDisabled);

    DrawText(Canvas.Handle, LDetails, ItemText, LTextRect, LTextFormat);
  end
  else
  {$ENDIF}
  begin
    Inc(LTextRect.Left, 5);
    DrawText(Canvas.Handle, LColor, ItemText, LTextRect, LTextFormat);
  end;

  { Draw ShortCut Text . }
  if MI <> nil then
  begin
    if MI.ShortCut <> 0 then
    begin
      sShortCut := ShortCutToText(MI.ShortCut);
      LTextRect := ItemRect;
      if RightToLeft then
      begin
        LTextRect.Left := ItemRect.Left + 14;
        LTextRect.Right := LTextRect.Left + Canvas.TextWidth(sShortCut);
      end
      else
      begin
        LTextRect.Left := ItemRect.Right - 14 - Canvas.TextWidth(sShortCut);
        LTextRect.Right := ItemRect.Right;
      end;

      {$IFDEF DELPHIXE2_UP}
      if TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesMenusColors then
        DrawText(Canvas.Handle, LDetails, sShortCut, LTextRect, LTextFormat)
      else
      {$ENDIF}
      DrawText(Canvas.Handle, LColor, sShortCut, LTextRect, LTextFormat);
    end;
  end;
end;

procedure TColorizerPopupStyleHook.EraseItem(Canvas: TCanvas; const Index: integer; const ItemRect: TRect);
var
  Bmp: TBitmap;
begin
  Bmp := TBitmap.Create;
  try
    Bmp.SetSize(SysControl.Width, SysControl.Height);
    PaintBackground(Bmp.Canvas);
    BitBlt(Canvas.Handle, ItemRect.Left, ItemRect.Top, (ItemRect.Right - ItemRect.Left), (ItemRect.Bottom - ItemRect.Top), Bmp.Canvas.Handle, ItemRect.Left, ItemRect.Top, SRCCOPY);
  finally
    Bmp.Free;
  end;
end;

function TColorizerPopupStyleHook.GetItemsCount: integer;
begin
  Result := GetMenuItemCount(FMenu);
end;

function TColorizerPopupStyleHook.GetMenuFromHandle(AHandle: HWND): HMENU;
begin
  Result := SendMessage(AHandle, MN_GETHMENU, 0, 0);
end;

function TColorizerPopupStyleHook.GetRightToLeft: Boolean;
var
  info: TMenuItemInfo;
begin
  FillChar(info, sizeof(info), Char(0));
  info.cbSize := sizeof(TMenuItemInfo);
  info.fMask := MIIM_TYPE;
  GetMenuItemInfo(FMenu, 0, True, info);
  Result := ((info.fType and MFT_RIGHTORDER) = MFT_RIGHTORDER) or ((info.fType and MFT_RIGHTJUSTIFY) = MFT_RIGHTJUSTIFY);
end;

function TColorizerPopupStyleHook.GetSysPopupItem(Index: integer): TSysPopupItem;
begin
  Result := nil;
  if (Index > -1) and (index <= Count) then
  begin
    if Assigned(FSysPopupItem) then
      FreeAndNil(FSysPopupItem);
    FSysPopupItem := TSysPopupItem.Create(Self, SysControl, Index, FMenu);
    Result := FSysPopupItem;
  end;
end;

procedure TColorizerPopupStyleHook.PaintBackground(Canvas: TCanvas);
{$IFDEF DELPHIXE2_UP}
var
  LDetails: TThemedElementDetails;
{$ENDIF}
begin
  {$IFDEF DELPHIXE2_UP}
  if TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesMenusColors then
  begin
    LDetails := ColorizerStyleServices.GetElementDetails(tmPopupBorders);
    ColorizerStyleServices.DrawElement(Canvas.Handle, LDetails, SysControl.ClientRect);
  end
  else
  {$ENDIF}
  begin

    if Assigned(TColorizerLocalSettings.ColorMap) and Assigned(TColorizerLocalSettings.Settings)  and TColorizerLocalSettings.Settings.Enabled then
    begin
      Canvas.Brush.Color:=TColorizerLocalSettings.ColorMap.MenuColor;
      Canvas.FillRect(SysControl.ClientRect);
      Canvas.Brush.Color:= TColorizerLocalSettings.ColorMap.Color;
      Canvas.FillRect(Rect(SysControl.ClientRect.Left, SysControl.ClientRect.Top, SysControl.ClientRect.Left+25, SysControl.ClientRect.Bottom));
      Canvas.Brush.Style:=bsClear;
      Canvas.Pen.Width:=2;
      Canvas.Pen.Color:=TColorizerLocalSettings.ColorMap.FrameTopLeftOuter;
      Canvas.Rectangle(SysControl.ClientRect);
    end;
  end;
end;

procedure TColorizerPopupStyleHook.UpdateColors;
begin
  inherited;
  Font := Screen.MenuFont;
end;
{$WARN IMPLICIT_STRING_CAST_LOSS OFF}

type
  TSubMenuItemInfo = record
    Menu: HMENU;
    WindowHandle: HWND;
    ItemIndex: integer;
  end;

var
  SubMenuItemInfoArray: array of TSubMenuItemInfo;

procedure TColorizerPopupStyleHook.MNSELECTITEM(var Message: TMessage);
var
  DC: HDC;
  Canvas: TCanvas;
  Index: integer;
  i: WORD;
  L: integer;
  ParentItem: integer;
  ParentPopup: HWND;
  LMenu: HMENU;
begin
  { The undocumented MN_SELECTITEM Message:
    This is the most importants message ,
    Windows sends this message every time when the user
    select an item (not clicking,only select) ...
    wParam=Current Item Index .
    lparam= may be it's unused (not sure).
  }

  Handled := False;
  Canvas := TCanvas.Create;
  ParentPopup := 0;
  ParentItem := -1;

  DC := GetDC(Handle);
  try
    Canvas.Handle := DC;
    Index := integer(Message.WParam);
    if Assigned(Font) then
      Canvas.Font := Font;
    { Out of index . }
    if (Index > FCount - 1) or (Index < 0) then
    begin
      { Make sure that wParam hold a valid Item Index .
        if not .. then mouse is not on the PopupMenu
        => Remove Item highlight .
      }
      SetRedraw(True);
      if (FPreviousHotItemIndex > -1) and (FPreviousHotItemIndex < FCount) then
        DoDrawItem(Canvas, FPreviousHotItemIndex);
      FPreviousHotItemIndex := -1;
      Handled := True;
      Exit;
    end;

    if not FItemsPainted then
    begin
      { Items are not painted completely . }
      FPreviousHotItemIndex := Index;
      DoDrawItem(Canvas, Index);
      if (Index = Count - 1) then
        FItemsPainted := True;
      Handled := True;
      Exit;
    end;

    L := Length(SubMenuItemInfoArray);
    if L <> 0 then
    begin
      for i := 0 to L - 1 do
      begin
        { Look for SubMenu Parent }
        LMenu := SubMenuItemInfoArray[i].Menu;
        if LMenu = FMenu then
        begin
          ParentPopup := SubMenuItemInfoArray[i].WindowHandle;
          ParentItem := SubMenuItemInfoArray[i].ItemIndex;
          Break;
        end;
      end;
    end;

    if (ParentPopup = Handle) then
      SetRedraw(True) { Allow drawing the current PopupMenu }
    else if ((ParentPopup <> Handle) and (FItemsPainted) and (ParentPopup <> 0)) then
    begin
      {
        if user jump so fast from the parent PopupMenu to the
        Child PopupMenu (SubMenu) , the hot item of parent Popup menu
        will be draw as a normal item (not hot)..
        So we need to repaint the hot item that drop the child popup menu.
      }
      if (not FParentSubItemPainted) and (ParentItem > -1) then
      begin
        SendMessage(ParentPopup, MN_SELECTITEM, ParentItem, 0);
        FParentSubItemPainted := True;
      end;
      { Don't Redraw the parent of the Current PopupMenu }
      SetRedraw(ParentPopup, False);
    end;

    { if Item can drop a sub Popup Menu }
    if Items[Index].HasSubMenu then
    begin
      L := Length(SubMenuItemInfoArray);
      if L = 0 then
        SetLength(SubMenuItemInfoArray, 1);
      for i := 0 to L do
        { Avoid duplication }
        if SubMenuItemInfoArray[i].Menu <> GetMenuFromHandle(Handle) then
        begin
          inc(L);
          SetLength(SubMenuItemInfoArray, L);
          SubMenuItemInfoArray[L - 1].Menu := GetSubMenu(FMenu, Index);
          SubMenuItemInfoArray[L - 1].WindowHandle := Handle;
          SubMenuItemInfoArray[L - 1].ItemIndex := Index;
          Break;
        end;
    end;

    { If all Items are painted }
    if FItemsPainted then
    begin
      { In order to show / hide SubMenu ,we need to
        process the default message handler . }
      SetRedraw(False);
      Message.Result := CallDefaultProc(Message);
      SetRedraw(True);
    end;

    if FPreviousHotItemIndex <> Index then
    begin
      { Draw Item normal . }
      DoDrawItem(Canvas, FPreviousHotItemIndex);
      { Draw Item hot . }
      DoDrawItem(Canvas, Index);
      FPreviousHotItemIndex := Index;
    end;

  finally
    Canvas.Free;
    ReleaseDC(Handle, DC);
  end;
  Handled := True;
end;

procedure TColorizerPopupStyleHook.WMPRINT(var Message: TMessage);
var
  DC: HDC;
  i: integer;
  Canvas: TCanvas;
begin

  FMenu := GetMenuFromHandle(Handle);
  FCount := GetItemsCount;

  if Message.WParam <> 0 then
    DC := HDC(Message.WParam)
  else
    DC := GetDC(Handle);

  Canvas := TCanvas.Create;
  try
    Canvas.Handle := DC;
    PaintBackground(Canvas);
  finally
    Canvas.Free;
    if DC <> HDC(Message.WParam) then
      ReleaseDC(Handle, DC);
  end;

  if Count > -1 then
  begin
    for i := 0 to Count - 1 do
      PostMessage(Handle, MN_SELECTITEM, i, 0);
  end;
  Handled := True;
end;

// ------------------------------------------------------------------------------
function IsItemSeparator(Menu: HMENU; const ItemIndex: integer): Boolean;
var
  info: TMenuItemInfo;
begin
  {
    Use this function instead of Items[Index].Separator .
    ==> Fast access in WM_KEYDOWN .
  }
  FillChar(info, sizeof(info), Char(0));
  info.cbSize := sizeof(TMenuItemInfo);
  info.fMask := MIIM_FTYPE;
  GetMenuItemInfo(Menu, ItemIndex, True, info);
  Result := (info.fType and MFT_SEPARATOR) = MFT_SEPARATOR;
end;
// ------------------------------------------------------------------------------

procedure TColorizerPopupStyleHook.WndProc(var Message: TMessage);
var
  i: integer;
  TopWin: HWND;
begin
  case Message.Msg of

    MN_SELECTITEM, WM_PRINT:
      begin

        if (not OverridePaint) or (not OverridePaintNC) then
        begin
          Message.Result := CallDefaultProc(Message);
          Exit; { Do not Dispatch . }
        end;
      end;

    WM_PAINT:
      begin
        if not OverridePaint then
        begin
          Message.Result := CallDefaultProc(Message);
          Exit;
        end;
        SetRedraw(False);

        Message.Result := CallDefaultProc(Message);
        SetRedraw(True);
        Exit; { Do not Dispatch . }
      end;

    WM_WINDOWPOSCHANGED:
      begin
        if (not OverridePaint) or (not OverridePaintNC) then
        begin
          Message.Result := CallDefaultProc(Message);
          Exit;
        end;
        SetTimer(Handle, $93, 100, nil);
      end;

    WM_TIMER:
      begin
        if (FItemsPainted) and (Message.WParam = $93) then
        begin
          { If PopupMenu is droped from MainMenu ,
            MainMenu will send WM_KEYDOWN message
            to the PopupMenu that cause the PopupMenu
            to paint the first item as a hot item instead of
            a normal item .
            I use a timer to solve this problem .
          }
          FPaintFirstItemFromMenu := True;
          KillTimer(Handle, $93);
        end;
      end;

    MN_BUTTONDOWN:
      begin
        SetRedraw(False);
        Message.Result := CallDefaultProc(Message);
        SetRedraw(True);
      end;

    WM_KEYDOWN:
      begin
        if (not OverridePaint) or (not OverridePaintNC) then
        begin
          Message.Result := CallDefaultProc(Message);
          Exit;
        end;
        FMenu := GetMenuFromHandle(Handle);
        if FPreviousHotItemIndex <> -1 then
          FKeyIndex := FPreviousHotItemIndex;
        case Message.WParam of
          VK_DOWN:
            if FPaintFirstItemFromMenu then
            begin
              if FKeyIndex >= GetMenuItemCount(Menu) - 1 then
                FKeyIndex := -1;
              inc(FKeyIndex);
              { If the Current Item is Separator then
                find the next valid item .
              }
              if IsItemSeparator(Menu, FKeyIndex) then
                for i := FKeyIndex to GetMenuItemCount(Menu) - 1 do
                  if (not IsItemSeparator(Menu, i)) then
                  begin
                    FKeyIndex := i;
                    Break;
                  end;
              SendMessage(Handle, MN_SELECTITEM, FKeyIndex, 0);
              Message.Result := 0;
            end;
          VK_UP:
            begin
              if FKeyIndex <= 0 then
                FKeyIndex := GetMenuItemCount(Menu);

              Dec(FKeyIndex);
              { If the Current Item is Separator then
                find the next valid item .
              }
              if IsItemSeparator(Menu, FKeyIndex) then
                for i := FKeyIndex downto 0 do
                  if not IsItemSeparator(Menu, i) then
                  begin
                    FKeyIndex := i;
                    Break;
                  end;
              SendMessage(Handle, MN_SELECTITEM, FKeyIndex, 0);
              Message.Result := 0;
            end;
        else
          { Calling the Default Message will cause
            the WM_PAINT Message to be Sent to the PopupMenu Window }
            Message.Result := CallDefaultProc(Message);
        end;
        Exit;
      end;

    WM_ERASEBKGND:
      begin
        if (not OverridePaint) or (not OverridePaintNC) then
        begin
          Message.Result := CallDefaultProc(Message);
          Exit;
        end;
        SendMessage(Handle, WM_PRINT, Message.WParam, Message.lParam);
        Message.Result := 1;
        Exit; { Do not Dispatch . }
      end;

    WM_PRINTCLIENT:
      begin
        if (not OverridePaint) or (not OverridePaintNC) then
        begin
          Message.Result := CallDefaultProc(Message);
          Exit;
        end;
        SendMessage(Handle, WM_PRINT, Message.WParam, Message.lParam);
        Exit;
      end;

    WM_NCCALCSIZE, WM_NCPAINT:
      begin
        if (not OverridePaint) or (not OverridePaintNC) then
        begin
          Message.Result := CallDefaultProc(Message);
          Exit;
        end;
        if not StyleServicesEnabled then
        begin
          Handled := False;
          Exit;
        end;
        Exit; { Do not Dispatch . }
      end;

    WM_DESTROY:
      begin
        TopWin := GetForegroundWindow;
        if TopWin > 0 then
        begin
          { The parent window that host menu should be repained !! }
          if IsVCLControl(TopWin) or IsControlHooked(TopWin) then
          begin
            // AddToLog(IntToStr(TopWin));
            InvalidateRect(TopWin, nil, False);
            UpdateWindow(TopWin);
          end;
        end;
        FVCLMenuItems := nil;
        SetLength(SubMenuItemInfoArray, 0);
        SubMenuItemInfoArray := nil;
        Handled := False;
      end;

  end;
  inherited;
end;

{ TSysPopupItem }

constructor TColorizerPopupStyleHook.TSysPopupItem.Create(SysPopupStyleHook: TColorizerPopupStyleHook; SysParent: TSysControl; const Index: integer; const Menu: HMENU);
begin
  inherited Create;
  FSysPopupStyleHook := SysPopupStyleHook;
  FMenu := Menu;
  FHandle := SysParent.Handle;
  FSysParent := SysParent;
  FIndex := Index;
end;

destructor TColorizerPopupStyleHook.TSysPopupItem.Destroy;
begin
  inherited;
end;

function TColorizerPopupStyleHook.TSysPopupItem.GetItemBitmap: HBITMAP;
var
  info: TMenuItemInfo;
begin
  FillChar(info, sizeof(info), Char(0));
  info.cbSize := sizeof(TMenuItemInfo);
  info.fMask := MIIM_CHECKMARKS or MIIM_BITMAP;
  GetMenuItemInfo(FMenu, FIndex, True, info);
  Result := info.hbmpItem;
  if Result = 0 then
    Result := info.hbmpUnchecked;
end;

function TColorizerPopupStyleHook.TSysPopupItem.GetItemID: WORD;
begin
  Result := 0;
  if (FMenu > 0) and (FIndex > -1) then
    Result := GetMenuItemID(FMenu, FIndex);
end;

function TColorizerPopupStyleHook.TSysPopupItem.GetItemRect: TRect;
begin
  Result := Rect(0, 0, 0, 0);
  if (FMenu > 0) and (FIndex > -1) then
    GetMenuItemRect(0, FMenu, FIndex, Result);
end;

function TColorizerPopupStyleHook.TSysPopupItem.GetItemText: String;
var
  Buffer: PChar;
  StrSize: integer;
  info: MENUITEMINFO;
begin

  if VCLItem <> nil then
  begin
    Result := VCLItem.Caption;
    Exit;
  end;

  { Note:
    The GetMenuString function has been superseded.
    Use the GetMenuItemInfo function to retrieve the menu item text.
  }

  Result := '';

  FillChar(info, sizeof(MENUITEMINFO), Char(0));
  info.cbSize := sizeof(MENUITEMINFO);
  info.fMask := MIIM_STRING or MIIM_FTYPE;
  info.dwTypeData := nil;
  GetMenuItemInfo(FMenu, FIndex, True, info);
  if not(info.fType and MFT_OWNERDRAW = MFT_OWNERDRAW) then
  begin
    { The Size needed for the Buffer . }
    StrSize := info.cch * 2 + 2;
    GetMem(Buffer, StrSize);
    try
      info.dwTypeData := Buffer;
      { inc cch to get the last char . }
      inc(info.cch);
      GetMenuItemInfo(FMenu, FIndex, True, info);
      Result := String(Buffer);
    finally
      FreeMem(Buffer, StrSize);
    end;
    Exit;
  end
  else
  begin
    { if the item is owner draw then we need another way to get
      the item text since , when setting an item to ownerdraw windows
      will destroy the dwTypeData that hold the text . }
    FillChar(info, sizeof(MENUITEMINFO), Char(0));
    info.cbSize := sizeof(MENUITEMINFO);
    info.fMask := MIIM_DATA;
    GetMenuItemInfo(FMenu, FIndex, True, info);
    Result := String(PChar(info.dwItemData));
  end;
end;

function TColorizerPopupStyleHook.TSysPopupItem.GetVCLRealItem: TMenuItem;
var
  i: integer;
  VisibleItems: TList;
  LVCLMenuItems: TMenuItem;
begin
  {
    Return the real menu item .
    If MenuItem has the Visible property set to false
    windows will delete this item but the VCL will not delete
    the item from Items property .And thats can cause the item to be painted !
    Do not access VCLMenuItems.Items[Index] directly
    => Instead , use this one : VCLItem .
  }
  VisibleItems := nil;
  Result := nil;
  LVCLMenuItems := VCLMenuItems;
  if LVCLMenuItems <> nil then
  begin
    VisibleItems := TList.Create;
    for i := 0 to LVCLMenuItems.Count - 1 do
    begin
      if LVCLMenuItems.Items[i].Visible then
        VisibleItems.Add(LVCLMenuItems.Items[i]);
    end;
  end;
  if Assigned(VisibleItems) then
  begin
    if (VisibleItems.Count > 0) and (FIndex < VisibleItems.Count) then
      Result := VisibleItems.Items[FIndex];
    FreeAndNil(VisibleItems);
  end;
end;

function TColorizerPopupStyleHook.TSysPopupItem.GetVCLMenuItems: TMenuItem;
var
  i, j: integer;
  PopupMenu: TPopupMenu;
  Form: TCustomForm;
  MI: TMenuItem;

  function GetChildPopup(Comp: TComponent): TMenuItem;
  var
    k: integer;
  begin
    { This is a CallBack function ==> Be careful !! }
    Result := nil;
    if Assigned(Comp) then
    begin
      for k := 0 to Comp.ComponentCount - 1 do
      begin

        if Comp.Components[k] is TPopupMenu then
        begin
          PopupMenu := TPopupMenu(Comp.Components[k]);
          if PopupMenu.Handle = FMenu then
            Exit(PopupMenu.Items);
        end
        else if Comp.Components[k] is TMenuItem then
        begin
          MI := TMenuItem(Comp.Components[k]);
          if MI.Handle = FMenu then
            Exit(MI);
        end;

        if Comp.Components[k].ComponentCount > 0 then
          Result := GetChildPopup(Comp.Components[k]);
        if Assigned(Result) then
          Exit;
      end;
    end;
  end;

  function ProcessMenu(AMenu: TMenuItem): TMenuItem;
  var
    L: integer;
  begin
    Result := nil;
    for L := 0 to AMenu.Count - 1 do
    begin
      if AMenu[L].Handle = FMenu then
        Exit(AMenu[L]);
      ProcessMenu(AMenu[L]);
    end;
  end;

begin
  // MI := nil;
  Result := nil;

  for i := 0 to PopupList.Count - 1 do
    if TPopupMenu(PopupList.Items[i]).Handle = FMenu then
      Exit(TPopupMenu(PopupList.Items[i]).Items);


  for i := 0 to Screen.FormCount - 1 do
  begin
   Form := Screen.Forms[i];
   //AddLog('Form', 'ClassName '+Form.ClassName);
      for j := 0 to Form.ComponentCount - 1 do
      begin
        if Form.Components[j] is TMenuItem then
        begin
          MI := TMenuItem(Form.Components[j]);
          if MI.Handle = FMenu then
            Exit(MI);
        end
        else if Form.Components[j] is TPopupMenu then
        begin
          PopupMenu := TPopupMenu(Form.Components[j]);
          if PopupMenu.Handle = FMenu then
            Exit(PopupMenu.Items);

          Result := ProcessMenu(PopupMenu.Items);
          if Assigned(Result) then
            Exit;
        end
        else
        begin
          Result := GetChildPopup(Form.Components[j]);
          if Assigned(Result) then
            Exit;
        end;
      end;
  end;

//  for i := 0 to Application.ComponentCount - 1 do
//  begin
//    if Application.Components[i] is TCustomForm then
//    begin
//      Form := TCustomForm(Application.Components[i]);
//      //AddLog('Form', 'ClassName '+Form.ClassName);
//      for j := 0 to Form.ComponentCount - 1 do
//      begin
//
//        if Form.Components[j] is TMenuItem then
//        begin
//          MI := TMenuItem(Form.Components[j]);
//          if MI.Handle = FMenu then
//            Exit(MI);
//        end
//        else if Form.Components[j] is TPopupMenu then
//        begin
//          PopupMenu := TPopupMenu(Form.Components[j]);
//          if PopupMenu.Handle = FMenu then
//            Exit(PopupMenu.Items);
//
//          Result := ProcessMenu(PopupMenu.Items);
//          if Assigned(Result) then
//            Exit;
//        end
//        else
//        begin
//          Result := GetChildPopup(Form.Components[j]);
//          if Assigned(Result) then
//            Exit;
//        end;
//      end;
//    end;
//  end;
end;

function TColorizerPopupStyleHook.TSysPopupItem.GetVCLMenuItemsFast: TMenuItem;
begin
  if Assigned(FSysPopupStyleHook.FVCLMenuItems) then
    Result := FSysPopupStyleHook.FVCLMenuItems
  else
  begin
    Result := GetVCLMenuItems;
    FSysPopupStyleHook.FVCLMenuItems := Result;
  end;
end;

function TColorizerPopupStyleHook.TSysPopupItem.IsItemDisabled: Boolean;
var
  info: TMenuItemInfo;
begin
  FillChar(info, sizeof(info), Char(0));
  info.cbSize := sizeof(TMenuItemInfo);
  info.fMask := MIIM_STATE;
  GetMenuItemInfo(FMenu, FIndex, True, info);
  Result := (info.fState and MFS_DISABLED = MFS_DISABLED) or (info.fState and MF_DISABLED = MF_DISABLED) or (info.fState and MF_GRAYED = MF_GRAYED);
end;

function TColorizerPopupStyleHook.TSysPopupItem.IsItemOwnerDraw: Boolean;
var
  info: TMenuItemInfo;
begin
  FillChar(info, sizeof(MENUITEMINFO), Char(0));
  info.cbSize := sizeof(MENUITEMINFO);
  info.fMask := MIIM_FTYPE;
  info.dwTypeData := nil;
  GetMenuItemInfo(FMenu, FIndex, True, info);
  Result := (info.fType and MFT_OWNERDRAW = MFT_OWNERDRAW);
end;

function TColorizerPopupStyleHook.TSysPopupItem.IsItemRadioCheck: Boolean;
var
  info: TMenuItemInfo;
begin
  FillChar(info, sizeof(info), Char(0));
  info.cbSize := sizeof(TMenuItemInfo);
  info.fMask := MIIM_FTYPE;
  GetMenuItemInfo(FMenu, FIndex, True, info);
  Result := (info.fType and MFT_RADIOCHECK) = MFT_RADIOCHECK;
end;

function TColorizerPopupStyleHook.TSysPopupItem.IsItemChecked: Boolean;
var
  info: TMenuItemInfo;
begin
  FillChar(info, sizeof(info), Char(0));
  info.cbSize := sizeof(TMenuItemInfo);
  info.fMask := MIIM_STATE;
  GetMenuItemInfo(FMenu, FIndex, True, info);
  Result := (info.fState and MFS_CHECKED) = MFS_CHECKED;
end;

function TColorizerPopupStyleHook.TSysPopupItem.IsItemContainsSubMenu: Boolean;
begin
  Result := (GetSubMenu(FMenu, FIndex) > 0);
end;

function TColorizerPopupStyleHook.TSysPopupItem.IsItemDefault: Boolean;
var
  info: TMenuItemInfo;
begin
  FillChar(info, sizeof(info), Char(0));
  info.cbSize := sizeof(TMenuItemInfo);
  info.fMask := MIIM_STATE;
  GetMenuItemInfo(FMenu, FIndex, True, info);
  Result := (info.fState and MFS_DEFAULT) = MFS_DEFAULT;
end;

function TColorizerPopupStyleHook.TSysPopupItem.IsItemSeparator: Boolean;
var
  info: TMenuItemInfo;
begin
  FillChar(info, sizeof(info), Char(0));
  info.cbSize := sizeof(TMenuItemInfo);
  info.fMask := MIIM_FTYPE;
  Result := False;
  if (FIndex > -1) and (FIndex < GetMenuItemCount(FMenu) - 1) then
  begin
    GetMenuItemInfo(FMenu, FIndex, True, info);
    Result := (info.fType and MFT_SEPARATOR) = MFT_SEPARATOR;
  end;
end;


procedure RegisterFlatMenusHooks;
begin
 if{$IF CompilerVersion >= 23}(StyleServices.Available){$ELSE} ThemeServices.ThemesAvailable {$IFEND} then
  TColorizerStyleManager.RegisterSysStyleHook('#32768', TColorizerPopupStyleHook);
end;

procedure UnregisterFlatMenusHooks;
begin
  TColorizerStyleManager.UnRegisterSysStyleHook('#32768', TColorizerPopupStyleHook);
end;

initialization
SubMenuItemInfoArray := nil;


end.

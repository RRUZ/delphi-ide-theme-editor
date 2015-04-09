//**************************************************************************************************
//
// Unit Vcl.Styles.NC
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
// The Original Code is Vcl.Styles.NC.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2014 Rodrigo Ruz V.
// All Rights Reserved.
//
//**************************************************************************************************

unit Vcl.Styles.NC;

interface

uses
  System.Classes,
  System.Generics.Collections,
  Vcl.ImgList,
  System.Types,
  System.UITypes,
  Winapi.Windows,
  Winapi.Messages,
  Vcl.Graphics,
  Vcl.Themes,
  Vcl.Controls,
  Vcl.Menus,
  Vcl.StdCtrls,
  Vcl.Forms;

type
  TNCButton      = class;
  TListNCButtons = TObjectList<TNCButton>;

  TNCControls    = class(TComponent)
  private
    FList: TListNCButtons;
    FStyleServices  : TCustomStyleServices;
    FVisible: Boolean;
    FForm : TCustomForm;
    FShowSystemMenu: Boolean;
    function GetStyleServices: TCustomStyleServices;
    procedure SetStyleServices(const Value: TCustomStyleServices);
    procedure SetVisible(const Value: Boolean);
  public
    property List : TListNCButtons read FList;
    property StyleServices : TCustomStyleServices read GetStyleServices write SetStyleServices;
    property Visible : Boolean read FVisible write SetVisible;
    property ShowSystemMenu : Boolean read FShowSystemMenu write FShowSystemMenu;
    constructor Create(AOwner: TComponent);override;
    destructor Destroy; override;
  end;

  TNCButton  = class(TControl)
  public type
    TNCButtonStyle = (nsPushButton, nsTranparent, nsSplitButton, nsSplitTrans);
    TNCImageStyle  = (isNormal, isGray, isGrayHot);
  private
    FDropDown : Boolean;
    FStyle: TNCButtonStyle;
    FImageAlignment: TImageAlignment;
    FImages: TCustomImageList;
    FPressedImageIndex: TImageIndex;
    FDropDownMenu: TPopupMenu;
    FOnDropDownClick: TNotifyEvent;
    FDisabledImageIndex: TImageIndex;
    FImageMargins: TImageMargins;
    FImageIndex: TImageIndex;
    FHotImageIndex: TImageIndex;
    FImageStyle: TNCImageStyle;
    FOnClick: TNotifyEvent;
    FNCControls  :  TNCControls;
    FHintWindow : THintWindow;
    procedure DrawButton(ACanvas: TCanvas; AMouseInControl, Pressed: Boolean);
    procedure SetStyle(const Value: TNCButtonStyle);
    procedure SetDisabledImageIndex(const Value: TImageIndex);
    procedure SetDropDownMenu(const Value: TPopupMenu);
    procedure SetHotImageIndex(const Value: TImageIndex);
    procedure SetImageAlignment(const Value: TImageAlignment);
    procedure SetImageIndex(const Value: TImageIndex);
    procedure SetImageMargins(const Value: TImageMargins);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetPressedImageIndex(const Value: TImageIndex);
    procedure DrawControlText(Canvas: TCanvas; Details: TThemedElementDetails; const S: string; var R: TRect; Flags: Cardinal; AColor : TColor = clNone);
    procedure SetImageStyle(const Value: TNCImageStyle);
    procedure ShowHintWindow(X, Y : Integer);
    procedure HideHintWindow;
  public
    property Style: TNCButtonStyle read FStyle write SetStyle;
    property ImageStyle: TNCImageStyle read FImageStyle write SetImageStyle;
    property DisabledImageIndex: TImageIndex read FDisabledImageIndex write SetDisabledImageIndex;
    property DropDownMenu: TPopupMenu read FDropDownMenu write SetDropDownMenu;
    property HotImageIndex: TImageIndex read FHotImageIndex write SetHotImageIndex;
    property Images: TCustomImageList read FImages write SetImages;
    property ImageAlignment: TImageAlignment read FImageAlignment write SetImageAlignment;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex;
    property ImageMargins: TImageMargins read FImageMargins write SetImageMargins;
    property PressedImageIndex: TImageIndex read FPressedImageIndex write SetPressedImageIndex;
    property NCControls: TNCControls read FNCControls;
    property OnDropDownClick: TNotifyEvent read FOnDropDownClick write FOnDropDownClick;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    constructor Create(AOwner: TComponent);  reintroduce; virtual;
    destructor Destroy; override;
  published
    property Caption;
  end;

  TFormStyleNCControls = class(TFormStyleHook)
  strict private
    FHotNCBtnIndex     : Integer;
    FPressedNCBtnIndex : Integer;
    FNCControls : TNCControls;
    function  GetButtonIndex(P: TPoint) : Integer;
    function  PointInButton(P: TPoint)  : Boolean;
  private
    function GetNCControls : TNCControls;
  protected
    procedure WMNCMouseMove(var Message: TWMNCHitMessage); message WM_NCMOUSEMOVE;
    procedure WMNCLButtonDown(var Message: TWMNCHitMessage); message WM_NCLBUTTONDOWN;
    procedure WMNCLButtonUp(var Message: TWMNCHitMessage); message WM_NCLBUTTONUP;
    procedure WMNCLButtonDblClk(var Message: TWMNCHitMessage); message WM_NCLBUTTONDBLCLK;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure PaintNCControls(Canvas: TCanvas; ARect : TRect);
    procedure PaintNC(Canvas: TCanvas); override;
  strict protected
    procedure MouseLeave; override;
    procedure MouseEnter; override;
    procedure Restore; override;
    procedure Maximize; override;
    procedure Minimize; override;
  public
    property NCControls : TNCControls read GetNCControls;
    constructor Create(AControl: TWinControl); override;
    destructor Destroy; override;
  end;


{
 TODO
   add more buttons styles  (colors, gradient, glow, link)
   add hot effects (glow, menu sel)
   add support for TAction
}

implementation

uses
 Winapi.CommCtrl,
 System.SysUtils,
 Winapi.UxTheme,
 Vcl.Styles.FormStyleHooks;

type
  THintWindowClass = class(THintWindow);
  TCustomFormClass = class(TCustomForm);
  TStyleHookList = TList<TStyleHookClass>;

  TStyleHookDictionary = TDictionary<TClass, TStyleHookList>;
  TCustomStyleEngineHelper = Class Helper for TCustomStyleEngine
  public
    class function GetRegisteredStyleHooks : TStyleHookDictionary;
  end;


class function TCustomStyleEngineHelper.GetRegisteredStyleHooks: TStyleHookDictionary;
begin
  Result:= Self.FRegisteredStyleHooks;
end;

function  IsStyleHookRegistered(ControlClass: TClass; StyleHookClass: TStyleHookClass) : Boolean;
var
  List : TStyleHookList;
begin
 Result:=False;
    if TCustomStyleEngine.GetRegisteredStyleHooks.ContainsKey(ControlClass) then
    begin
      List := TCustomStyleEngine.GetRegisteredStyleHooks[ControlClass];
      Result:=List.IndexOf(StyleHookClass) <> -1;
    end;
end;

function  GetRegisteredStylesHooks(ControlClass: TClass) : TStyleHookList;
begin
 Result:=nil;
    if TCustomStyleEngine.GetRegisteredStyleHooks.ContainsKey(ControlClass) then
      Result:=TCustomStyleEngine.GetRegisteredStyleHooks[ControlClass];
end;

{ TNCControls }

constructor TNCControls.Create(AOwner: TComponent);
begin
  if not (AOwner is TCustomForm) then Raise EAbort.Create('TNCControls only must be created in forms');
  inherited Create(AOwner);
  FForm  := TCustomForm(AOwner);
  FList  :=TListNCButtons.Create(True);
  FStyleServices:=nil;
  FVisible:=True;
  FShowSystemMenu:=True;
  if not IsStyleHookRegistered(AOwner.ClassType, TFormStyleNCControls) then
    TStyleManager.Engine.RegisterStyleHook(AOwner.ClassType, TFormStyleNCControls);
  FForm.Perform(CM_RECREATEWND, 0, 0);
end;

destructor TNCControls.Destroy;
begin
  FList.Free;
  inherited;
end;


function TNCControls.GetStyleServices: TCustomStyleServices;
begin
 Result:= FStyleServices;
 if Result=nil then
  Result:=Vcl.Themes.StyleServices;
end;

procedure TNCControls.SetStyleServices(const Value: TCustomStyleServices);
begin
 if Value<>FStyleServices then
   FStyleServices:= Value;
end;

procedure TNCControls.SetVisible(const Value: Boolean);
begin
 if Value<>FVisible then
 begin
  FVisible := Value;
  if FForm.HandleAllocated then
    SendMessage(FForm.Handle, WM_NCPAINT, 0, 0);
 end;

end;

{ TNCButton }

constructor TNCButton.Create(AOwner: TComponent);
begin
  if not (AOwner is TNCControls) then Raise EAbort.Create(Format('%s only must be created in %s',[Self.ClassName, 'TNCControls']));
  inherited Create(Owner);
  FNCControls:=TNCControls(AOwner);
  FDropDown:=False;
  FImages  :=nil;
  FImageMargins := TImageMargins.Create;

  FDisabledImageIndex:=-1;
  FPressedImageIndex :=-1;
  FImageIndex        :=-1;
  FHotImageIndex     :=-1;
  FImageAlignment    := iaLeft;
  FStyle             := nsPushButton;
  FImageStyle        := isNormal;
  FDropDownMenu      := nil;

  FOnDropDownClick   :=nil;
  FOnClick           :=nil;
  FHintWindow        :=THintWindow.Create(Self);
end;

destructor TNCButton.Destroy;
begin
  FHintWindow.Free;
  FImageMargins.Free;
  inherited;
end;


procedure TNCButton.DrawControlText(Canvas: TCanvas; Details: TThemedElementDetails; const S: string; var R: TRect; Flags: Cardinal; AColor : TColor = clNone);
var
  ThemeTextColor: TColor;
  TextFormat: TTextFormatFlags;
  LStyleServices : TCustomStyleServices;
begin
  Canvas.Font := Font;
  LStyleServices:=NCControls.StyleServices;
  TextFormat := TTextFormatFlags(Flags);
  if LStyleServices.GetElementColor(Details, ecTextColor, ThemeTextColor) then
  begin
    if AColor<>clNone then
     Canvas.Font.Color := AColor
    else
     Canvas.Font.Color := ThemeTextColor;
    LStyleServices.DrawText(Canvas.Handle, Details, S, R, TextFormat, Canvas.Font.Color);
  end
  else
  begin
    Canvas.Refresh;
    LStyleServices.DrawText(Canvas.Handle, Details, S, R, TextFormat);
  end;
end;

procedure DoDrawGrayImage(hdcDst: HDC; himl: HIMAGELIST; ImageIndex, X, Y: Integer);
var
  pimldp: TImageListDrawParams;
begin
  FillChar(pimldp, SizeOf(pimldp), #0);
  pimldp.fState := ILS_SATURATE;
  pimldp.cbSize := SizeOf(pimldp);
  pimldp.hdcDst := hdcDst;
  pimldp.himl := himl;
  pimldp.i := ImageIndex;
  pimldp.x := X;
  pimldp.y := Y;
  ImageList_DrawIndirect(@pimldp);
end;

procedure TNCButton.DrawButton(ACanvas: TCanvas; AMouseInControl, Pressed: Boolean);
var
  Details:  TThemedElementDetails;
  DrawRect, LRect : TRect;
  IW, IH, IX, IY: Integer;
  SaveIndex: Integer;
  X, Y, I, ImgIndex: Integer;
  BCaption: String;
  LStyleServices : TCustomStyleServices;
  ThemeTextColor : TColor;
begin
  LStyleServices:=NCControls.StyleServices;
  BCaption := Text;
  ImgIndex := ImageIndex;
  if not Enabled then
  begin
    Details := LStyleServices.GetElementDetails(tbPushButtonDisabled);
    if DisabledImageIndex<>-1 then
      ImgIndex := DisabledImageIndex;
  end
  else
  if Pressed then
  begin
    Details := LStyleServices.GetElementDetails(tbPushButtonPressed);
    if PressedImageIndex<>-1 then
      ImgIndex := PressedImageIndex;
  end
  else
  if AMouseInControl then
  begin
    Details := LStyleServices.GetElementDetails(tbPushButtonHot);
    if HotImageIndex<>-1 then
      ImgIndex := HotImageIndex;
  end
  else
  if Enabled then
    Details := LStyleServices.GetElementDetails(tbPushButtonNormal);

  DrawRect := BoundsRect;//ClientRect;

  if Enabled and ((FStyle=nsTranparent) or (FStyle=nsSplitTrans)) and AMouseInControl then
  begin
    Details := LStyleServices.GetElementDetails(tmMenuBarItemHot);
    LStyleServices.DrawElement(ACanvas.Handle, Details, DrawRect);
  end
  else
  if (FStyle<>nsTranparent) and (FStyle<>nsSplitTrans) then
    LStyleServices.DrawElement(ACanvas.Handle, Details, DrawRect);

  if (FImages<>nil) and (FImages.Handle <> 0) and
     ImageList_GetIconSize(FImages.Handle, IW, IH) then
  begin
    IX := DrawRect.Left + (DrawRect.Width  - IW) div 2;
    IY := DrawRect.Top  + (DrawRect.Height - IH) div 2;

       case ImageAlignment of
          iaLeft:
            begin
              IX := DrawRect.Left + 2;
              Inc(IX, ImageMargins.Left);
              Inc(IY, ImageMargins.Top);
              Dec(IY, ImageMargins.Bottom);
              Inc(DrawRect.Left, {IX +} IW + ImageMargins.Right);
            end;
          iaRight:
            begin
              IX := DrawRect.Right - IW - 2;
              Dec(IX, ImageMargins.Right);
              Dec(IX, ImageMargins.Left);
              Inc(IY, ImageMargins.Top);
              Dec(IY, ImageMargins.Bottom);
              DrawRect.Right := IX;
            end;
          iaTop:
            begin
              IX := {DrawRect.Left +} ({DrawRect.Width -} IW) div 2;
              Inc(IX, ImageMargins.Left);
              Dec(IX, ImageMargins.Right);
              IY := DrawRect.Top + 2;
              Inc(IY, ImageMargins.Top);
              Inc(DrawRect.Top, IY + IH + ImageMargins.Bottom);
            end;
          iaBottom:
            begin
              IX := {DrawRect.Left +} ({DrawRect.Width -} IW) div 2;
              Inc(IX, ImageMargins.Left);
              Dec(IX, ImageMargins.Right);
              IY := DrawRect.Bottom - IH - 2;
              Dec(IY, ImageMargins.Bottom);
              Dec(IY, ImageMargins.Top);
              DrawRect.Bottom := IY;
            end;
       end;

//    if AMouseInControl then
//      Dec(IY);
    if Enabled and (( FImageStyle=isNormal) or ((FImageStyle=isGrayHot) and AMouseInControl)) then
     ImageList_Draw(FImages.Handle, ImgIndex, ACanvas.Handle, IX, IY, ILD_NORMAL)
    else
      DoDrawGrayImage(ACanvas.Handle, FImages.Handle, ImgIndex, IX, IY);
  end;

    if (FStyle in [nsSplitButton, nsSplitTrans]) then
    begin
      LRect:=DrawRect;
      Dec(DrawRect.Right, 15);

      if (FStyle=nsSplitTrans) and (not AMouseInControl)  then
       //use font color of the caption
       StyleServices.GetElementColor(StyleServices.GetElementDetails(twCaptionActive), ecTextColor, ThemeTextColor)
      else
       ThemeTextColor:=clNone;
       DrawControlText(ACanvas, Details, Text, DrawRect, DT_VCENTER or DT_CENTER, ThemeTextColor);

      if FDropDown then
      begin
        Details := LStyleServices.GetElementDetails(tbPushButtonPressed);
        SaveIndex := SaveDC(ACanvas.Handle);
        try
          IntersectClipRect(ACanvas.Handle, Width - 15, 0, Width, Height);
          DrawRect := Rect(Width - 30, 0, Width, Height);
          LStyleServices.DrawElement(ACanvas.Handle, Details, DrawRect);
        finally
          RestoreDC(ACanvas.Handle, SaveIndex);
        end;
      end;

      with ACanvas do
      begin
        // draw split line
        if FStyle<>nsSplitTrans then
        begin
          Pen.Color := LStyleServices.GetSystemColor(clBtnShadow);
          MoveTo(LRect.Right - 15, LRect.Top + 3);
          LineTo(LRect.Right - 15, LRect.Bottom - 3);
          if Enabled then
            Pen.Color := LStyleServices.GetSystemColor(clBtnHighLight)
          else
            Pen.Color := Font.Color;
          MoveTo(LRect.Right - 14, LRect.Top + 3);
          LineTo(LRect.Right - 14, LRect.Bottom - 3);
        end;


        // draw arrow
        if (FStyle=nsSplitTrans) and (not AMouseInControl)  then
         Pen.Color := ThemeTextColor
        else
         Pen.Color := Font.Color;

        X := LRect.Right - 8;
        Y := LRect.Top + (Height div 2) + 1;
        for i := 3 downto 0 do
        begin
          MoveTo(X - I, Y - I);
          LineTo(X + I + 1, Y - I);
        end;
      end;
    end
    else
    begin
      if (FStyle=nsTranparent) and (not AMouseInControl)  then
       //use font color of the caption
       StyleServices.GetElementColor(StyleServices.GetElementDetails(twCaptionActive), ecTextColor, ThemeTextColor)
      else
       ThemeTextColor:=clNone;
      DrawControlText(ACanvas, Details, BCaption, DrawRect, DT_VCENTER or DT_CENTER or DT_WORDBREAK, ThemeTextColor);
    end;
end;


procedure TNCButton.SetDisabledImageIndex(const Value: TImageIndex);
begin
  FDisabledImageIndex := Value;
end;

procedure TNCButton.SetDropDownMenu(const Value: TPopupMenu);
begin
  FDropDownMenu := Value;
end;

procedure TNCButton.SetHotImageIndex(const Value: TImageIndex);
begin
  FHotImageIndex := Value;
end;

procedure TNCButton.SetImageAlignment(const Value: TImageAlignment);
begin
  FImageAlignment := Value;
end;

procedure TNCButton.SetImageIndex(const Value: TImageIndex);
begin
  FImageIndex := Value;
end;

procedure TNCButton.SetImageMargins(const Value: TImageMargins);
begin
  FImageMargins := Value;
end;

procedure TNCButton.SetImages(const Value: TCustomImageList);
begin
  FImages := Value;
end;

procedure TNCButton.SetImageStyle(const Value: TNCImageStyle);
begin
  FImageStyle := Value;
end;

procedure TNCButton.SetPressedImageIndex(const Value: TImageIndex);
begin
  FPressedImageIndex := Value;
end;

procedure TNCButton.SetStyle(const Value: TNCButtonStyle);
begin
  FStyle := Value;
end;

procedure TNCButton.ShowHintWindow(X, Y : Integer);
begin
  if THintWindowClass(FHintWindow).WindowHandle=0 then
  begin
    FHintWindow.Visible := False;
    FHintWindow.Color   := StyleServices.GetSystemColor(clInfoBk);
    FHintWindow.Caption := Hint;
    FHintWindow.ParentWindow := Application.Handle;
    FHintWindow.Left:=NCControls.FForm.Left + BoundsRect.Left;
    FHintWindow.Top :=NCControls.FForm.Top +  BoundsRect.Bottom+5;
    FHintWindow.Show;
  end;
end;

procedure TNCButton.HideHintWindow;
begin
 if THintWindowClass(FHintWindow).WindowHandle<>0 then
  FHintWindow.ReleaseHandle;
end;


{ TFormStyleNCControls }
constructor TFormStyleNCControls.Create(AControl: TWinControl);
begin
  inherited;
  FHotNCBtnIndex:=0;
  FPressedNCBtnIndex:=0;
  FNCControls:=nil;
end;

destructor TFormStyleNCControls.Destroy;
begin
  inherited;
end;

function TFormStyleNCControls.GetButtonIndex(P: TPoint): Integer;
var
 i : Integer;
begin
 Result:=-1;
  if (NCControls<>nil) then
   for i:=0 to FNCControls.List.Count-1 do
     if FNCControls.List[i].Visible and PtInRect(FNCControls.List[i].BoundsRect, P)  then
      Exit(i);
end;


function TFormStyleNCControls.GetNCControls: TNCControls;
var
 i :  integer;
begin
 Result:=FNCControls;
  if Result=nil then
  for i:=0 to Form.ComponentCount-1 do
   if Form.Components[i] is TNCControls then
    begin
      FNCControls:=TNCControls(Form.Components[i]);
      Exit(FNCControls);
    end;
end;

function TFormStyleNCControls.PointInButton(P: TPoint): Boolean;
begin
  Result:=GetButtonIndex(P) >= 0;
end;


procedure TFormStyleNCControls.Maximize;
begin
  if Handle <> 0 then
  begin
    FPressedNCBtnIndex := -1;
    FHotNCBtnIndex     := -1;
  end;
  inherited;
end;

procedure TFormStyleNCControls.Minimize;
begin
  if Handle <> 0 then
  begin
    FPressedNCBtnIndex := -1;
    FHotNCBtnIndex     := -1;
  end;
  inherited;
end;

procedure TFormStyleNCControls.MouseEnter;
begin
  inherited;
  FPressedNCBtnIndex := -1;
end;

procedure TFormStyleNCControls.MouseLeave;
begin
  inherited;
  if FHotNCBtnIndex <> -1 then
  begin
    FHotNCBtnIndex     := -1;
    FPressedNCBtnIndex := -1;
    if Form.BorderStyle <> bsNone then
      InvalidateNC;
  end;
end;

procedure TFormStyleNCControls.Restore;
begin
  FPressedNCBtnIndex := -1;
  FHotNCBtnIndex := -1;
  inherited;
end;

procedure TFormStyleNCControls.PaintNCControls(Canvas: TCanvas; ARect : TRect);
var
  i : Integer;
  LNCButton : TNCButton;
begin
  if (NCControls<>nil) and (NCControls.List.Count>0) and (NCControls.Visible) then
   for i:=0 to NCControls.List.Count-1 do
   begin
    LNCButton:=NCControls.List[i];
    if LNCButton.Visible and (LNCButton.BoundsRect.Right<= ARect.Right) then
     LNCButton.DrawButton(Canvas, FHotNCBtnIndex=i, FPressedNCBtnIndex=i);
   end;
end;


procedure TFormStyleNCControls.WMNCHitTest(var Message: TWMNCHitTest);
var
  P : TPoint;
  LHitTest : Integer;
begin
  if (NCControls<>nil) and (NCControls.Visible) then
  begin
      {$IF CompilerVersion>23}
      if (TStyleManager.FormBorderStyle = fbsCurrentStyle) and (seBorder in Form.StyleElements) then
      {$IFEND}
      begin
        P := _NormalizePoint(Point(Message.XPos, Message.YPos));
        LHitTest := _GetHitTest(P);
        if (LHitTest<>HTSYSMENU) or ((LHitTest=HTSYSMENU)  and NCControls.ShowSystemMenu) then
        begin
          Message.Result := LHitTest;
          Handled := True;
        end
        else
        begin
          Message.Result := WM_NULL;
          Handled := True;
        end;
      end;
  end
  else
   inherited;
end;

//Avoid maximize or restore on DblClk

procedure TFormStyleNCControls.WMNCLButtonDblClk(var Message: TWMNCHitMessage);
var
 P : TPoint;
begin
  if (NCControls<>nil) and (NCControls.Visible) then
  begin
    P := _NormalizePoint(Point(Message.XCursor, Message.YCursor));
    if ((Message.HitTest = HTTOP) or (Message.HitTest = HTCAPTION)) and PointInButton(P) then
    begin
      Message.Result := 0;
      Message.Msg := WM_NULL;
      Handled := True;
      Exit;
    end;
  end;
  inherited;
end;

procedure TFormStyleNCControls.WMNCLButtonDown(var Message: TWMNCHitMessage);
var
 P : TPoint;
begin
  inherited;
  {$IF CompilerVersion>23}
  if not ((TStyleManager.FormBorderStyle = fbsCurrentStyle) and (seBorder in Form.StyleElements)) then
  begin
    Handled := False;
    Exit;
  end;
  {$IFEND}

  if (NCControls<>nil) and (NCControls.Visible) then
  begin
    P := _NormalizePoint(Point(Message.XCursor, Message.YCursor));
    if ((Message.HitTest = HTTOP) or (Message.HitTest = HTCAPTION)) and PointInButton(P) then
    begin
      FPressedNCBtnIndex := GetButtonIndex(P);
      InvalidateNC;
      Message.Result := 0;
      Message.Msg := WM_NULL;
      Handled := True;
    end;
  end;

end;

procedure TFormStyleNCControls.WMNCLButtonUp(var Message: TWMNCHitMessage);
var
  I, OldIndex : Integer;
  P : TPoint;
  LRect : TRect;
  LNCButtton : TNCButton;
begin
  inherited;

  if (NCControls<>nil) and (NCControls.Visible) then
  begin
   {$IF CompilerVersion>23}
    if not ((TStyleManager.FormBorderStyle = fbsCurrentStyle) and (seBorder in Form.StyleElements)) then
    begin
      Handled := False;
      Exit;
    end;
   {$IFEND}

    OldIndex := FPressedNCBtnIndex;

    if FPressedNCBtnIndex <> -1 then
    begin
      FPressedNCBtnIndex := -1;
      InvalidateNC;
    end;

    if OldIndex = FHotNCBtnIndex then
    begin
      P := _NormalizePoint(Point(Message.XCursor, Message.YCursor));
      I :=  GetButtonIndex(P);
      if ((Message.HitTest = HTTOP) or (Message.HitTest = HTCAPTION)) and (I>=0) then
      begin
        LRect:=Rect(0, 0, 0, 0);
        if (NCControls<>nil) then
        begin
         LRect:=NCControls.List[i].BoundsRect;
         LRect.Left:=LRect.Right-15;
        end;

        LNCButtton:= NCControls.List[i];
        if (LNCButtton.Enabled) and Assigned(LNCButtton.FOnDropDownClick) and (LNCButtton.Style in [nsSplitButton, nsSplitTrans]) and PtInRect(LRect, P) then
          LNCButtton.FOnDropDownClick(LNCButtton)
        else
        if (LNCButtton.Enabled) and Assigned(LNCButtton.FDropDownMenu) and PtInRect(LRect, P) then
          LNCButtton.FDropDownMenu.Popup(Form.Left + LNCButtton.BoundsRect.Left, Form.Top + LNCButtton.BoundsRect.Bottom)
        else
        if (LNCButtton.Enabled) and Assigned(LNCButtton.FOnClick) then
            LNCButtton.FOnClick(LNCButtton);
      end;
    end;

    Message.Result := 0;
    Message.Msg := WM_NULL;
    Handled := True;
  end;
end;

procedure TFormStyleNCControls.WMNCMouseMove(var Message: TWMNCHitMessage);
var
 I : Integer;
 P : TPoint;
begin
  inherited;
  if (NCControls<>nil) and (NCControls.Visible) then
  begin
    P := _NormalizePoint(Point(Message.XCursor, Message.YCursor));
    //OutputDebugString(PChar(Format('Message.HitTest %d XCursor %d YCursor %d  P.X %d  P.Y %d',[Message.HitTest, Message.XCursor, Message.YCursor, P.X, P.Y])));

    {$IF CompilerVersion>23}
    if not ((TStyleManager.FormBorderStyle = fbsCurrentStyle) and (seBorder in Form.StyleElements)) then
    begin
      Handled := False;
      Exit;
    end;
    {$IFEND}

    if ((Message.HitTest = HTTOP) or (Message.HitTest = HTCAPTION) or (not  NCControls.ShowSystemMenu and (Message.HitTest = HTSYSMENU) )) and PointInButton(P) then
    begin
      if FHotNCBtnIndex <> GetButtonIndex(P) then
      begin
        FHotNCBtnIndex := GetButtonIndex(P);

        for I := 0 to NCControls.List.Count-1 do
          if (FHotNCBtnIndex<>I) and NCControls.List[I].ShowHint then
              NCControls.List[I].HideHintWindow();

        if NCControls.List[FHotNCBtnIndex].ShowHint then
          NCControls.List[FHotNCBtnIndex].ShowHintWindow(Message.XCursor, Message.YCursor);

        InvalidateNC;
      end;
    end
    else if FHotNCBtnIndex <> -1 then
    begin
      for I := 0 to NCControls.List.Count-1 do
        if NCControls.List[I].ShowHint then
            NCControls.List[I].HideHintWindow();

      FHotNCBtnIndex := -1;
      InvalidateNC;
    end;
  end;
end;


procedure TFormStyleNCControls.PaintNC(Canvas: TCanvas);
var
  Details, CaptionDetails, IconDetails: TThemedElementDetails;
  Detail: TThemedWindow;
  R, R1, R2, DrawRect, ButtonRect, TextRect: TRect;
  CaptionBuffer: TBitmap;
  FButtonState: TThemedWindow;
  TextFormat: TTextFormat;
  LText: string;
  LStyleServices :  TCustomStyleServices;
begin
  if Form.BorderStyle = bsNone then
  begin
    MainMenuBarHookPaint(Canvas);    Exit;
  end;

//  if NCControls<>nil then
//    LStyleServices:=NCControls.StyleServices
//  else
    LStyleServices:=StyleServices;

  _FCloseButtonRect := Rect(0, 0, 0, 0);
  _FMaxButtonRect := Rect(0, 0, 0, 0);
  _FMinButtonRect := Rect(0, 0, 0, 0);
  _FHelpButtonRect := Rect(0, 0, 0, 0);
  _FSysMenuButtonRect := Rect(0, 0, 0, 0);
  _FCaptionRect := Rect(0, 0, 0, 0);

  if not LStyleServices.Available then
    Exit;
  R := _GetBorderSize;


  if (Form.BorderStyle <> bsToolWindow) and
     (Form.BorderStyle <> bsSizeToolWin) then
  begin
    if _FFormActive then
      Detail := twCaptionActive
    else
      Detail := twCaptionInActive
  end
  else
  begin
   if _FFormActive then
      Detail := twSmallCaptionActive
    else
      Detail := twSmallCaptionInActive
  end;
  CaptionBuffer := TBitmap.Create;
  CaptionBuffer.SetSize(_FWidth, R.Top);

  //caption
  DrawRect := Rect(0, 0, CaptionBuffer.Width, CaptionBuffer.Height);
  Details := LStyleServices.GetElementDetails(Detail);
  LStyleServices.DrawElement(CaptionBuffer.Canvas.Handle, Details, DrawRect);
  TextRect := DrawRect;
  CaptionDetails := Details;

  //icon
  if ((NCControls<>nil) and NCControls.ShowSystemMenu)  and
     (biSystemMenu in TCustomFormClass(Form).BorderIcons) and
     (Form.BorderStyle <> bsDialog) and
     (Form.BorderStyle <> bsToolWindow) and
     (Form.BorderStyle <> bsSizeToolWin) then
  begin
    IconDetails := LStyleServices.GetElementDetails(twSysButtonNormal);
    if not LStyleServices.GetElementContentRect(0, IconDetails, DrawRect, ButtonRect) then
      ButtonRect := Rect(0, 0, 0, 0);
    R1 := Rect(0, 0, GetSystemMetrics(SM_CXSMICON), GetSystemMetrics(SM_CYSMICON));
    RectVCenter(R1, ButtonRect);
    if ButtonRect.Width > 0 then
      DrawIconEx(CaptionBuffer.Canvas.Handle, R1.Left, R1.Top, _GetIconFast.Handle, 0, 0, 0, 0, DI_NORMAL);
    Inc(TextRect.Left, ButtonRect.Width + 5);
    _FSysMenuButtonRect := ButtonRect;
  end
  else
    Inc(TextRect.Left, R.Left);

  //buttons
  if (biSystemMenu in TCustomFormClass(Form).BorderIcons) then
  begin
    if (Form.BorderStyle <> bsToolWindow) and
       (Form.BorderStyle <> bsSizeToolWin) then
    begin
        if (_FPressedButton = HTCLOSE) and (_FHotButton = HTCLOSE) then
          FButtonState :=twCloseButtonPushed
        else
        if _FHotButton = HTCLOSE then
          FButtonState :=twCloseButtonHot
        else
        if _FFormActive then
          FButtonState :=twCloseButtonNormal
        else
          FButtonState :=twCloseButtonDisabled;
     end
    else
    begin
      if (_FPressedButton = HTCLOSE) and (_FHotButton = HTCLOSE) then
        FButtonState := twSmallCloseButtonPushed
      else if _FHotButton = HTCLOSE then
        FButtonState := twSmallCloseButtonHot
      else
        if _FFormActive then
          FButtonState := twSmallCloseButtonNormal
        else
          FButtonState := twSmallCloseButtonDisabled;
    end;

    Details := LStyleServices.GetElementDetails(FButtonState);
    if not LStyleServices.GetElementContentRect(0, Details, DrawRect, ButtonRect) then
      ButtonRect := Rect(0, 0, 0, 0);

    LStyleServices.DrawElement(CaptionBuffer.Canvas.Handle, Details, ButtonRect);

    if ButtonRect.Left > 0 then
      TextRect.Right := ButtonRect.Left;
    _FCloseButtonRect := ButtonRect;
  end;

  if (biMaximize in TCustomFormClass(Form).BorderIcons) and
     (biSystemMenu in TCustomFormClass(Form).BorderIcons) and
     (Form.BorderStyle <> bsDialog) and
     (Form.BorderStyle <> bsToolWindow) and
     (Form.BorderStyle <> bsSizeToolWin) then
  begin
    if Form.WindowState = wsMaximized then
    begin

      if (_FPressedButton = HTMAXBUTTON) and (_FHotButton = HTMAXBUTTON) then
        FButtonState := twRestoreButtonPushed
      else
      if _FHotButton = HTMAXBUTTON then
        FButtonState := twRestoreButtonHot
      else
      if _FFormActive then
        FButtonState := twRestoreButtonNormal
      else
        FButtonState := twRestoreButtonDisabled;
    end
    else
    begin
      if (_FPressedButton = HTMAXBUTTON) and (_FHotButton = HTMAXBUTTON) then
        FButtonState := twMaxButtonPushed
      else
      if _FHotButton = HTMAXBUTTON then
        FButtonState := twMaxButtonHot
      else
      if _FFormActive then
        FButtonState := twMaxButtonNormal
      else
        FButtonState := twMaxButtonDisabled;
    end;
    Details := LStyleServices.GetElementDetails(FButtonState);

    if not LStyleServices.GetElementContentRect(0, Details, DrawRect, ButtonRect) then
      ButtonRect := Rect(0, 0, 0, 0);
    if ButtonRect.Width > 0 then
      LStyleServices.DrawElement(CaptionBuffer.Canvas.Handle, Details, ButtonRect);

    if ButtonRect.Left > 0 then
      TextRect.Right := ButtonRect.Left;
    _FMaxButtonRect := ButtonRect;
  end;

  if (biMinimize in TCustomFormClass(Form).BorderIcons) and
     (biSystemMenu in TCustomFormClass(Form).BorderIcons) and
     (Form.BorderStyle <> bsDialog) and
     (Form.BorderStyle <> bsToolWindow) and
     (Form.BorderStyle <> bsSizeToolWin) then
  begin
      if (_FPressedButton = HTMINBUTTON) and (_FHotButton = HTMINBUTTON) then
        FButtonState := twMinButtonPushed
      else
      if _FHotButton = HTMINBUTTON then
        FButtonState := twMinButtonHot
      else
      if _FFormActive then
        FButtonState := twMinButtonNormal
      else
        FButtonState := twMinButtonDisabled;

    Details := LStyleServices.GetElementDetails(FButtonState);

    if not LStyleServices.GetElementContentRect(0, Details, DrawRect, ButtonRect) then
      ButtonRect := Rect(0, 0, 0, 0);
    if ButtonRect.Width > 0 then
      LStyleServices.DrawElement(CaptionBuffer.Canvas.Handle, Details, ButtonRect);

    if ButtonRect.Left > 0 then TextRect.Right := ButtonRect.Left;
    _FMinButtonRect := ButtonRect;
  end;

  if (biHelp in TCustomFormClass(Form).BorderIcons) and (biSystemMenu in TCustomFormClass(Form).BorderIcons) and
     ((not (biMaximize in TCustomFormClass(Form).BorderIcons) and
     not (biMinimize in TCustomFormClass(Form).BorderIcons)) or (Form.BorderStyle = bsDialog))
  then
  begin
    if (_FPressedButton = HTHELP) and (_FHotButton = HTHELP) then
      FButtonState := twHelpButtonPushed
    else if _FHotButton = HTHELP then
      FButtonState := twHelpButtonHot
    else
    if _FFormActive then
      FButtonState := twHelpButtonNormal
    else
      FButtonState := twHelpButtonDisabled;
    Details := LStyleServices.GetElementDetails(FButtonState);

    if not LStyleServices.GetElementContentRect(0, Details, DrawRect, ButtonRect) then
      ButtonRect := Rect(0, 0, 0, 0);
    if ButtonRect.Width > 0 then
      LStyleServices.DrawElement(CaptionBuffer.Canvas.Handle, Details, ButtonRect);

    if ButtonRect.Left > 0 then
      TextRect.Right := ButtonRect.Left;
    _FHelpButtonRect := ButtonRect;
  end;

  R2:=TextRect;
  if (NCControls<>nil) and  (NCControls.List.Count>0) and (NCControls.Visible) then
   Inc(TextRect.Left, NCControls.List[NCControls.List.Count-1].BoundsRect.Right - NCControls.List[0].BoundsRect.Left + 10);

  //text
  TextFormat := [tfLeft, tfSingleLine, tfVerticalCenter];
  if Control.UseRightToLeftReading then
    Include(TextFormat, tfRtlReading);

  LText := Text;
  LStyleServices.DrawText(CaptionBuffer.Canvas.Handle, CaptionDetails, LText, TextRect, TextFormat);

  if (NCControls<>nil) and (NCControls.List.Count>0) and (NCControls.Visible) then
   Dec(TextRect.Left, NCControls.List[NCControls.List.Count-1].BoundsRect.Right - NCControls.List[0].BoundsRect.Left + 10);


  _FCaptionRect := TextRect;
  PaintNCControls(CaptionBuffer.Canvas, R2);

  //caption
  Canvas.Draw(0, 0, CaptionBuffer);
  CaptionBuffer.Free;
  //menubar
  MainMenuBarHookPaint(Canvas);

  //left
  if (Form.BorderStyle <> bsToolWindow) and
     (Form.BorderStyle <> bsSizeToolWin) then
  begin
    if _FFormActive then
      Detail := twFrameLeftActive
    else
      Detail := twFrameLeftInActive
  end
  else
  begin
    if _FFormActive then
      Detail := twSmallFrameLeftActive
    else
      Detail := twSmallFrameLeftInActive
  end;
  DrawRect := Rect(0, R.Top, R.Left, _FHeight - R.Bottom);
  Details := LStyleServices.GetElementDetails(Detail);

  if DrawRect.Bottom - DrawRect.Top > 0 then
    LStyleServices.DrawElement(Canvas.Handle, Details, DrawRect);

  //right
  if (Form.BorderStyle <> bsToolWindow) and
     (Form.BorderStyle <> bsSizeToolWin) then
  begin
    if _FFormActive then
      Detail := twFrameRightActive
    else
      Detail := twFrameRightInActive
  end
  else
  begin
   if _FFormActive then
      Detail := twSmallFrameRightActive
    else
      Detail := twSmallFrameRightInActive
  end;
  DrawRect := Rect(_FWidth - R.Right, R.Top, _FWidth, _FHeight - R.Bottom);
  Details := LStyleServices.GetElementDetails(Detail);

  if DrawRect.Bottom - DrawRect.Top > 0 then
    LStyleServices.DrawElement(Canvas.Handle, Details, DrawRect);

  //Bottom
  if (Form.BorderStyle <> bsToolWindow) and
     (Form.BorderStyle <> bsSizeToolWin) then
  begin
    if _FFormActive then
      Detail := twFrameBottomActive
    else
      Detail := twFrameBottomInActive
  end
  else
  begin
   if _FFormActive then
      Detail := twSmallFrameBottomActive
    else
      Detail := twSmallFrameBottomInActive
  end;
  DrawRect := Rect(0, _FHeight - R.Bottom, _FWidth, _FHeight);
  Details := LStyleServices.GetElementDetails(Detail);

  if DrawRect.Bottom - DrawRect.Top > 0 then
    LStyleServices.DrawElement(Canvas.Handle, Details, DrawRect);
end;

end.

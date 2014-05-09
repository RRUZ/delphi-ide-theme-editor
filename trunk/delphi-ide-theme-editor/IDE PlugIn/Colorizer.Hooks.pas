//**************************************************************************************************
//
// Unit Colorizer.Hooks
// unit Colorizer.Hooks for the Delphi IDE Colorizer
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is Colorizer.Hooks.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2014 Rodrigo Ruz V.
// All Rights Reserved.
//
//**************************************************************************************************

unit Colorizer.Hooks;

interface

 procedure InstallColorizerHooks;
 procedure RemoveColorizerHooks;


implementation

uses
{$IF CompilerVersion >= 23}
  Vcl.Styles,
  Vcl.Themes,
  Messages,
  Controls,
{$IFEND}
  Forms,
  System.IOUtils,
  ExtCtrls,
  Dialogs,
  ComCtrls,
  Windows,
  Classes,
  uDelphiVersions,
  uDelphiIDEHighlight,
  SysUtils,
  Graphics,
  ImgList,
  CommCtrl,
  JclDebug,
  PngImage,
  Colorizer.Utils,
  CaptionedDockTree,
  GraphUtil,
  DDetours;

type
 TWinControlClass        = class(TWinControl);
 TCustomPanelClass       = class(TCustomPanel);
 TCustomStatusBarClass   = class(TCustomStatusBar);
 TDockCaptionDrawerClass = class(TDockCaptionDrawer);
 TUxThemeStyleClass      = class(TUxThemeStyle);
 TCustomFormClass        = class(TCustomForm);
 TBrushClass             = class(TBrush);
 TCustomListViewClass    = class(TCustomListView);
var
  TrampolineCustomImageList_DoDraw     : procedure(Self: TObject; Index: Integer; Canvas: TCanvas; X, Y: Integer; Style: Cardinal; Enabled: Boolean) = nil;
  Trampoline_TCanvas_FillRect          : procedure(Self: TCanvas;const Rect: TRect) = nil;
  Trampoline_TStyleEngine_HandleMessage: function(Self: TStyleEngine; Control: TWinControl; var Message: TMessage; DefWndProc: TWndMethod): Boolean = nil;
  Trampoline_TCustomStatusBar_WMPAINT  : procedure(Self: TCustomStatusBarClass; var Message: TWMPaint) = nil;
  Trampoline_TDockCaptionDrawer_DrawDockCaption : function (Self : TDockCaptionDrawerClass;const Canvas: TCanvas; CaptionRect: TRect; State: TParentFormState): TDockCaptionHitTest =nil;
  Trampoline_TUxThemeStyle_DoDrawElement        : function (Self : TUxThemeStyle;DC: HDC; Details: TThemedElementDetails; const R: TRect; ClipRect: PRect = nil): Boolean = nil;
  Trampoline_TCustomListView_HeaderWndProc : procedure (Self:TCustomListView;var Message: TMessage) = nil;
  Trampoline_ProjectTree2PaintText         : procedure(Self : TObject; Sender: TObject{TBaseVirtualTree}; const TargetCanvas: TCanvas; Node: {PVirtualNode}Pointer; Column: Integer{TColumnIndex}; TextType: Byte {TVSTTextType})=nil;
  Trampoline_DrawText                      : function (hDC: HDC; lpString: LPCWSTR; nCount: Integer;  var lpRect: TRect; uFormat: UINT): Integer; stdcall = nil;
  Trampoline_GetSysColor                   : function (nIndex: Integer): DWORD; stdcall = nil;

  FGutterBkColor : TColor = clNone;

type
  TCustomStatusBarHelper = class helper for TCustomStatusBar
  private
    function GetCanvasRW: TCanvas;
    procedure SetCanvasRW(const Value: TCanvas);
  public
    function  WMPaintAddress: Pointer;
    procedure DoUpdatePanels(UpdateRects, UpdateText: Boolean);
    property  CanvasRW : TCanvas read GetCanvasRW Write SetCanvasRW;
   end;

  TCustomFormHelper = class helper for TCustomForm
  public
    function  SetVisibleAddress: Pointer;
   end;

  TCustomListViewHelper = class helper for TCustomListView
  public
    function  HeaderWndProcAddress: Pointer;
    function  GetHeaderHandle: HWND;
   end;

{ TCustomStatusBarHelper }

procedure TCustomStatusBarHelper.DoUpdatePanels(UpdateRects,
  UpdateText: Boolean);
begin
  Self.UpdatePanels(UpdateRects, UpdateText);
end;

function TCustomStatusBarHelper.GetCanvasRW: TCanvas;
begin
 Result:= Self.FCanvas;
end;

procedure TCustomStatusBarHelper.SetCanvasRW(const Value: TCanvas);
begin
 Self.FCanvas:= Value;
end;

function TCustomStatusBarHelper.WMPaintAddress: Pointer;
var
  MethodAddr: procedure(var Message: TWMPaint) of object;
begin
  MethodAddr := Self.WMPaint;
  Result     := TMethod(MethodAddr).Code;
end;

{ TCustomFormHelper }

function TCustomFormHelper.SetVisibleAddress: Pointer;
var
  MethodAddr: procedure(Value: Boolean) of object;
begin
  MethodAddr := Self.SetVisible;
  Result     := TMethod(MethodAddr).Code;
end;

{ TCustomListViewHelper }

function TCustomListViewHelper.GetHeaderHandle: HWND;
begin
  Result:=Self.FHeaderHandle;
end;

function TCustomListViewHelper.HeaderWndProcAddress: Pointer;
var
  MethodAddr: procedure(var Message: TMessage) of object;
begin
  MethodAddr := Self.HeaderWndProc;
  Result     := TMethod(MethodAddr).Code;
end;


 //@Editorcontrol@TCustomEditControl@EVFillGutter$qqrrx18
 //002F0A00 11656 219E __fastcall Editorcontrol::TCustomEditControl::EVFillGutter(System::Types::TRect&, unsigned short, int, bool, int)
//  Trampoline_EVFillGutter              : procedure(Self: TObject;ARect:TRect; p1 : USHORT; p2 : Integer; p3 :Boolean; p4 : Integer);
//  Addr_EVFillGutter                    : Pointer;
//Const
  //FooMethod='@Editcolorpage@TEditorColor@SetColorSpeedSetting$qqr26Vedopts@TColorSpeedSetting';
  //FooMethod='@Editcolorpage@TEditorColor@ColorSpeedSettingClick$qqrp14System@TObject';


function GetBplMethodAddress(Method: Pointer): Pointer;
type
  PJmpCode = ^TJmpCode;
  TJmpCode = packed record
    Code: Word;
    Addr: ^Pointer;
  end;
const
  csJmp32Code = $25FF;
begin
  if PJmpCode(Method)^.Code = csJmp32Code then
    Result := PJmpCode(Method)^.Addr^
  else
    Result := Method;
end;

procedure Bitmap2GrayScale(const BitMap: TBitmap);
type
  TRGBArray = array[0..32767] of TRGBTriple;
  PRGBArray = ^TRGBArray;
var
  x, y, Gray: Integer;
  Row       : PRGBArray;
begin
  BitMap.PixelFormat := pf24Bit;
  for y := 0 to BitMap.Height - 1 do
  begin
    Row := BitMap.ScanLine[y];
    for x := 0 to BitMap.Width - 1 do
    begin
      Gray             := (Row[x].rgbtRed + Row[x].rgbtGreen + Row[x].rgbtBlue) div 3;
      Row[x].rgbtRed   := Gray;
      Row[x].rgbtGreen := Gray;
      Row[x].rgbtBlue  := Gray;
    end;
  end;
end;

function GetRGBColor(Value: TColor): DWORD;
begin
  Result := ColorToRGB(Value);
  case Result of
    clNone    :  Result := CLR_NONE;
    clDefault :  Result := CLR_DEFAULT;
  end;
end;

type
  TCustomImageListClass = class(TCustomImageList);

procedure CustomImageListHack_DoDraw(Self: TObject; Index: Integer; Canvas: TCanvas; X, Y: Integer; Style: Cardinal; Enabled: Boolean);
var
  MaskBitMap : TBitmap;
  GrayBitMap : TBitmap;
  LImageList : TCustomImageListClass;
begin
  if Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled then
    begin
      LImageList:=TCustomImageListClass(Self);
      if not LImageList.HandleAllocated then Exit;
      if Enabled then
        ImageList_DrawEx(LImageList.Handle, Index, Canvas.Handle, X, Y, 0, 0, GetRGBColor(LImageList.BkColor), GetRGBColor(LImageList.BlendColor), Style)
      else
      begin
        GrayBitMap := TBitmap.Create;
        MaskBitMap := TBitmap.Create;
        try
          GrayBitMap.SetSize(LImageList.Width, LImageList.Height);
          MaskBitMap.SetSize(LImageList.Width, LImageList.Height);
          LImageList.GetImages(Index, GrayBitMap, MaskBitMap);
          Bitmap2GrayScale(GrayBitMap);
          BitBlt(Canvas.Handle, X, Y, LImageList.Width, LImageList.Height, MaskBitMap.Canvas.Handle, 0, 0, SRCERASE);
          BitBlt(Canvas.Handle, X, Y, LImageList.Width, LImageList.Height, GrayBitMap.Canvas.Handle, 0, 0, SRCINVERT);
        finally
          GrayBitMap.Free;
          MaskBitMap.Free;
        end;
      end;
    end
  else
    TrampolineCustomImageList_DoDraw(Self, Index, Canvas, X, Y, Style, Enabled);
end;

//Retuns the current Gutter color , using the background of the current syntax highlighter
function GetGutterBkColor : TColor;
var
  ATheme : TIDETheme;
  sColor : string;
begin
  if FGutterBkColor<>clNone then
   Result:=FGutterBkColor
  else
  begin
    if Assigned(TColorizerLocalSettings.IDEData) then
    begin
      ImportDelphiIDEThemeFromReg(ATheme, TColorizerLocalSettings.IDEData.Version, False);
      sColor:=ATheme[LineNumber].BackgroundColorNew;
      try
        Result:=StringToColor(sColor);
      except
        if Assigned(TColorizerLocalSettings.ColorMap) then
         Result:=TColorizerLocalSettings.ColorMap.Color
        else
         Result:=clBtnFace;
      end;
      FGutterBkColor:=Result;
    end
    else
    if Assigned(TColorizerLocalSettings.ColorMap) then
      Result:=TColorizerLocalSettings.ColorMap.Color
    else
      Result:=clBtnFace
  end;
end;

//Hook for paint the gutter of the TEditControl and the bacgrounf of the TGradientTabSet component
procedure  CustomFillRect(Self: TCanvas;const Rect: TRect);
var
  sCaller : string;
begin
   if Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled and Assigned(TColorizerLocalSettings.ColorMap) and  (Self.Brush.Color=clBtnFace) then
   begin
     sCaller := ProcByLevel(1);
     //TFile.AppendAllText('C:\Delphi\google-code\DITE\delphi-ide-theme-editor\IDE PlugIn\CustomFillRect.txt', Format('%s %s',[sCaller, SLineBreak]));
     if SameText(sCaller, 'EditorControl.TCustomEditControl.EVFillGutter') then
        Self.Brush.Color:=GetGutterBkColor
     else
      if SameText(sCaller, 'GDIPlus.GradientTabs.TGradientTabSet.DrawTabsToMemoryBitmap') then
        Self.Brush.Color:=TColorizerLocalSettings.ColorMap.Color;
   end;
   Trampoline_TCanvas_FillRect(Self, Rect);
end;


//Hook for paint the header of the TVirtualStringTree component
function CustomDrawElement(Self : TUxThemeStyle;DC: HDC; Details: TThemedElementDetails; const R: TRect; ClipRect: PRect = nil): Boolean;
const
  HP_HEADERITEMRIGHT = 3;
var
  sCaller : string;
  LCanvas : TCanvas;
  SaveIndex: Integer;
begin

   if Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled and Assigned(TColorizerLocalSettings.ColorMap) and (Details.Element = teHeader) {and (Details.Part=HP_HEADERITEMRIGHT) } then
   begin
    sCaller := ProcByLevel(2);
    if SameText(sCaller, 'IDEVirtualTrees.TVirtualTreeColumns.PaintHeader') then
    begin
       SaveIndex := SaveDC(DC);
       LCanvas:=TCanvas.Create;
       try
         LCanvas.Handle:=DC;
//         LCanvas.Brush.Color:=TColorizerLocalSettings.ColorMap.Color;
//         LCanvas.FillRect(R);
          GradientFillCanvas(LCanvas, TColorizerLocalSettings.ColorMap.Color, TColorizerLocalSettings.ColorMap.HighlightColor, R, gdVertical);
          LCanvas.Brush.Style:=TBrushStyle.bsClear;
          LCanvas.Pen.Color:=TColorizerLocalSettings.ColorMap.FrameTopLeftInner;
          LCanvas.Rectangle(R);
       finally
          LCanvas.Handle:=0;
          LCanvas.Free;
          RestoreDC(DC, SaveIndex);
       end;

       exit(True);
    end;
   end;
   Result:=Trampoline_TUxThemeStyle_DoDrawElement(Self, DC, Details, R, ClipRect);
end;

//function CustomGetElementColor(Self : TUxThemeStyle;Details: TThemedElementDetails; ElementColor: TElementColor; out Color: TColor): Boolean;
//var
//  sCaller : string;
//begin
//    sCaller := ProcByLevel(2);
//
// TFile.AppendAllText('C:\Delphi\google-code\DITE\delphi-ide-theme-editor\IDE PlugIn\CustomGetElementColor.txt', Format('%s %s',[sCaller, SLineBreak]));
//
//   if (ElementColor=ecTextColor) and Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled and Assigned(TColorizerLocalSettings.ColorMap) { and (Details.Element = teHeader) {and (Details.Part=HP_HEADERITEMRIGHT) } then
//   begin
//    //sCaller := ProcByLevel(2);
//    //if SameText(sCaller, 'IDEVirtualTrees.TVirtualTreeColumns.PaintHeader') then
//    begin
//       Color:=TColorizerLocalSettings.ColorMap.FontColor;
//       exit(True);
//    end;
//   end;
//   Result:=Trampoline_TUxThemeStyle_DoGetElementColor(Self, Details, ElementColor, Color);
//end;

//Hook, for avoid apply a VCL Style to a TWinControl in desing time
function CustomHandleMessage(Self: TStyleEngine; Control: TWinControl; var Message: TMessage; DefWndProc: TWndMethod): Boolean;
begin
  if Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled and Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.UseVCLStyles then
  begin
    Result:=False;
    if not Assigned(Control) then exit;
    if csDesigning in Control.ComponentState then  exit;
  end;
  Result:=Trampoline_TStyleEngine_HandleMessage(Self, Control, Message, DefWndProc);
end;

//Hook for paint IDE TStatusBar
procedure CustomStatusBarWMPaint(Self: TCustomStatusBarClass; var Message: TWMPaint);
var
  DC: HDC;
  Buffer: TBitmap;
  LCanvas: TCanvas;
  PS: TPaintStruct;

      procedure DrawControlText(Canvas: TCanvas; Details: TThemedElementDetails;
        const S: string; var R: TRect; Flags: Cardinal);
      var
        TextFormat: TTextFormatFlags;
      begin
        Canvas.Font := TWinControlClass(Self).Font;
        TextFormat := TTextFormatFlags(Flags);
        Canvas.Font.Color := TColorizerLocalSettings.ColorMap.FontColor;
        StyleServices.DrawText(Canvas.Handle, Details, S, R, TextFormat, Canvas.Font.Color);
      end;

      procedure Paint(Canvas : TCanvas);
      const
        AlignStyles: array [TAlignment] of Integer = (DT_LEFT, DT_RIGHT, DT_CENTER);
      var
        R : TRect;
        Res, Count, I: Integer;
        Idx, Flags: Cardinal;
        Details: TThemedElementDetails;
        LText: string;
        Borders: array [0..2] of Integer;
        SaveCanvas: TCanvas;
      begin
        if not StyleServices.Available then
          Exit;

        Details := StyleServices.GetElementDetails(tsStatusRoot);
        //StyleServices.DrawElement(Canvas.Handle, Details, Rect(0, 0, Self.Width, Self.Height));
        Canvas.Brush.Color := TColorizerLocalSettings.ColorMap.Color;
        Canvas.FillRect(Rect(0, 0, Self.Width, Self.Height));


        if SendMessage(Self.Handle, SB_ISSIMPLE, 0, 0) > 0 then
        begin
          R := Self.ClientRect;
          FillChar(Borders, SizeOf(Borders), 0);
          SendMessage(Self.Handle, SB_GETBORDERS, 0, IntPtr(@Borders));
          R.Left := Borders[0] + Borders[2];
          R.Top := Borders[1];
          R.Bottom := R.Bottom - Borders[1];
          R.Right := R.Right - Borders[2];

          Canvas.Brush.Color := TColorizerLocalSettings.ColorMap.Color;
          Canvas.FillRect(R);

          //R1 := Self.ClientRect;
          //R1.Left := R1.Right - R.Height;
          //Details := StyleServices.GetElementDetails(tsGripper);
          //StyleServices.DrawElement(Canvas.Handle, Details, R1);

          Details := StyleServices.GetElementDetails(tsPane);
          SetLength(LText, Word(SendMessage(Self.Handle, SB_GETTEXTLENGTH, 0, 0)));
          if Length(LText) > 0 then
          begin
           SendMessage(Self.Handle, SB_GETTEXT, 0, IntPtr(@LText[1]));
           Flags := Self.DrawTextBiDiModeFlags(DT_LEFT);
           DrawControlText(Canvas, Details, LText, R, Flags);
          end;
        end
        else
        begin
          Count := Self.Panels.Count;
          for I := 0 to Count - 1 do
          begin
            R := Rect(0, 0, 0, 0);
            SendMessage(Self.Handle, SB_GETRECT, I, IntPtr(@R));
            if IsRectEmpty(R) then
              Exit;

            Canvas.Brush.Color := TColorizerLocalSettings.ColorMap.HighlightColor;
            Canvas.FillRect(R);

//            if I = Count - 1 then
//            begin
//              R1 := Self.ClientRect;
//              R1.Left := R1.Right - R.Height;
//              Details := StyleServices.GetElementDetails(tsGripper);
//              StyleServices.DrawElement(Canvas.Handle, Details, R1);
//            end;
            Details := StyleServices.GetElementDetails(tsPane);
            InflateRect(R, -1, -1);
            if Self is TCustomStatusBar then
              Flags := Self.DrawTextBiDiModeFlags(AlignStyles[TCustomStatusBar(Self).Panels[I].Alignment])
            else
              Flags := Self.DrawTextBiDiModeFlags(DT_LEFT);
            Idx := I;
            SetLength(LText, Word(SendMessage(Self.Handle, SB_GETTEXTLENGTH, Idx, 0)));
            if Length(LText) > 0 then
            begin
              Res := SendMessage(Self.Handle, SB_GETTEXT, Idx, IntPtr(@LText[1]));
              if (Res and SBT_OWNERDRAW = 0) then
                DrawControlText(Canvas, Details, LText, R, Flags)
              else
              if (Self is TCustomStatusBar) and Assigned(TCustomStatusBar(Self).OnDrawPanel) then
              begin
                SaveCanvas  := Self.Canvas;
                Self.CanvasRW := Canvas;
                try
                  Self.OnDrawPanel(TCustomStatusBar(Self), Self.Panels[I], R);
                finally
                  Self.CanvasRW := SaveCanvas;
                end;
              end;
            end
            else if (Self is TCustomStatusBar) then
             if (TCustomStatusBar(Self).Panels[I].Style <> psOwnerDraw) then
               DrawControlText(Canvas, Details, TCustomStatusBar(Self).Panels[I].Text, R, Flags)
             else
               if Assigned(TCustomStatusBar(Self).OnDrawPanel) then
               begin
                 SaveCanvas := TCustomStatusBar(Self).Canvas;
                 TCustomStatusBar(Self).CanvasRW := Canvas;
                 try
                   TCustomStatusBar(Self).OnDrawPanel(TCustomStatusBar(Self), TCustomStatusBar(Self).Panels[I], R);
                 finally
                   TCustomStatusBar(Self).CanvasRW := SaveCanvas;
                 end;
               end;
          end;
        end;

      end;

begin
    if (Assigned(TColorizerLocalSettings.Settings) and not TColorizerLocalSettings.Settings.Enabled) or (csDesigning in Self.ComponentState) or (not Assigned(TColorizerLocalSettings.ColorMap)) or (Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.UseVCLStyles) then
    begin
     Trampoline_TCustomStatusBar_WMPAINT(Self, Message);
     exit;
    end;

    Self.DoUpdatePanels(False, True);

    DC := HDC(Message.DC);
    LCanvas := TCanvas.Create;
    try
        if DC <> 0 then
          LCanvas.Handle := DC
        else
        LCanvas.Handle := BeginPaint(Self.Handle, PS);
        if (DC = 0) then
        begin
          Buffer := TBitmap.Create;
          try
            Buffer.SetSize(Self.Width, Self.Height);
            LCanvas.Brush.Color := TColorizerLocalSettings.ColorMap.Color;
            LCanvas.FillRect(Self.ClientRect);
            Paint(Buffer.Canvas);
            // paint other controls
            if Self is TWinControl then
              TWinControlClass(Self).PaintControls(Buffer.Canvas.Handle, nil);
            LCanvas.Draw(0, 0, Buffer);
          finally
            Buffer.Free;
          end;
        end;

      if DC = 0 then
        EndPaint(Self.Handle, PS);
    finally
      LCanvas.Handle := 0;
      LCanvas.Free;
    end;

end;


//Hook for the docked IDE windows.
function CustomDrawDockCaption(Self : TDockCaptionDrawerClass;const Canvas: TCanvas; CaptionRect: TRect; State: TParentFormState): TDockCaptionHitTest;
var
  LColor: TColor;
  LStyle: TCustomStyleServices;
  LDetails: TThemedElementDetails;

  procedure DrawIcon;
  var
    FormBitmap: TBitmap;
    DestBitmap: TBitmap;
    ImageSize: Integer;
    X, Y: Integer;
  begin
    if (State.Icon <> nil) and (State.Icon.HandleAllocated) then
    begin
      if Self.DockCaptionOrientation = dcoHorizontal then
      begin
        ImageSize := CaptionRect.Bottom - CaptionRect.Top - 3;
        X := CaptionRect.Left;
        Y := CaptionRect.Top + 2;
      end
      else
      begin
        ImageSize := CaptionRect.Right - CaptionRect.Left - 3;
        X := CaptionRect.Left + 1;
        Y := CaptionRect.Top;
      end;

      FormBitmap := nil;
      DestBitmap := TBitmap.Create;
      try
        FormBitmap := TBitmap.Create;
        DestBitmap.Width :=  ImageSize;
        DestBitmap.Height := ImageSize;
        DestBitmap.Canvas.Brush.Color := clFuchsia;
        DestBitmap.Canvas.FillRect(Rect(0, 0, DestBitmap.Width, DestBitmap.Height));
        FormBitmap.Width := State.Icon.Width;
        FormBitmap.Height := State.Icon.Height;
        FormBitmap.Canvas.Draw(0, 0, State.Icon);
        ScaleImage(FormBitmap, DestBitmap, DestBitmap.Width / FormBitmap.Width);

        DestBitmap.TransparentColor := DestBitmap.Canvas.Pixels[0, DestBitmap.Height - 1];
        DestBitmap.Transparent := True;

        Canvas.Draw(X, Y, DestBitmap);
      finally
        FormBitmap.Free;
        DestBitmap.Free;
      end;

      if Self.DockCaptionOrientation = dcoHorizontal then
        CaptionRect.Left := CaptionRect.Left + 6 + ImageSize
      else
        CaptionRect.Top := CaptionRect.Top + 6 + ImageSize;
    end;
  end;

  function CalcButtonSize(
    const CaptionRect: TRect): Integer;
  const
    cButtonBuffer = 8;
  begin
    if Self.DockCaptionOrientation = dcoHorizontal then
      Result := CaptionRect.Bottom - CaptionRect.Top - cButtonBuffer
    else
      Result := CaptionRect.Right - CaptionRect.Left - cButtonBuffer;
  end;

  function GetCloseRect(const CaptionRect: TRect): TRect;
  const
    cSideBuffer = 4;
  var
    CloseSize: Integer;
  begin
    CloseSize := CalcButtonSize(CaptionRect);
    if Self.DockCaptionOrientation = dcoHorizontal then
    begin
      Result.Left := CaptionRect.Right - CloseSize - cSideBuffer;
      Result.Top := CaptionRect.Top + ((CaptionRect.Bottom - CaptionRect.Top) - CloseSize) div 2;
    end
    else
    begin
      Result.Left := CaptionRect.Left + ((CaptionRect.Right - CaptionRect.Left) - CloseSize) div 2;
      Result.Top := CaptionRect.Top + 2 * cSideBuffer;
    end;
    Result.Right := Result.Left + CloseSize;
    Result.Bottom := Result.Top + CloseSize;
  end;

  function GetPinRect(const CaptionRect: TRect): TRect;
  const
    cSideBuffer = 4;
  var
    PinSize: Integer;
  begin
    PinSize := CalcButtonSize(CaptionRect);
    if Self.DockCaptionOrientation = dcoHorizontal then
    begin
      Result.Left := CaptionRect.Right - 2*PinSize - 2*cSideBuffer;
      Result.Top := CaptionRect.Top + ((CaptionRect.Bottom - CaptionRect.Top) - PinSize) div 2;
    end
    else
    begin
      Result.Left := CaptionRect.Left + ((CaptionRect.Right - CaptionRect.Left) - PinSize) div 2;
      Result.Top := CaptionRect.Top + 2*cSideBuffer + 2*PinSize;
    end;
    Result.Right := Result.Left + PinSize + 2;
    Result.Bottom := Result.Top + PinSize;
  end;

const
  CHorzStates: array[Boolean] of TThemedPanel = (tpDockPanelHorzNormal, tpDockPanelHorzSelected);
  CVertStates: array[Boolean] of TThemedPanel = (tpDockPanelVertNormal, tpDockPanelVertSelected);

var
  ShouldDrawClose: Boolean;
  CloseRect, PinRect: TRect;
  LPngImage : TPngImage;
begin

  if (Assigned(TColorizerLocalSettings.Settings) and not TColorizerLocalSettings.Settings.Enabled) or (not Assigned(TColorizerLocalSettings.ColorMap)) or (Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.UseVCLStyles) then
  begin
    Result:=Trampoline_TDockCaptionDrawer_DrawDockCaption(Self, Canvas, CaptionRect, State);
    exit;
  end;

  LStyle := StyleServices;

  Canvas.Font.Color :=  TColorizerLocalSettings.ColorMap.FontColor;
  if Self.DockCaptionOrientation = dcoHorizontal then
  begin
//      LDetails:= LStyle.GetElementDetails(twSmallCaptionActive);
//      LStyle.DrawElement(Canvas.Handle, LDetails, CaptionRect);

    LDetails := LStyle.GetElementDetails(CHorzStates[State.Focused]);
    Canvas.Pen.Width := 1;
    Canvas.Pen.Color := TColorizerLocalSettings.ColorMap.FrameTopLeftInner;

    CaptionRect.Top := CaptionRect.Top + 1;

    if State.Focused then
      LColor := TColorizerLocalSettings.ColorMap.Color
    else
      LColor := TColorizerLocalSettings.ColorMap.MenuColor;

    Canvas.Brush.Color := LColor;

    //Canvas.FillRect(Rect(CaptionRect.Left + 1, CaptionRect.Top + 1, CaptionRect.Right, CaptionRect.Bottom));
    GradientFillCanvas(Canvas, LColor, TColorizerLocalSettings.ColorMap.MenuColor, Rect(CaptionRect.Left + 1, CaptionRect.Top + 1, CaptionRect.Right, CaptionRect.Bottom), gdVertical);

    Canvas.Pen.Color := GetShadowColor(Canvas.Pen.Color, -20);
    with CaptionRect do
      Canvas.Polyline([Point(Left + 2, Top),
        Point(Right - 2, Top),
        Point(Right, Top + 2),
        Point(Right, Bottom - 2),
        Point(Right - 2, Bottom),
        Point(Left + 2, Bottom),
        Point(Left, Bottom - 2),
        Point(Left, Top + 2),
        Point(Left + 3, Top)]);

    CloseRect := GetCloseRect(CaptionRect);

    if Self.DockCaptionPinButton <> dcpbNone then
    begin
      PinRect := GetPinRect(CaptionRect);

      LPngImage:=TPNGImage.Create;
      try
        if Self.DockCaptionPinButton = dcpbUp then
         LPngImage.LoadFromResourceName(HInstance, 'pin_dock_left')
        else
         LPngImage.LoadFromResourceName(HInstance, 'pin_dock');

        Canvas.Draw(PinRect.Left, PinRect.Top, LPngImage);
      finally
        LPngImage.free;
      end;

      CaptionRect.Right := PinRect.Right - 2;
    end
    else
      CaptionRect.Right := CloseRect.Right - 2;

    CaptionRect.Left := CaptionRect.Left + 6;
    DrawIcon;
    ShouldDrawClose := CloseRect.Left >= CaptionRect.Left;

  end
  else
  begin
    Canvas.MoveTo(CaptionRect.Left + 1, CaptionRect.Top + 1);
    Canvas.LineTo(CaptionRect.Right - 1, CaptionRect.Top + 1);

    LDetails := LStyle.GetElementDetails(CVertStates[State.Focused]);

    if State.Focused then
      LColor := TColorizerLocalSettings.ColorMap.Color
    else
      LColor := TColorizerLocalSettings.ColorMap.MenuColor;

    Canvas.Brush.Color := LColor;

    Canvas.FillRect(Rect(CaptionRect.Left, CaptionRect.Top + 2,
      CaptionRect.Right, CaptionRect.Bottom));

    Canvas.Pen.Color := State.EndColor;
    Canvas.MoveTo(CaptionRect.Left + 1, CaptionRect.Bottom);
    Canvas.LineTo(CaptionRect.Right - 1, CaptionRect.Bottom);

    Canvas.Font.Orientation := 900;
    CloseRect := GetCloseRect(CaptionRect);

    if Self.DockCaptionPinButton <> dcpbNone then
    begin
      PinRect := GetPinRect(CaptionRect);
      LPngImage:=TPNGImage.Create;
      try
        if Self.DockCaptionPinButton = dcpbUp then
         LPngImage.LoadFromResourceName(HInstance, 'pin_dock_left')
        else
         LPngImage.LoadFromResourceName(HInstance, 'pin_dock');

        Canvas.Draw(PinRect.Left, PinRect.Top, LPngImage);
      finally
        LPngImage.free;
      end;
      CaptionRect.Top := PinRect.Bottom + 2;
    end
    else
      CaptionRect.Top := CloseRect.Bottom + 2;

    ShouldDrawClose   := CaptionRect.Top < CaptionRect.Bottom;
    CaptionRect.Right := CaptionRect.Left + (CaptionRect.Bottom - CaptionRect.Top - 2);
    CaptionRect.Top   := CaptionRect.Top + Canvas.TextWidth(State.Caption) + 2;

    if CaptionRect.Top > CaptionRect.Bottom then
      CaptionRect.Top := CaptionRect.Bottom;
  end;

  Canvas.Brush.Style := bsClear;
  if State.Caption <> '' then
  begin
    if State.Focused then
      Canvas.Font.Style := Canvas.Font.Style + [fsBold]
    else
      Canvas.Font.Style := Canvas.Font.Style - [fsBold];

   if ShouldDrawClose then
     CaptionRect.Right := CaptionRect.Right - (CloseRect.Right - CloseRect.Left) - 4;

    Canvas.TextRect(CaptionRect, State.Caption,
      [tfEndEllipsis, tfVerticalCenter, tfSingleLine]);
  end;

  if ShouldDrawClose then
  begin
    LPngImage:=TPNGImage.Create;
    try
      LPngImage.LoadFromResourceName(HInstance, 'close_dock');
      Canvas.Draw(CloseRect.Left, CloseRect.Top, LPngImage);
    finally
      LPngImage.free;
    end;
  end;
end;

//Hook for the TCustomListView component
procedure CustomHeaderWndProc(Self:TCustomListView;var Message: TMessage);

    procedure DrawControlText(Canvas: TCanvas; Details: TThemedElementDetails;
      const S: string; var R: TRect; Flags: Cardinal);
    var
      TextFormat: TTextFormatFlags;
    begin
      Canvas.Font := TWinControlClass(Self).Font;
      TextFormat := TTextFormatFlags(Flags);
      Canvas.Font.Color := TColorizerLocalSettings.ColorMap.FontColor;
      StyleServices.DrawText(Canvas.Handle, Details, S, R, TextFormat, Canvas.Font.Color);
    end;

    procedure DrawHeaderSection(Canvas: TCanvas; R: TRect; Index: Integer;
      const Text: string; IsPressed, IsBackground: Boolean);
    var
      Item: THDItem;
      ImageList: HIMAGELIST;
      DrawState: TThemedHeader;
      IconWidth, IconHeight: Integer;
      LDetails: TThemedElementDetails;
      LBuffer : TBitmap;
    begin
      FillChar(Item, SizeOf(Item), 0);
      Item.Mask := HDI_FORMAT;
      Header_GetItem(Self.Handle, Index, Item);

      LBuffer:=TBitmap.Create;
      try
       LBuffer.SetSize(R.Width, R.Height);
       LBuffer.Canvas.Pen.Color:=TColorizerLocalSettings.ColorMap.FrameTopLeftInner;
       LBuffer.Canvas.Rectangle(Rect(0, 0, R.Right, R.Bottom));
       GradientFillCanvas(LBuffer.Canvas, TColorizerLocalSettings.ColorMap.Color, TColorizerLocalSettings.ColorMap.HighlightColor, Rect(1, 1, R.Right-1, R.Bottom-1), gdVertical);
       Canvas.Draw(R.Left, R.Top, LBuffer);
      finally
       LBuffer.Free;
      end;

      ImageList := SendMessage(Self.Handle, HDM_GETIMAGELIST, 0, 0);
      Item.Mask := HDI_FORMAT or HDI_IMAGE;
      InflateRect(R, -2, -2);
      if (ImageList <> 0) and Header_GetItem(Self.Handle, Index, Item) then
      begin
        if Item.fmt and HDF_IMAGE = HDF_IMAGE then
          ImageList_Draw(ImageList, Item.iImage, Canvas.Handle, R.Left, R.Top, ILD_TRANSPARENT);
        ImageList_GetIconSize(ImageList, IconWidth, IconHeight);
        Inc(R.Left, IconWidth + 5);
      end;

      if IsBackground then
        DrawState := thHeaderItemNormal
      else
      if IsPressed then
        DrawState := thHeaderItemPressed
      else
        DrawState := thHeaderItemNormal;

      LDetails := StyleServices.GetElementDetails(DrawState);
      DrawControlText(Canvas, LDetails, Text, R, DT_VCENTER or DT_LEFT or  DT_SINGLELINE or DT_END_ELLIPSIS);
    end;


var
  Canvas: TCanvas;
  R, HeaderR: TRect;
  PS: TPaintStruct;
  HeaderDC: HDC;
  I, ColumnIndex, RightOffset: Integer;
  SectionOrder: array of Integer;
  Item: THDItem;
  Buffer: array [0..255] of Char;
  LParentForm : TCustomForm;
begin

  if Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled and  (Message.Msg=WM_PAINT) then
  if not (csDesigning in Self.ComponentState) then
  begin
    LParentForm:= GetParentForm(Self);
    if Assigned(LParentForm) and Assigned(TColorizerLocalSettings.HookedWindows) and (TColorizerLocalSettings.HookedWindows.IndexOf(LParentForm.ClassName)>=0) then
    begin
        HeaderDC := BeginPaint(Self.GetHeaderHandle, PS);
      try
        Canvas := TCanvas.Create;
        try
          Canvas.Handle := HeaderDC;
          RightOffset := 0;

          for I := 0 to Header_GetItemCount(Self.GetHeaderHandle) - 1 do
          begin
            SetLength(SectionOrder, Header_GetItemCount(Self.GetHeaderHandle));
            Header_GetOrderArray(Self.GetHeaderHandle, Header_GetItemCount(Self.GetHeaderHandle),
              Pointer(SectionOrder));
            ColumnIndex := SectionOrder[I];
            Header_GETITEMRECT(Self.GetHeaderHandle, ColumnIndex, @R);
            FillChar(Item, SizeOf(Item), 0);
            Item.Mask := HDI_TEXT;
            Item.pszText := @Buffer;
            Item.cchTextMax := Length(Buffer);
            Header_GetItem(Self.GetHeaderHandle, ColumnIndex, Item);
            DrawHeaderSection(Canvas, R, ColumnIndex, Item.pszText,
              {FPressedSection = ColumnIndex} False, False);

            if RightOffset < R.Right then
              RightOffset := R.Right;
          end;

          GetWindowRect(Self.GetHeaderHandle, HeaderR);
          R := Rect(RightOffset, 0, HeaderR.Width + 2, HeaderR.Height);
          if not IsRectEmpty(R) then
            DrawHeaderSection(Canvas, R, -1, '', False, True);

        finally
          Canvas.Handle := 0;
          Canvas.Free;
        end;
      finally
          EndPaint(Self.GetHeaderHandle, PS)
      end;
      exit;
    end;
  end;
  Trampoline_TCustomListView_HeaderWndProc(Self, Message);
end;

procedure CustomProjectTree2PaintText(Self : TObject; Sender: TObject{TBaseVirtualTree}; const TargetCanvas: TCanvas; Node: {PVirtualNode}Pointer; Column: Integer{TColumnIndex}; TextType: Byte {TVSTTextType});
begin
  //TargetCanvas.Font.Color:=clRed;
  Trampoline_ProjectTree2PaintText(Self, Sender, TargetCanvas, Node, Column, TextType);
end;

//Hook for allow change font color in TProjectManagerForm.TVirtualStringTree ,
//because this component is not using the colors set via RTTI
//Note  : This is a temporal workaround.
function CustomDrawText(hDC: HDC; lpString: LPCWSTR; nCount: Integer;  var lpRect: TRect; uFormat: UINT): Integer; stdcall;
var
  sCaller : string;
begin
 if Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled and Assigned(TColorizerLocalSettings.ColorMap) then
 begin
  if GetTextColor(hDC) = GetSysColor(COLOR_WINDOWTEXT) then
  begin
    sCaller := ProcByLevel(2);
    if SameText(sCaller, 'IDEVirtualTrees.TCustomVirtualStringTree.PaintNormalText') then
      SetTextColor(hDC, TColorizerLocalSettings.ColorMap.FontColor);
  end;
 end;

  Result:=Trampoline_DrawText(hDC, lpString, nCount, lpRect, uFormat);
end;

//Hook to fix artifacts and undocumented painting methods ex: TClosableTabScroller background
function CustomGetSysColor(nIndex: Integer): DWORD; stdcall;
var
  sCaller : string;
//  i  : Integer;
begin
   if  Assigned(TColorizerLocalSettings.Settings) and (TColorizerLocalSettings.Settings.Enabled) and Assigned(TColorizerLocalSettings.ColorMap) and  (nIndex=COLOR_BTNFACE) then
   begin
    //Vcl.Controls.TWinControl.PaintHandler
    //Vcl.Controls.TWinControl.WMPaint
    //Vcl.Controls.TWinControl.WMPrintClient
    //
    //       ok  x2,x4,x6
     sCaller := ProcByLevel(2);
     if SameText(sCaller, '') then
     begin
       Result:=TColorizerLocalSettings.ColorMap.Color;
       exit;
     end;

//    for i := 2 to 30 do
//    begin
//       sCaller := ProcByLevel(i);
//       TFile.AppendAllText('C:\Delphi\google-code\DITE\delphi-ide-theme-editor\IDE PlugIn\CustomGetSysColor.txt', Format('%d %s %s',[i, sCaller, SLineBreak]));
//    end;
//       TFile.AppendAllText('C:\Delphi\google-code\DITE\delphi-ide-theme-editor\IDE PlugIn\CustomGetSysColor.txt', Format('%s %s',['---------------', SLineBreak]));

   end;

   Result:= Trampoline_GetSysColor(nIndex);
end;


const
// sEVFillGutter ='@Editorcontrol@TCustomEditControl@EVFillGutter$qqrr';
  sProjectTree2PaintText ='@Projectfrm@TProjectManagerForm@ProjectTree2PaintText$qqrp32Idevirtualtrees@TBaseVirtualTreexp20Vcl@Graphics@TCanvasp28Idevirtualtrees@TVirtualNodei28Idevirtualtrees@TVSTTextType';

procedure InstallColorizerHooks;
var
  GetSysColorOrgPointer : Pointer;
begin
  TrampolineCustomImageList_DoDraw:=InterceptCreate(@TCustomImageListClass.DoDraw, @CustomImageListHack_DoDraw);
  Trampoline_TCanvas_FillRect     :=InterceptCreate(@TCanvas.FillRect, @CustomFillRect);
  Trampoline_TStyleEngine_HandleMessage := InterceptCreate(@TStyleEngine.HandleMessage,   @CustomHandleMessage);
  Trampoline_TCustomStatusBar_WMPAINT   := InterceptCreate(TCustomStatusBarClass(nil).WMPaintAddress,   @CustomStatusBarWMPaint);
  Trampoline_TDockCaptionDrawer_DrawDockCaption  := InterceptCreate(@TDockCaptionDrawer.DrawDockCaption,   @CustomDrawDockCaption);
  Trampoline_TUxThemeStyle_DoDrawElement    := InterceptCreate(@TUxThemeStyleClass.DoDrawElement,   @CustomDrawElement);
  Trampoline_TCustomListView_HeaderWndProc  := InterceptCreate(TCustomListViewClass(nil).HeaderWndProcAddress, @CustomHeaderWndProc);
  Trampoline_DrawText                       := InterceptCreate(@Windows.DrawTextW, @CustomDrawText);

   GetSysColorOrgPointer     := GetProcAddress(GetModuleHandle('user32.dll'), 'GetSysColor');
   if Assigned(GetSysColorOrgPointer) then
     Trampoline_GetSysColor    :=  InterceptCreate(GetSysColorOrgPointer, @CustomGetSysColor);
//  CorIdeModule := LoadLibrary('coreide180.bpl');
//  if CorIdeModule<>0 then
//  begin
//   pProjectTree2PaintText := GetBplMethodAddress(GetProcAddress(CorIdeModule, sProjectTree2PaintText));
//   if Assigned(pProjectTree2PaintText) then
//    Trampoline_ProjectTree2PaintText:= InterceptCreate(pProjectTree2PaintText, @CustomProjectTree2PaintText);
//  end;
end;

procedure RemoveColorizerHooks;
begin
  if Assigned(TrampolineCustomImageList_DoDraw) then
    InterceptRemove(@TrampolineCustomImageList_DoDraw);
  if Assigned(Trampoline_TCanvas_FillRect) then
    InterceptRemove(@Trampoline_TCanvas_FillRect);
  if Assigned(Trampoline_TStyleEngine_HandleMessage) then
    InterceptRemove(@Trampoline_TStyleEngine_HandleMessage);
  if Assigned(Trampoline_TCustomStatusBar_WMPAINT) then
    InterceptRemove(@Trampoline_TCustomStatusBar_WMPAINT);
  if Assigned(Trampoline_TDockCaptionDrawer_DrawDockCaption) then
    InterceptRemove(@Trampoline_TDockCaptionDrawer_DrawDockCaption);
  if Assigned(Trampoline_TUxThemeStyle_DoDrawElement) then
    InterceptRemove(@Trampoline_TUxThemeStyle_DoDrawElement);
  if Assigned(Trampoline_TCustomListView_HeaderWndProc) then
    InterceptRemove(@Trampoline_TCustomListView_HeaderWndProc);
  if Assigned(Trampoline_ProjectTree2PaintText) then
    InterceptRemove(@Trampoline_ProjectTree2PaintText);
  if Assigned(Trampoline_DrawText) then
    InterceptRemove(@Trampoline_DrawText);
  if Assigned(Trampoline_GetSysColor) then
    InterceptRemove(@Trampoline_GetSysColor);
end;

{
    004228A4 17391 1F38 __fastcall Editcolorpage::Finalization()
    00420B94 17411 1F39 Editcolorpage::TEditorColor::
    00422188 17400 1F3A __fastcall Editcolorpage::TEditorColor::ColorClick(System::TObject *)
    0042174C 17407 1F3B __fastcall Editcolorpage::TEditorColor::ColorSpeedSettingClick(System::TObject *)
    004224BC 17396 1F3C __fastcall Editcolorpage::TEditorColor::DefaultClick(System::TObject *)
    00422414 17397 1F3D __fastcall Editcolorpage::TEditorColor::EditorColorBroadcast(System::TObject *, Winapi::Messages::TMessage&)
    00421584 17409 1F3E __fastcall Editcolorpage::TEditorColor::EditorColorCreate(System::TObject *)
    00421730 17408 1F3F __fastcall Editcolorpage::TEditorColor::EditorColorDestroy(System::TObject *)
    004217B0 17406 1F40 __fastcall Editcolorpage::TEditorColor::ElementListClick(System::TObject *)
    004222E8 17399 1F41 __fastcall Editcolorpage::TEditorColor::FontClick(System::TObject *)
    004225DC 17395 1F42 __fastcall Editcolorpage::TEditorColor::HelpClick(System::TObject *)
    00421AE8 17404 1F43 __fastcall Editcolorpage::TEditorColor::InitLineFlags(const System::DelphiInterface<Toolsapi::IOTAHighlighterPreview>)
    004219B8 17405 1F44 __fastcall Editcolorpage::TEditorColor::InitSamplePane()
    00421BC8 17403 1F45 __fastcall Editcolorpage::TEditorColor::InitSyntaxEditView(const System::DelphiInterface<Toolsapi::IOTAHighlighterPreview>)
    0042262C 17393 1F46 __fastcall Editcolorpage::TEditorColor::LoadHighlightPreviews()
    004225F4 17394 1F47 __fastcall Editcolorpage::TEditorColor::MarkDirty()
    004220E4 17401 1F48 __fastcall Editcolorpage::TEditorColor::SampleClick(System::TObject *)
    00422080 17402 1F49 __fastcall Editcolorpage::TEditorColor::SetColorSpeedSetting(Vedopts::TColorSpeedSetting)
    0042238C 17398 1F4A __fastcall Editcolorpage::TEditorColor::UpdateSamplePane()
    00422814 17392 1F4B __fastcall Editcolorpage::TEditorColor::tbsetPreviewsChange(System::TObject *, int, bool&)
    004AA8D4 17390 1F4C __fastcall Editcolorpage::initialization()

    002F0A00 11656 219E __fastcall Editorcontrol::TCustomEditControl::EVFillGutter(System::Types::TRect&, unsigned short, int, bool, int)
}

//initialization
//  InstallColorizerHooks;
//
//finalization
//  RemoveColorizerHooks;
end.


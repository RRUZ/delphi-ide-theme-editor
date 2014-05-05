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
  Windows,
  Classes,
  uDelphiVersions,
  uDelphiIDEHighlight,
  SysUtils,
  ComCtrls,
  Graphics,
  ImgList,
  CommCtrl,
  JclDebug,
  PngImage,
  Colorizer.Utils,
  CaptionedDockTree,
  GraphUtil,
  DDetours;



implementation

type
 TWinControlClass        = class(TWinControl);
 TCustomPanelClass       = class(TCustomPanel);
 TCustomStatusBarClass   = class(TCustomStatusBar);
 TDockCaptionDrawerClass = class(TDockCaptionDrawer);
 TUxThemeStyleClass      = class(TUxThemeStyle);
var
  TrampolineCustomImageList_DoDraw     : procedure(Self: TObject; Index: Integer; Canvas: TCanvas; X, Y: Integer; Style: Cardinal; Enabled: Boolean) = nil;
  Trampoline_TCanvas_FillRect          : procedure(Self: TCanvas;const Rect: TRect) = nil;
  //Trampoline_TCanvas_PolyLine          : procedure(Self: TCanvas;const Points: array of TPoint) = nil;
  //Trampoline_TCanvas_LineTo            : procedure(Self: TCanvas;X, Y: Integer) = nil;
  //Trampoline_TCanvas_Polygon             : procedure(Self: TCanvas;const Points: array of TPoint) = nil;
  //Trampoline_TCanvas_Draw             :  procedure(Self: TCanvas;X, Y: Integer; Graphic: TGraphic) = nil;
  Trampoline_TStyleEngine_HandleMessage: function(Self: TStyleEngine; Control: TWinControl; var Message: TMessage; DefWndProc: TWndMethod): Boolean = nil;
  Trampoline_TCustomStatusBar_WMPAINT  : procedure(Self: TCustomStatusBarClass; var Message: TWMPaint) = nil;
  Trampoline_TDockCaptionDrawer_DrawDockCaption : function (Self : TDockCaptionDrawerClass;const Canvas: TCanvas; CaptionRect: TRect; State: TParentFormState): TDockCaptionHitTest =nil;
  Trampoline_TUxThemeStyle_DoDrawElement  : function (Self : TUxThemeStyle;DC: HDC; Details: TThemedElementDetails; const R: TRect; ClipRect: PRect = nil): Boolean = nil;
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

 //@Editorcontrol@TCustomEditControl@EVFillGutter$qqrrx18
 //002F0A00 11656 219E __fastcall Editorcontrol::TCustomEditControl::EVFillGutter(System::Types::TRect&, unsigned short, int, bool, int)
//  Trampoline_EVFillGutter              : procedure(Self: TObject;ARect:TRect; p1 : USHORT; p2 : Integer; p3 :Boolean; p4 : Integer);
//  Addr_EVFillGutter                    : Pointer;
//Const
  //FooMethod='@Editcolorpage@TEditorColor@SetColorSpeedSetting$qqr26Vedopts@TColorSpeedSetting';
  //FooMethod='@Editcolorpage@TEditorColor@ColorSpeedSettingClick$qqrp14System@TObject';


//function GetBplMethodAddress(Method: Pointer): Pointer;
//type
//  PJmpCode = ^TJmpCode;
//  TJmpCode = packed record
//    Code: Word;
//    Addr: ^Pointer;
//  end;
//const
//  csJmp32Code = $25FF;
//begin
//  if PJmpCode(Method)^.Code = csJmp32Code then
//    Result := PJmpCode(Method)^.Addr^
//  else
//    Result := Method;
//end;

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
begin
  with TCustomImageListClass(Self) do
  begin
    if not HandleAllocated then Exit;
    if Enabled then
      ImageList_DrawEx(Handle, Index, Canvas.Handle, X, Y, 0, 0, GetRGBColor(BkColor), GetRGBColor(BlendColor), Style)
    else
    begin
      GrayBitMap := TBitmap.Create;
      MaskBitMap := TBitmap.Create;
      try
        GrayBitMap.SetSize(Width, Height);
        MaskBitMap.SetSize(Width, Height);
        GetImages(Index, GrayBitMap, MaskBitMap);
        Bitmap2GrayScale(GrayBitMap);
        BitBlt(Canvas.Handle, X, Y, Width, Height, MaskBitMap.Canvas.Handle, 0, 0, SRCERASE);
        BitBlt(Canvas.Handle, X, Y, Width, Height, GrayBitMap.Canvas.Handle, 0, 0, SRCINVERT);
      finally
        GrayBitMap.Free;
        MaskBitMap.Free;
      end;
    end;
  end;
end;

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


procedure  CustomFillRect(Self: TCanvas;const Rect: TRect);
var
  sCaller : string;
begin
   if Assigned(TColorizerLocalSettings.ColorMap) and  (Self.Brush.Color=clBtnFace) then
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

//procedure CustomPolyLine(Self: TCanvas;const Points: array of TPoint);
//var
//  sCaller : string;
//begin
//   sCaller := ProcByLevel(1);
//   TFile.AppendAllText('C:\Delphi\google-code\DITE\delphi-ide-theme-editor\IDE PlugIn\PolyLine.txt', Format('%s %s',[sCaller, SLineBreak]));
//   Trampoline_TCanvas_PolyLine(Self, Points);
//end;

//procedure CustomPolygon(Self: TCanvas;const Points: array of TPoint);
//var
//  sCaller : string;
//begin
//   sCaller := ProcByLevel(2);
//   TFile.AppendAllText('C:\Delphi\google-code\DITE\delphi-ide-theme-editor\IDE PlugIn\CustomPolygon.txt', Format('%s %s',[sCaller, SLineBreak]));
//   Trampoline_TCanvas_Polygon(Self, Points);
//end;

//procedure CustomLineTo(Self: TCanvas;X, Y: Integer);
//var
//  sCaller : string;
//begin
//   //if Assigned(TColorizerLocalSettings.ColorMap) and  (Self.Pen.Color=clBtnFace) then
//   begin
//    sCaller := ProcByLevel(1);
//    if sCaller<>'' then
//     TFile.AppendAllText('C:\Delphi\google-code\DITE\delphi-ide-theme-editor\IDE PlugIn\LineTo.txt', Format('%s %s %s',[sCaller, IntToHex(Self.Pen.Color, 8), SLineBreak]));
//   end;
//    Trampoline_TCanvas_LineTo(Self, X, Y);
//end;

//procedure CustomDraw(Self: TCanvas;X, Y: Integer; Graphic: TGraphic);
//var
//  sCaller : string;
//begin
//
//    sCaller := ProcByLevel(2);
//    if sCaller<>'' then
//     TFile.AppendAllText('C:\Delphi\google-code\DITE\delphi-ide-theme-editor\IDE PlugIn\TCanvas_Draw.txt', Format('%s %s',[sCaller, SLineBreak]));
//
//  Trampoline_TCanvas_Draw(Self, X, Y, Graphic);
//end;

function CustomDrawElement(Self : TUxThemeStyle;DC: HDC; Details: TThemedElementDetails; const R: TRect; ClipRect: PRect = nil): Boolean;
const
  HP_HEADERITEMRIGHT = 3;
var
  sCaller : string;
  LCanvas : TCanvas;
  SaveIndex: Integer;
begin
   if Assigned(TColorizerLocalSettings.ColorMap) and (Details.Element = teHeader) {and (Details.Part=HP_HEADERITEMRIGHT) } then
   begin
    sCaller := ProcByLevel(2);
    if SameText(sCaller, 'IDEVirtualTrees.TVirtualTreeColumns.PaintHeader') then
    begin
       SaveIndex := SaveDC(DC);
       LCanvas:=TCanvas.Create;
       try
         LCanvas.Handle:=DC;
         LCanvas.Brush.Color:=TColorizerLocalSettings.ColorMap.Color;
         LCanvas.FillRect(R);
         LCanvas.Pen.Color:=TColorizerLocalSettings.ColorMap.FrameTopLeftInner;//TColorizerLocalSettings.ColorMap.MenuColor;
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

function CustomHandleMessage(Self: TStyleEngine; Control: TWinControl; var Message: TMessage; DefWndProc: TWndMethod): Boolean;
begin
  if Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.UseVCLStyles then
  begin
    Result:=False;
    if not Assigned(Control) then exit;
    if csDesigning in Control.ComponentState then  exit;
  end;
  Result:=Trampoline_TStyleEngine_HandleMessage(Self, Control, Message, DefWndProc);
end;

procedure CustomStatusBarWMPaint(Self: TCustomStatusBarClass; var Message: TWMPaint);
var
  DC: HDC;
  Buffer: TBitmap;
  LCanvas: TCanvas;
  PS: TPaintStruct;

      procedure DrawControlText(Canvas: TCanvas; Details: TThemedElementDetails;
        const S: string; var R: TRect; Flags: Cardinal);
      var
        ThemeTextColor: TColor;
        TextFormat: TTextFormatFlags;
      begin
        Canvas.Font := TWinControlClass(Self).Font;
        TextFormat := TTextFormatFlags(Flags);
        if StyleServices.GetElementColor(Details, ecTextColor, ThemeTextColor) then
        begin
          if not Self.Enabled or (seFont in Self.StyleElements) then
            Canvas.Font.Color := ThemeTextColor;
          StyleServices.DrawText(Canvas.Handle, Details, S, R, TextFormat, Canvas.Font.Color);
        end
        else
        begin
          Canvas.Refresh;
          StyleServices.DrawText(Canvas.Handle, Details, S, R, TextFormat);
        end;
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
    if (not Assigned(TColorizerLocalSettings.ColorMap)) or (Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.UseVCLStyles) then
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

  if  (not Assigned(TColorizerLocalSettings.ColorMap)) or (Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.UseVCLStyles) then
  begin
    Result:=Trampoline_TDockCaptionDrawer_DrawDockCaption(Self, Canvas, CaptionRect, State);
    exit;
  end;

  LStyle := StyleServices;
  LDetails := LStyle.GetElementDetails(CHorzStates[State.Focused]);

  Canvas.Font.Color :=  TColorizerLocalSettings.ColorMap.FontColor;
  if Self.DockCaptionOrientation = dcoHorizontal then
  begin

    Canvas.Pen.Width := 1;
    Canvas.Pen.Color := TColorizerLocalSettings.ColorMap.FrameTopLeftInner;

    CaptionRect.Top := CaptionRect.Top + 1;

    if State.Focused then
      LColor := TColorizerLocalSettings.ColorMap.Color
    else
      LColor := TColorizerLocalSettings.ColorMap.MenuColor;

    Canvas.Brush.Color := LColor;

    Canvas.FillRect(Rect(CaptionRect.Left + 1, CaptionRect.Top + 1, CaptionRect.Right, CaptionRect.Bottom));

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

//const
// sEVFillGutter ='@Editorcontrol@TCustomEditControl@EVFillGutter$qqrr';
procedure InstallColorizerHooks;
begin
  TrampolineCustomImageList_DoDraw:=InterceptCreate(@TCustomImageListClass.DoDraw, @CustomImageListHack_DoDraw);
  Trampoline_TCanvas_FillRect     :=InterceptCreate(@TCanvas.FillRect, @CustomFillRect);
  //Trampoline_TCanvas_PolyLine     :=InterceptCreate(@TCanvas.PolyLine, @CustomPolyLine);
  //Trampoline_TCanvas_Polygon      :=InterceptCreate(@TCanvas.Polygon, @CustomPolygon);
  //Trampoline_TCanvas_Draw           :=InterceptCreate(@TCanvas.Draw, @CustomDraw);
  //Trampoline_TCanvas_LineTo       :=InterceptCreate(@TCanvas.LineTo, @CustomLineTo);
  Trampoline_TStyleEngine_HandleMessage := InterceptCreate(@TStyleEngine.HandleMessage,   @CustomHandleMessage);
  Trampoline_TCustomStatusBar_WMPAINT   := InterceptCreate(TCustomStatusBarClass(nil).WMPaintAddress,   @CustomStatusBarWMPaint);
  Trampoline_TDockCaptionDrawer_DrawDockCaption  := InterceptCreate(@TDockCaptionDrawer.DrawDockCaption,   @CustomDrawDockCaption);
  Trampoline_TUxThemeStyle_DoDrawElement    := InterceptCreate(@TUxThemeStyleClass.DoDrawElement,   @CustomDrawElement);
end;

procedure RemoveColorizerHooks;
begin
  if Assigned(TrampolineCustomImageList_DoDraw) then
    InterceptRemove(@TrampolineCustomImageList_DoDraw);
  if Assigned(Trampoline_TCanvas_FillRect) then
    InterceptRemove(@Trampoline_TCanvas_FillRect);
//  if Assigned(Trampoline_TCanvas_PolyLine) then
//    InterceptRemove(@Trampoline_TCanvas_PolyLine);
//  if Assigned(Trampoline_TCanvas_Polygon) then
//    InterceptRemove(@Trampoline_TCanvas_Polygon);
//  if Assigned(Trampoline_TCanvas_LineTo) then
//    InterceptRemove(@Trampoline_TCanvas_LineTo);
//  if Assigned(Trampoline_TCanvas_Draw) then
//    InterceptRemove(@Trampoline_TCanvas_Draw);
  if Assigned(Trampoline_TStyleEngine_HandleMessage) then
    InterceptRemove(@Trampoline_TStyleEngine_HandleMessage);
  if Assigned(Trampoline_TCustomStatusBar_WMPAINT) then
    InterceptRemove(@Trampoline_TCustomStatusBar_WMPAINT);
  if Assigned(Trampoline_TDockCaptionDrawer_DrawDockCaption) then
    InterceptRemove(@Trampoline_TDockCaptionDrawer_DrawDockCaption);
  if Assigned(Trampoline_TUxThemeStyle_DoDrawElement) then
    InterceptRemove(@Trampoline_TUxThemeStyle_DoDrawElement);
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

initialization
  InstallColorizerHooks;

finalization
  RemoveColorizerHooks;
end.


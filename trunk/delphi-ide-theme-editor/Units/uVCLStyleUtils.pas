{**************************************************************************************************}
{                                                                                                  }
{ Unit uVCLStyleUtils                                                                              }
{ unit for the VCL Styles Utils                                                                    }
{ http://code.google.com/p/vcl-styles-utils/                                                       }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is uVCLStyleUtils.pas.                                                         }
{                                                                                                  }
{ The Initial Developer of the Original Code is Rodrigo Ruz V.                                     }
{ Portions created by Rodrigo Ruz V. are Copyright (C) 2012 Rodrigo Ruz V.                         }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}
unit uVCLStyleUtils;

interface

{$DEFINE USE_VCL_STYLESAPI}

Uses
  Vcl.Themes,
  Vcl.Styles,
  Vcl.Forms,
  Generics.Collections,
  Winapi.Windows,
  Vcl.Graphics,
  Classes;

type
  TStyleHookList = TList<TStyleHookClass>;

  TStyleServicesHandle = type Pointer;
  TSourceInfo = record
    Data: TStyleServicesHandle;
    StyleClass: TCustomStyleServicesClass;
  end;

  TStyleManagerHelper = Class Helper for TStyleManager
  strict private
    class function GetStyleSourceInfo(const StyleName: string): TSourceInfo; static;
  public
   class function GetRegisteredStyles: TDictionary<string, TSourceInfo>;
   class property StyleSourceInfo[const StyleName: string]: TSourceInfo read GetStyleSourceInfo;
  end;


Procedure ApplyEmptyVCLStyleHook(ControlClass :TClass);
Procedure RemoveEmptyVCLStyleHook(ControlClass :TClass);
function  IsStyleHookRegistered(ControlClass: TClass; StyleHookClass: TStyleHookClass) : Boolean;
function  GetRegisteredStylesHooks(ControlClass: TClass) : TStyleHookList;
procedure DrawSampleWindow(Style:TCustomStyle;Canvas:TCanvas;ARect:TRect;const ACaption : string);


{$IFDEF USE_VCL_STYLESAPI}
type
  TCustomStyleExt = class(TCustomStyle)
  strict private
    FStream    : TStream;
  public
    function  GetStyleInfo : TStyleInfo;
  public
    constructor Create(const FileName :string);overload;
    constructor Create(const Stream:TStream);overload;
    destructor Destroy;override;
    property StyleInfo : TStyleInfo read GetStyleInfo;
  end;

procedure ReadVCLStyleInfo(const StyleFileName:string;var StyleInfo : TStyleInfo;Bitmap:TBitmap);overload;
function  GetVCLStyleImage(const StyleFileName:string):TBitmap;
{$ENDIF}

implementation


uses
{$IFDEF USE_VCL_STYLESAPI}
 System.ZLib,
 System.UITypes,
 Vcl.Controls,
 Vcl.StdCtrls,
 Vcl.ImgList,
 Vcl.Consts,
 Vcl.GraphUtil,
 Vcl.Imaging.pngimage,
 Messages,
{$ENDIF}
 Sysutils;

{$IFDEF USE_VCL_STYLESAPI}
{$I 'C:\Program Files (x86)\Embarcadero\RAD Studio\9.0\source\vcl\StyleUtils.inc'}
{$I 'C:\Program Files (x86)\Embarcadero\RAD Studio\9.0\source\vcl\StyleAPI.inc'}
{$ENDIF}


type
  TCustomStyleHelper = Class Helper for TCustomStyle
  private
    function GetSource: TObject;
  public
    property Source: TObject read GetSource;
  End;

  TStyleHookDictionary = TDictionary<TClass, TStyleHookList>;
  TCustomStyleEngineHelper = Class Helper for TCustomStyleEngine
  public
    class function GetRegisteredStyleHooks : TStyleHookDictionary;
  END;



class function TCustomStyleEngineHelper.GetRegisteredStyleHooks: TStyleHookDictionary;
begin
  Result:= Self.FRegisteredStyleHooks;
end;

function TCustomStyleHelper.GetSource: TObject;
begin
  Result:=Self.FSource;
end;

{ TStyleManagerHelper }



class function TStyleManagerHelper.GetRegisteredStyles: TDictionary<string, TSourceInfo>;
var
  t            : TPair<string, TStyleManager.TSourceInfo>;
  SourceInfo   : TSourceInfo;
begin
 Result:=TDictionary<string, TSourceInfo>.Create;
  for t in Self.FRegisteredStyles do
  begin
   SourceInfo.Data:=t.Value.Data;
   SourceInfo.StyleClass:=t.Value.StyleClass;
   Result.Add(t.Key,SourceInfo);
  end;
end;


class function TStyleManagerHelper.GetStyleSourceInfo(const StyleName: string): TSourceInfo;
Var
 LRegisteredStyles : TDictionary<string, TSourceInfo>;
begin
  LRegisteredStyles:=TStyleManager.GetRegisteredStyles;
  try
    if LRegisteredStyles.ContainsKey(StyleName) then
      Result:=LRegisteredStyles[StyleName];
  finally
     LRegisteredStyles.Free;
  end;
end;

function  GetRegisteredStylesHooks(ControlClass: TClass) : TStyleHookList;
begin
 Result:=nil;
    if TCustomStyleEngine.GetRegisteredStyleHooks.ContainsKey(ControlClass) then
      Result:=TCustomStyleEngine.GetRegisteredStyleHooks[ControlClass];
end;

function  IsStyleHookRegistered(ControlClass: TClass; StyleHookClass: TStyleHookClass) : Boolean;
var
  List    : TStyleHookList;
begin
 Result:=False;
    if TCustomStyleEngine.GetRegisteredStyleHooks.ContainsKey(ControlClass) then
    begin
      List := TCustomStyleEngine.GetRegisteredStyleHooks[ControlClass];
      Result:=List.IndexOf(StyleHookClass) <> -1;
    end;
end;

Procedure ApplyEmptyVCLStyleHook(ControlClass :TClass);
begin
   if not IsStyleHookRegistered(ControlClass, TStyleHook) then
    TStyleManager.Engine.RegisterStyleHook(ControlClass, TStyleHook);
end;

Procedure RemoveEmptyVCLStyleHook(ControlClass :TClass);
begin
   if IsStyleHookRegistered(ControlClass, TStyleHook) then
    TStyleManager.Engine.UnRegisterStyleHook(ControlClass, TStyleHook);
end;


{$IFDEF USE_VCL_STYLESAPI}

function  GetVCLStyleImage(const StyleFileName:string):TBitmap;
var
  FileStream : TFileStream;
  StyleSource: TSeStyleSource;
  StyleFilter : TSeStyleFilter;
  Index : integer;
begin
  Result    := TBitmap.Create;
  FileStream:=TFileStream.Create(StyleFileName, fmOpenRead);
  StyleFilter := TSeStyleFilter.Create;
  StyleSource := TSeStyleSource.Create(nil);
  try
    StyleFilter.SetStyleSource(StyleSource);
    StyleFilter.ReadStyle(FileStream);

    if StyleFilter.StyleSource.Bitmaps.Count>0 then
    //StyleFilter.StyleSource.Bitmaps.Count-1 do
    begin
        Index:=0;//only read the first bitmap of the style (for the moment).
        Result.PixelFormat:=pf32bit;
        Result.Width:= StyleFilter.StyleSource.Bitmaps[Index].Width;
        Result.Height:= StyleFilter.StyleSource.Bitmaps[Index].Height;
        StyleFilter.StyleSource.Bitmaps[Index].Draw(Result.Canvas,0,0);
    end;
  finally
    FileStream.Free;
    StyleFilter.Free;
    StyleSource.Free;
  end;
end;

procedure ReadVCLStyleInfo(const StyleFileName:string;var StyleInfo : TStyleInfo;Bitmap:TBitmap);
var
  FileStream : TFileStream;
  StyleSource: TSeStyleSource;
  StyleFilter : TSeStyleFilter;
  Index : integer;
begin

  FileStream:=TFileStream.Create(StyleFileName, fmOpenRead);
  StyleFilter := TSeStyleFilter.Create;
  StyleSource := TSeStyleSource.Create(nil);
  try
    StyleFilter.SetStyleSource(StyleSource);
    StyleFilter.ReadStyle(FileStream);
    StyleInfo.Name := StyleFilter.StyleSource.Name;
    StyleInfo.Version := StyleFilter.StyleSource.Version;
    StyleInfo.Author := StyleFilter.StyleSource.Author;
    StyleInfo.AuthorEMail := StyleFilter.StyleSource.AuthorEMail;
    StyleInfo.AuthorURL := StyleFilter.StyleSource.AuthorURL;

    if Assigned(Bitmap) and (StyleFilter.StyleSource.Bitmaps.Count>0) then
    //StyleFilter.StyleSource.Bitmaps.Count-1 do
    begin
        Index:=0;//only read the first bitmap of the style (for the moment).
        Bitmap.PixelFormat:=pf32bit;
        Bitmap.Width:= StyleFilter.StyleSource.Bitmaps[Index].Width;
        Bitmap.Height:= StyleFilter.StyleSource.Bitmaps[Index].Height;
        StyleFilter.StyleSource.Bitmaps[Index].Draw(Bitmap.Canvas,0,0);
        //b.SaveToFile(ChangeFileExt(StyleFileName,'.'+IntToStr(i)+'.bmp'));
        //StyleFilter.StyleSource.Bitmaps[i].SaveToFile(ChangeFileExt(StyleFileName,'.'+IntToStr(i)+'.bmp'));
    end;
  finally
    FileStream.Free;
    StyleFilter.Free;
    StyleSource.Free;
  end;
end;

{ TVCLStyleExt }

constructor TCustomStyleExt.Create(const FileName: string);
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(FileName, fmOpenRead);
  try
    Create(LStream);
  finally
    LStream.Free;
  end;
end;

constructor TCustomStyleExt.Create(const Stream: TStream);
begin
  inherited Create;
  FStream:=TMemoryStream.Create;

  Stream.Seek(0, soBeginning); //index 0 to load
  FStream.CopyFrom(Stream, Stream.Size);
  Stream.Seek(0, soBeginning); //restore index 0 after

  FStream.Seek(0, soBeginning);//index 0 to load
  TseStyle(Source).LoadFromStream(FStream);
end;

destructor TCustomStyleExt.Destroy;
begin
  if Assigned(FStream) then
    FStream.Free;
  inherited Destroy;
end;

function TCustomStyleExt.GetStyleInfo: TStyleInfo;
begin
 Result.Name        :=  TseStyle(Source).StyleSource.Name;
 Result.Author      :=  TseStyle(Source).StyleSource.Author;
 Result.AuthorEMail :=  TseStyle(Source).StyleSource.AuthorEMail;
 Result.AuthorURL   :=  TseStyle(Source).StyleSource.AuthorURL;
 Result.Version     :=  TseStyle(Source).StyleSource.Version;
end;
{$ENDIF}

procedure DrawSampleWindow(Style:TCustomStyle;Canvas:TCanvas;ARect:TRect;const ACaption : string);
var
  LDetails        : TThemedElementDetails;
  CaptionDetails  : TThemedElementDetails;
  IconDetails     : TThemedElementDetails;
  IconRect        : TRect;
  BorderRect      : TRect;
  CaptionRect     : TRect;
  ButtonRect      : TRect;
  TextRect        : TRect;
  CaptionBitmap   : TBitmap;

    function GetBorderSize: TRect;
    var
      Size: TSize;
      Details: TThemedElementDetails;
      Detail: TThemedWindow;
    begin
      Result  := Rect(0, 0, 0, 0);
      Detail  := twCaptionActive;
      Details := Style.GetElementDetails(Detail);
      Style.GetElementSize(0, Details, esActual, Size);
      Result.Top := Size.cy;
      Detail := twFrameLeftActive;
      Details := Style.GetElementDetails(Detail);
      Style.GetElementSize(0, Details, esActual, Size);
      Result.Left := Size.cx;
      Detail := twFrameRightActive;
      Details := Style.GetElementDetails(Detail);
      Style.GetElementSize(0, Details, esActual, Size);
      Result.Right := Size.cx;
      Detail := twFrameBottomActive;
      Details := Style.GetElementDetails(Detail);
      Style.GetElementSize(0, Details, esActual, Size);
      Result.Bottom := Size.cy;
    end;

    function RectVCenter(var R: TRect; Bounds: TRect): TRect;
    begin
      OffsetRect(R, -R.Left, -R.Top);
      OffsetRect(R, 0, (Bounds.Height - R.Height) div 2);
      OffsetRect(R, Bounds.Left, Bounds.Top);
      Result := R;
    end;

begin
  BorderRect := GetBorderSize;

  CaptionBitmap := TBitmap.Create;
  CaptionBitmap.SetSize(ARect.Width, BorderRect.Top);

  //Draw background
  LDetails.Element := teWindow;
  LDetails.Part := 0;
  Style.DrawElement(Canvas.Handle, LDetails, ARect);

  //Draw caption border
  CaptionRect := Rect(0, 0, CaptionBitmap.Width, CaptionBitmap.Height);
  LDetails := Style.GetElementDetails(twCaptionActive);
  Style.DrawElement(CaptionBitmap.Canvas.Handle, LDetails, CaptionRect);
  TextRect := CaptionRect;
  CaptionDetails := LDetails;

  //Draw icon
  IconDetails := Style.GetElementDetails(twSysButtonNormal);
  if not Style.GetElementContentRect(0, IconDetails, CaptionRect, ButtonRect) then
    ButtonRect := Rect(0, 0, 0, 0);
  IconRect := Rect(0, 0, GetSystemMetrics(SM_CXSMICON), GetSystemMetrics(SM_CYSMICON));
  RectVCenter(IconRect, ButtonRect);
  if ButtonRect.Width > 0 then
   if Assigned(Application.MainForm) then
    DrawIconEx(CaptionBitmap.Canvas.Handle, IconRect.Left, IconRect.Top, Application.MainForm.Icon.Handle, 0, 0, 0, 0, DI_NORMAL);

  Inc(TextRect.Left, ButtonRect.Width + 5);

  //Draw buttons

  //Close button
  LDetails := Style.GetElementDetails(twCloseButtonNormal);
  if Style.GetElementContentRect(0, LDetails, CaptionRect, ButtonRect) then
   Style.DrawElement(CaptionBitmap.Canvas.Handle, LDetails, ButtonRect);

  //Maximize button
  LDetails := Style.GetElementDetails(twMaxButtonNormal);
  if Style.GetElementContentRect(0, LDetails, CaptionRect, ButtonRect) then
    Style.DrawElement(CaptionBitmap.Canvas.Handle, LDetails, ButtonRect);

  //Minimize button
  LDetails := Style.GetElementDetails(twMinButtonNormal);

  if Style.GetElementContentRect(0, LDetails, CaptionRect, ButtonRect) then
    Style.DrawElement(CaptionBitmap.Canvas.Handle, LDetails, ButtonRect);

  //Help button
  LDetails := Style.GetElementDetails(twHelpButtonNormal);
  if Style.GetElementContentRect(0, LDetails, CaptionRect, ButtonRect) then
    Style.DrawElement(CaptionBitmap.Canvas.Handle, LDetails, ButtonRect);

  if ButtonRect.Left > 0 then
    TextRect.Right := ButtonRect.Left;

  //Draw text
  Style.DrawText(CaptionBitmap.Canvas.Handle, CaptionDetails, ACaption, TextRect, [tfLeft, tfSingleLine, tfVerticalCenter]);

  //Draw caption
  Canvas.Draw(0, 0, CaptionBitmap);
  CaptionBitmap.Free;

  //Draw left border
  CaptionRect := Rect(0, BorderRect.Top, BorderRect.Left, ARect.Height - BorderRect.Bottom);
  LDetails := Style.GetElementDetails(twFrameLeftActive);
  if CaptionRect.Bottom - CaptionRect.Top > 0 then
    Style.DrawElement(Canvas.Handle, LDetails, CaptionRect);

  //Draw right border
  CaptionRect := Rect(ARect.Width - BorderRect.Right, BorderRect.Top, ARect.Width, ARect.Height - BorderRect.Bottom);
  LDetails := Style.GetElementDetails(twFrameRightActive);
  Style.DrawElement(Canvas.Handle, LDetails, CaptionRect);

  //Draw Bottom border
  CaptionRect := Rect(0, ARect.Height - BorderRect.Bottom, ARect.Width, ARect.Height);
  LDetails := Style.GetElementDetails(twFrameBottomActive);
  Style.DrawElement(Canvas.Handle, LDetails, CaptionRect);
end;

initialization
{$IFDEF USE_VCL_STYLESAPI}
 InitStyleAPI;
{$ENDIF}

finalization
{$IFDEF USE_VCL_STYLESAPI}
 FinalizeStyleAPI;
{$ENDIF}


end.

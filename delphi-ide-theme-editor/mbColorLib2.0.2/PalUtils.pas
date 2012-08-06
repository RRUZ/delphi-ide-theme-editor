unit PalUtils;

interface

uses
 Windows, SysUtils, Classes, Graphics, RGBHSVUtils, RGBHSLUtils, RGBCIEUtils, RGBCMYKUtils,
 HTMLColors;

const
 clCustom = $2FFFFFFF;
 clTransparent = $3FFFFFFF;

type
 TSortOrder = (soAscending, soDescending);
 TSortMode = (smRed, smGreen, smBlue, smHue, smSaturation, smLuminance, smValue, smNone, smCyan, smMagenta, smYellow, smBlacK, smCIEx, smCIEy, smCIEz, smCIEl, smCIEa, smCIEb);

 AcoColors = record
  Colors: array of TColor;
  Names: array of WideString;
  HasNames: boolean;
 end;

//replaces passed strings with passed value
function ReplaceFlags(s: string; flags: array of string; value: integer): string;
//replaces the appropriate tags with values in a hint format string
function FormatHint(fmt: string; c: TColor): string;
//converts a string value to TColor including clCustom and clTransparent
function mbStringToColor(s: string): TColor;
//converts a TColor to a string value including clCustom and clTransparent
function mbColorToString(c: TColor): string;
//blends two colors together in proportion C1 : C2 = W1 : 100 - W1, where 0 <= W1 <= 100
function Blend(C1, C2: TColor; W1: Integer): TColor;
//generates a white-color-black or a black-color-white gradient palette
function MakePalette(BaseColor: TColor; SortOrder: TSortOrder): string;
//generates a gradient palette from the given colors
function MakeGradientPalette(Colors: array of TColor): string;
//sorts colors in a string list
procedure SortPalColors(Colors: TStrings; SortMode: TSortMode; SortOrder: TSortOrder);
//reads JASC .pal file
function ReadJASCPal(PalFile: TFileName): string;
//saves a string list to a JASC .pal file
procedure SaveJASCPal(pal: TStrings; FileName: TFileName);
//reads Photoshop .aco file into an Aco record
function ReadPhotoshopAco(PalFile: TFileName): AcoColors;
//reads Photoshop .act file
function ReadPhotoshopAct(PalFile: TFileName): string;

implementation

function ReplaceFlags(s: string; flags: array of string; value: integer): string;
var
  i, p: integer;
  v: string;
begin
  Result := s;
  v := IntToStr(value);
  for i := 0 to Length(flags) - 1 do
  begin
    p := Pos(flags[i], Result);
    if p > 0 then
    begin
      Delete(Result, p, Length(flags[i]));
      Insert(v, Result, p);
    end;
  end;
end;

function AnsiReplaceText(const AText, AFromText, AToText: string): string;
begin
  Result := StringReplace(AText, AFromText, AToText, [rfReplaceAll, rfIgnoreCase]);
end;

function FormatHint(fmt: string; c: TColor): string;
var
 h: string;
begin
 h := AnsiReplaceText(fmt, '%hex', ColorToHex(c));
 h := AnsiReplaceText(h, '%cieL', IntToStr(Round(GetCIElValue(c))));
 h := AnsiReplaceText(h, '%cieA', IntToStr(Round(GetCIEaValue(c))));
 h := AnsiReplaceText(h, '%cieB', IntToStr(Round(GetCIEbValue(c))));
 h := AnsiReplaceText(h, '%cieX', IntToStr(Round(GetCIExValue(c))));
 h := AnsiReplaceText(h, '%cieY', IntToStr(Round(GetCIEyValue(c))));
 h := AnsiReplaceText(h, '%cieZ', IntToStr(Round(GetCIEzValue(c))));
 h := AnsiReplaceText(h, '%cieC', IntToStr(Round(GetCIEcValue(c))));
 h := AnsiReplaceText(h, '%cieH', IntToStr(Round(GetCIEhValue(c))));
 h := AnsiReplaceText(h, '%hslH', IntToStr(RGBHSLUtils.GetHValue(c)));
 h := AnsiReplaceText(h, '%hslS', IntToStr(RGBHSLUtils.GetSValue(c)));
 h := AnsiReplaceText(h, '%hslL', IntToStr(RGBHSLUtils.GetLValue(c)));
 h := AnsiReplaceText(h, '%hsvH', IntToStr(RGBHSVUtils.GetHValue(c)));
 h := AnsiReplaceText(h, '%hsvS', IntToStr(RGBHSVUtils.GetSValue(c)));
 h := AnsiReplaceText(h, '%hsvV', IntToStr(RGBHSVUtils.GetVValue(c)));
 h := AnsiReplaceText(h, '%r', IntToStr(GetRValue(c)));
 h := AnsiReplaceText(h, '%g', IntToStr(GetGValue(c)));
 h := AnsiReplaceText(h, '%b', IntToStr(GetBValue(c)));
 h := AnsiReplaceText(h, '%c', IntToStr(GetCValue(c)));
 h := AnsiReplaceText(h, '%m', IntToStr(GetMValue(c)));
 h := AnsiReplaceText(h, '%y', IntToStr(GetYValue(c)));
 h := AnsiReplaceText(h, '%k', IntToStr(GetKValue(c)));
 h := AnsiReplaceText(h, '%h', IntToStr(RGBHSLUtils.GetHValue(c)));
 h := AnsiReplaceText(h, '%s', IntToStr(RGBHSLUtils.GetSValue(c)));
 h := AnsiReplaceText(h, '%l', IntToStr(RGBHSLUtils.GetLValue(c)));
 h := AnsiReplaceText(h, '%v', IntToStr(RGBHSVUtils.GetVValue(c)));
 Result := h;
end;

function mbStringToColor(s: string): TColor;
begin
 //remove spaces
 s := AnsiReplaceText(s, ' ', '');
 if SameText(s, 'clCustom') then
  Result := clCustom
 else
  if SameText(s, 'clTransparent') then
   Result := clTransparent
  else
   Result := StringToColor(s);
end;

function mbColorToString(c: TColor): string;
begin
 if c = clCustom then
  Result := 'clCustom'
 else
  if c = clTransparent then
   Result := 'clTransparent'
  else
   Result := ColorToString(c);
end;

//taken from TBXUtils, TBX Package © Alex Denisov (www.g32.org)
function Blend(C1, C2: TColor; W1: Integer): TColor;
var
  W2, A1, A2, D, F, G: Integer;
begin
  if C1 < 0 then C1 := GetSysColor(C1 and $FF);
  if C2 < 0 then C2 := GetSysColor(C2 and $FF);

  if W1 >= 100 then D := 1000
  else D := 100;

  W2 := D - W1;
  F := D div 2;

  A2 := C2 shr 16 * W2;
  A1 := C1 shr 16 * W1;
  G := (A1 + A2 + F) div D and $FF;
  Result := G shl 16;

  A2 := (C2 shr 8 and $FF) * W2;
  A1 := (C1 shr 8 and $FF) * W1;
  G := (A1 + A2 + F) div D and $FF;
  Result := Result or G shl 8;

  A2 := (C2 and $FF) * W2;
  A1 := (C1 and $FF) * W1;
  G := (A1 + A2 + F) div D and $FF;
  Result := Result or G;
end;

function IsMember(sl: TStrings; s: string): boolean;
var
 i: integer;
begin
 Result := false;
 for i := 0 to sl.count -1 do
  if sl.Strings[i] = s then
   Result := true;
end;

function MakePalette(BaseColor: TColor; SortOrder: TSortOrder): string;
var
 i: integer;
 s: TStrings;
begin
 Result := '';
 s := TStringList.Create;
 try
  case SortOrder of
   soAscending:
    for i := 239 downto 0 do
     s.Add(ColorToString(HSLRangeToRGB(GetHValue(BaseColor), GetSValue(BaseColor), 240 - i)));
   soDescending:
    for i := 0 to 239 do
     s.Add(ColorToString(HSLRangeToRGB(GetHValue(BaseColor), GetSValue(BaseColor), 240 - i)));
  end;
  Result := s.Text;
 finally
  s.Free;
 end;
end;

function MakeGradientPalette(Colors: array of TColor): string;
type
  RGBArray = array[0..2] of Byte;
var
  i, j, k, Span: Integer;
  s: TStringList;
  Scolor: string;
  Faktor: double;
  a: RGBArray;
  b: array of RGBArray;
begin
 Result := '';
 Span := 300;
 s := TStringList.Create;
 try
  SetLength(b, High(Colors) + 1);
  for i := 0 to High(Colors) do
   begin
    Colors[i] := ColorToRGB(Colors[i]);
    b[i, 0] := GetRValue(Colors[i]);
    b[i, 1] := GetGValue(Colors[i]);
    b[i, 2] := GetBValue(Colors[i]);
   end;
  for i := 0 to High(Colors) - 1 do
   for j := 0 to Span do
    begin
     Faktor := j / Span;
     for k := 0 to 3 do
      a[k] := Trunc(b[i, k] + ((b[i + 1, k] - b[i, k]) * Faktor));
     Scolor := ColorToString(RGB(a[0], a[1], a[2]));
     if not IsMember(s, Scolor) then
      s.add(Scolor);
    end;
  Result := s.Text;
 finally
  s.Free;
 end;
end;

procedure SortPalColors(Colors: TStrings; SortMode: TSortMode; SortOrder: TSortOrder);

 function MaxPos(s: TStrings; sm: TSortMode): integer;
 var
  i: integer;
  first: TColor;
 begin
  Result := 0;
  first := clBlack;
  for i := 0 to s.Count - 1 do
   case sm of
    smRed:
     if GetRValue(first) < GetRValue(mbStringToColor(s.Strings[i])) then
      begin
       first := mbStringToColor(s.Strings[i]);
       Result := i;
      end;
    smGreen:
     if GetGValue(first) < GetGValue(mbStringToColor(s.Strings[i])) then
      begin
       first := mbStringToColor(s.Strings[i]);
       Result := i;
      end;
    smBlue:
     if GetBValue(first) < GetBValue(mbStringToColor(s.Strings[i])) then
      begin
       first := mbStringToColor(s.Strings[i]);
       Result := i;
      end;
    smHue:
     if GetHValue(first) < GetHValue(mbStringToColor(s.Strings[i])) then
      begin
       first := mbStringToColor(s.Strings[i]);
       Result := i;
      end;
    smSaturation:
     if GetSValue(first) < GetSValue(mbStringToColor(s.Strings[i])) then
      begin
       first := mbStringToColor(s.Strings[i]);
       Result := i;
      end;
    smLuminance:
     if GetLValue(first) < GetLValue(mbStringToColor(s.Strings[i])) then
      begin
       first := mbStringToColor(s.Strings[i]);
       Result := i;
      end;
    smValue:
     if GetVValue(first) < GetVValue(mbStringToColor(s.Strings[i])) then
      begin
       first := mbStringToColor(s.Strings[i]);
       Result := i;
      end;
    smCyan:
     if GetCValue(first) < GetCValue(mbStringToColor(s.Strings[i])) then
      begin
       first := mbStringToColor(s.Strings[i]);
       Result := i;
      end;
    smMagenta:
     if GetMValue(first) < GetMValue(mbStringToColor(s.Strings[i])) then
      begin
       first := mbStringToColor(s.Strings[i]);
       Result := i;
      end;
    smYellow:
     if GetYValue(first) < GetYValue(mbStringToColor(s.Strings[i])) then
      begin
       first := mbStringToColor(s.Strings[i]);
       Result := i;
      end;
    smBlacK:
     if GetKValue(first) < GetKValue(mbStringToColor(s.Strings[i])) then
      begin
       first := mbStringToColor(s.Strings[i]);
       Result := i;
      end;
    smCIEx:
     if GetCIEXValue(first) < GetCIEXValue(mbStringToColor(s.Strings[i])) then
      begin
       first := mbStringToColor(s.Strings[i]);
       Result := i;
      end;
    smCIEy:
     if GetCIEYValue(first) < GetCIEYValue(mbStringToColor(s.Strings[i])) then
      begin
       first := mbStringToColor(s.Strings[i]);
       Result := i;
      end;
    smCIEz:
     if GetCIEZValue(first) < GetCIEZValue(mbStringToColor(s.Strings[i])) then
      begin
       first := mbStringToColor(s.Strings[i]);
       Result := i;
      end;
    smCIEl:
     if GetCIELValue(first) < GetCIELValue(mbStringToColor(s.Strings[i])) then
      begin
       first := mbStringToColor(s.Strings[i]);
       Result := i;
      end;
    smCIEa:
     if GetCIEAValue(first) < GetCIEAValue(mbStringToColor(s.Strings[i])) then
      begin
       first := mbStringToColor(s.Strings[i]);
       Result := i;
      end;
    smCIEb:
     if GetCIEBValue(first) < GetCIEBValue(mbStringToColor(s.Strings[i])) then
      begin
       first := mbStringToColor(s.Strings[i]);
       Result := i;
      end;
   end;
 end;

 function MinPos(s: TStrings; sm: TSortMode): integer;
 var
  i: integer;
  first: TColor;
 begin
  Result := 0;
  first := clWhite;
  for i := 0 to s.Count - 1 do
   case sm of
    smRed:
     if GetRValue(first) > GetRValue(mbStringToColor(s.Strings[i])) then
      begin
       first := mbStringToColor(s.Strings[i]);
       Result := i;
      end;
    smGreen:
     if GetGValue(first) > GetGValue(mbStringToColor(s.Strings[i])) then
      begin
       first := mbStringToColor(s.Strings[i]);
       Result := i;
      end;
    smBlue:
     if GetBValue(first) > GetBValue(mbStringToColor(s.Strings[i])) then
      begin
       first := mbStringToColor(s.Strings[i]);
       Result := i;
      end;
    smHue:
     if GetHValue(first) > GetHValue(mbStringToColor(s.Strings[i])) then
      begin
       first := mbStringToColor(s.Strings[i]);
       Result := i;
      end;
    smSaturation:
     if GetSValue(first) > GetSValue(mbStringToColor(s.Strings[i])) then
      begin
       first := mbStringToColor(s.Strings[i]);
       Result := i;
      end;
    smLuminance:
     if GetLValue(first) > GetLValue(mbStringToColor(s.Strings[i])) then
      begin
       first := mbStringToColor(s.Strings[i]);
       Result := i;
      end;
    smValue:
     if GetVValue(first) > GetVValue(mbStringToColor(s.Strings[i])) then
      begin
       first := mbStringToColor(s.Strings[i]);
       Result := i;
      end;
    smCyan:
     if GetCValue(first) > GetCValue(mbStringToColor(s.Strings[i])) then
      begin
       first := mbStringToColor(s.Strings[i]);
       Result := i;
      end;
    smMagenta:
     if GetMValue(first) > GetMValue(mbStringToColor(s.Strings[i])) then
      begin
       first := mbStringToColor(s.Strings[i]);
       Result := i;
      end;
    smYellow:
     if GetYValue(first) > GetYValue(mbStringToColor(s.Strings[i])) then
      begin
       first := mbStringToColor(s.Strings[i]);
       Result := i;
      end;
    smBlacK:
     if GetKValue(first) > GetKValue(mbStringToColor(s.Strings[i])) then
      begin
       first := mbStringToColor(s.Strings[i]);
       Result := i;
      end;
    smCIEx:
     if GetCIEXValue(first) > GetCIEXValue(mbStringToColor(s.Strings[i])) then
      begin
       first := mbStringToColor(s.Strings[i]);
       Result := i;
      end;
    smCIEy:
     if GetCIEYValue(first) > GetCIEYValue(mbStringToColor(s.Strings[i])) then
      begin
       first := mbStringToColor(s.Strings[i]);
       Result := i;
      end;
    smCIEz:
     if GetCIEZValue(first) > GetCIEZValue(mbStringToColor(s.Strings[i])) then
      begin
       first := mbStringToColor(s.Strings[i]);
       Result := i;
      end;
    smCIEl:
     if GetCIELValue(first) > GetCIELValue(mbStringToColor(s.Strings[i])) then
      begin
       first := mbStringToColor(s.Strings[i]);
       Result := i;
      end;
    smCIEa:
     if GetCIEAValue(first) > GetCIEAValue(mbStringToColor(s.Strings[i])) then
      begin
       first := mbStringToColor(s.Strings[i]);
       Result := i;
      end;
    smCIEb:
     if GetCIEBValue(first) > GetCIEBValue(mbStringToColor(s.Strings[i])) then
      begin
       first := mbStringToColor(s.Strings[i]);
       Result := i;
      end;
   end;
 end;

var
 i, m: integer;
 s: TStrings;
begin
 if SortMode <> smNone then
  begin
   if Colors.Count = 0 then Exit;
   m := 0;
   s := TStringList.Create;
   s.AddStrings(Colors);
   Colors.Clear;
   for i := s.Count - 1 downto 0 do
    begin
     case SortOrder of
      soAscending: m := MinPos(s, SortMode);
      soDescending: m := MaxPos(s, SortMode);
     end;
     Colors.Add(s.Strings[m]);
     s.Delete(m);
    end;
   s.Free;
  end;
end;

function ReadJASCPal(PalFile: TFileName): string;
var
 p, t, c: TStrings;
 i: integer;
begin
 if not FileExists(PalFile) then
  begin
   raise Exception.Create('File not found');
   Exit;
  end;
 p := TStringList.Create;
 t := TStringList.Create;
 c := TStringList.Create;
 try
  p.LoadFromFile(PalFile);
  for i := 0 to p.Count - 1 do
   if p.strings[i] <> '' then
    begin
     t.Clear;
     ExtractStrings([' '], [], PChar(p.strings[i]), t);
     if t.Count = 3 then
      c.Add(ColorToString(RGB(StrToInt(t.strings[0]), StrToInt(t.strings[1]), StrToInt(t.strings[2]))));
    end;
  Result := c.Text;
 finally
  c.Free;
  t.Free;
  p.Free;
 end;
end;

procedure SaveJASCPal(pal: TStrings; FileName: TFileName);
var
 i: integer;
 p: TStringList;
 c: TColor;
begin
 if not FileExists(FileName) then
  begin
   raise Exception.Create('File not found');
   Exit;
  end;
 p := TStringList.Create;
 try
  p.Add('JASC-PAL');
  p.Add('0100');
  p.Add('256');
  for i := 0 to pal.Count - 1 do
   if (pal.Strings[i] <> '') and not SameText(pal.Strings[i], 'clCustom') and not SameText(pal.Strings[i], 'clTransparent') then
    begin
     c := StringToColor(pal.Strings[i]);
     p.Add(IntToStr(GetRValue(c)) + ' ' + IntToStr(GetGValue(c)) + ' ' + IntToStr(GetBValue(c)));
    end;
  p.SaveToFile(FileName);
 finally
  p.Free;
 end;
end;

procedure ExchangeBytes(var w);
asm
 MOV DX,[w] //assign the word to the data register
 XCHG DL,DH // exchange low and high data values
 MOV [w],DX //assign the register data to the word
end;

procedure ExchangeChars(var s: WideString);
var
 i: Integer;
 w: Word;
begin
 for i := 1 to Length(s) do
  begin
   w := Word(s[i]);
   ExchangeBytes(w);
   s[i] := WideChar(w);
  end;
end;

function GetAcoColor(space,w,x,y,z: word): TColor;
begin
 case space of
  0: //RGB
    Result := RGB(w div 256, x div 256, y div 256);
  1: //HSB - HSV
    Result := HSVToColor(Round(w/182.04), Round(x/655.35), Round(y/655.35));
  2: //CMYK
    Result := CMYKToTColor(Round(100-w/55.35), Round(100-x/655.35), Round(100-y/655.35), Round(100-z/655.35));
  7: //Lab
    Result := LabToRGB(w/100, x/100, y/100);
  8: //Grayscale
    Result := RGB(Round(w/39.0625), Round(w/39.0625), Round(w/39.0625));
  9: //Wide CMYK
    Result := CMYKToTColor(w div 100, x div 100, y div 100, z div 100)
  else //unknown
   Result := RGB(w div 256, x div 256, y div 256);
 end;
end;

function ReadPhotoshopAco(PalFile: TFileName): AcoColors;
var
 f: file;
 ver, num, space, w, x, y, z, dummy: Word;
 i: integer;
 v0Length: byte;
 v0Name: string;
 v2Length: Word;
 v2Name: WideString;
begin
 if not FileExists(PalFile) then
  begin
   raise Exception.Create('File not found');
   SetLength(Result.Colors, 0);
   SetLength(Result.Names, 0);
   Result.HasNames := false;
   Exit;
  end;
 AssignFile(f, PalFile);
 Reset(f, 1);
 //read version
 BlockRead(f, ver, sizeof(ver));
 ExchangeBytes(ver);
 if not (ver in [0, 1, 2]) then
  begin
   CloseFile(f);
   Exception.Create('The file you are trying to load is not (yet) supported.'#13'Please submit the file for testing to MXS so loading of this version will be supported too');
   Exit;
  end;
 //read number of colors
 BlockRead(f, num, sizeof(num));
 ExchangeBytes(num);
 //read names
 if (ver = 0) or (ver = 2) then
  begin
   SetLength(Result.Names, num);
   Result.HasNames := true;
  end
 else
  begin
   SetLength(Result.Names, 0);
   Result.HasNames := false;
  end;
 //read colors
 SetLength(Result.Colors, num);
 for i := 0 to num - 1 do
  begin
   BlockRead(f, space, sizeof(space));
   ExchangeBytes(space);
   BlockRead(f, w, sizeof(w));
   ExchangeBytes(w);
   BlockRead(f, x, sizeof(x));
   ExchangeBytes(x);
   BlockRead(f, y, sizeof(y));
   ExchangeBytes(y);
   BlockRead(f, z, sizeof(z));
   ExchangeBytes(z);
   Result.Colors[i] := GetAcoColor(space, w, x, y, z);
   case ver of
    0:
     begin
      BlockRead(f, v0Length, SizeOf(v0Length));
      SetLength(v0Name, v0Length);
      if v0Length > 0 then
       BlockRead(f, PChar(v0Name)^, v0Length);
      Result.Names[i] := v0Name;
     end;
    2:
     begin
      BlockRead(f, dummy, sizeof(dummy));
      BlockRead(f, v2Length, SizeOf(v2Length));
      ExchangeBytes(v2Length);
      SetLength(v2Name, v2Length - 1);
      if v2Length > 0 then
       begin
        BlockRead(f, PWideChar(v2Name)^, 2*(v2Length - 1));
        ExchangeChars(v2Name);
       end;
      Result.Names[i] := v2Name;
      BlockRead(f, dummy, sizeof(dummy));
     end;
   end;
  end;
 CloseFile(f);
end;

function ReadPhotoshopAct(PalFile: TFileName): string;
var
 f: file;
 r, g, b: byte;
 s: TStringList;
 i: integer;
begin
 if not FileExists(PalFile) then
  begin
   raise Exception.Create('File not found');
   Result := '';
   Exit;
  end;
 s := TStringList.Create;
 try
  AssignFile(f, PalFile);
  Reset(f, 1);
  for i := 0 to 255 do
   begin
    BlockRead(f, r, sizeof(r));
    BlockRead(f, g, sizeof(g));
    BlockRead(f, b, sizeof(b));
    s.Add(ColorToString(RGB(r, g, b)));
   end;
  Result := s.Text;
 finally
  s.Free;
 end;
 CloseFile(f);
end;

end.

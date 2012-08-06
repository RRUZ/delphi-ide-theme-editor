unit Unit2;

interface

uses
  Windows, Messages, SysUtils, {$IFDEF DELPHI_6_UP}Variants,{$ENDIF} Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, mbColorTree, Math, XPMan, RGBHSLUtils;

type
  TColorDragObject = class(TDragControlObject)
  private
    FDragImages: TDragImageList;
  protected
    function GetDragImages: TDragImageList; override;
  public
    FBmp: TBitmap;
    destructor Destroy; override;
  end;

  TDragControl = class(TWinControl);

  TForm1 = class(TForm)
    Button1: TButton;
    XPManifest1: TXPManifest;
    t: TmbColorTree;
    procedure FormCreate(Sender: TObject);
    procedure tStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure tEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure tDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure Button1Click(Sender: TObject);
    procedure tMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormDestroy(Sender: TObject);
  private
    FDragObject: TColorDragObject;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  mx, my: integer;
  w: TDragControl;

implementation

uses ImgList;

{$R *.dfm}

destructor TColorDragObject.Destroy;
begin
  FDragImages.Free;
  inherited;
end;

function TColorDragObject.GetDragImages: TDragImageList;
begin
  if not Assigned(FDragImages) then
    FDragImages := TDragImageList.Create(nil);
  Result := FDragImages;
  Result.Clear;
  try
   Result.Width := FBmp.Width;
   Result.Height := FBmp.Height;
   Result.SetDragImage(Result.Add(FBmp, nil), 0, 0);
  finally
   FBmp.Free;
  end
end;

procedure TForm1.FormCreate(Sender: TObject);
var
 C: HCURSOR;
 i: integer;
begin
 C := LoadCursor(0, IDC_HAND);
 if C <> 0 then Screen.Cursors[crHandPoint] := C;

 w := TDragControl.CreateParented(Handle);
 w.Width := 1;
 w.Height := 1;
 w.Left := -1;
 w.Top := -1;
 insertcontrol(w);
 w.OnStartDrag := tStartDrag;
 w.OnEndDrag := tEndDrag;

 ControlStyle := ControlStyle + [csDisplayDragImage];
  for I := 0 to ControlCount - 1 do
    with Controls[I] do
      ControlStyle := ControlStyle + [csDisplayDragImage];
end;

procedure TForm1.tStartDrag(Sender: TObject; var DragObject: TDragObject);
var
 r: TRect;
 Bmp: TBitmap;
begin
 FDragObject := TColorDragObject.Create(Sender as TControl);

 Bmp := Tbitmap.Create;

 Bmp.PixelFormat := pf32bit;
 Bmp.Width := 48;
 Bmp.Height := 48;
 R := t.GetNodeAt(mx, my).DisplayRect(false);
 R := Rect(R.Left + 6, R.Top + 6, R.Left + 42, R.Top + 42);
 Bmp.Canvas.CopyRect(Rect(0, 0, 48, 48), t.Canvas, R);
 FDragObject.FBmp := Bmp;

 DragObject := FDragObject;
end;

procedure TForm1.tEndDrag(Sender, Target: TObject; X, Y: Integer);
begin
 FDragObject.Free;
 FDragObject := nil
end;

procedure TForm1.tDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
 accept := IsDragObject(Source);
end;

procedure TForm1.Button1Click(Sender: TObject);
var
 i: integer;
begin
 for i := 1 to 18 do
  t.AddColor('color'+inttostr(i), HSLRangeToRGB(i*20, 255, 150), false);
 t.UpdateColors;
end;

procedure TForm1.tMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
 if ssleft in shift then
  if t.GetNodeAt(x, y) <> nil then
   w.BeginDrag(false);
 mx := x;
 my := y;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
w.free;
end;

end.

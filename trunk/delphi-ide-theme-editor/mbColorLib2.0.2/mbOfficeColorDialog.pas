unit mbOfficeColorDialog;

interface

uses
  SysUtils, Classes, Windows, Graphics, Forms, OfficeMoreColorsDialog;

type
  TmbOfficeColorDialog = class(TComponent)
  private
   FWin: TOfficeMoreColorsWin;
   FSelColor: TColor;
   FUseHint: boolean;
  public
   constructor Create(AOwner: TComponent); override;
   function Execute: boolean; overload;
   function Execute(AColor: TColor): boolean; overload;
  published
   property SelectedColor: TColor read FSelColor write FSelColor default clWhite;
   property UseHints: boolean read FUseHint write FUseHint default false;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('mbColor Lib', [TmbOfficeColorDialog]);
end;

constructor TmbOfficeColorDialog.Create(AOwner: TComponent);
begin
 inherited;
 FSelColor := clWhite;
 FUseHint := false;
end;

function TmbOfficeColorDialog.Execute: boolean;
begin
 FWin := TOfficeMoreColorsWin.Create(Application);
 try
  FWin.OldSwatch.Color := FSelColor;
  FWin.ShowHint := FUseHint;
  Result := (FWin.ShowModal = IdOK);
  if Result then
   FSelColor := FWin.NewSwatch.Color
  else
   FSelColor := clNone;
 finally
  FWin.Free;
 end;
end;

function TmbOfficeColorDialog.Execute(AColor: TColor): boolean;
begin
 FWin := TOfficeMoreColorsWin.Create(Application);
 try
  FWin.OldSwatch.Color := AColor;
  FWin.ShowHint := FUseHint;
  Result := (FWin.ShowModal = IdOK);
  if Result then
   FSelColor := FWin.NewSwatch.Color
  else
   FSelColor := clNone;
 finally
  FWin.Free;
 end;
end;

end.

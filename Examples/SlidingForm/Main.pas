unit Main;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.UITypes,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Graphics,

  AnyiQuack,
  AQPControlAnimations,
  AQPSystemTypesAnimations;

type
  TForm1 = class(TForm)
    procedure FormClick(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormClick(Sender: TObject);
var
  NewLeft, NewWidth: Integer;
  NewColor: TColor;
  NewAlphaBlend: Byte;
  AniPlugin: TAQPControlAnimations;
  AQ: TAQ;
begin
  NewWidth := Screen.WorkAreaWidth div 2;
  if Left <> 0 then
  begin
    NewLeft := 0;
    NewColor := clBlack;
    NewAlphaBlend := 160;
  end
  else
  begin
    NewLeft := Screen.WorkAreaWidth - NewWidth;
    NewColor := clWhite;
    NewAlphaBlend := MAXBYTE;
  end;

  AQ := Take(Sender);

  AniPlugin := AQ
    .FinishAnimations
    .Plugin<TAQPControlAnimations>;
  AniPlugin.BoundsAnimation(NewLeft, 0, NewWidth, Screen.WorkAreaHeight,
      500, 0, TAQ.Ease(etBack, emInSnake));
  AniPlugin.AlphaBlendAnimation(NewAlphaBlend, 2000, 0, TAQ.Ease(etCircle, emInInverted));

  AQ.Plugin<TAQPSystemTypesAnimations>
    .ColorAnimation(NewColor,
    function(RefObject: TObject): TColor
    begin
      Result := TForm(RefObject).Color;
    end,
    procedure(RefObject: TObject; const NewColor: TColor)
    begin
      TForm(RefObject).Color := NewColor;
    end,
    1000, 0, TAQ.Ease(etCubic));
end;

end.

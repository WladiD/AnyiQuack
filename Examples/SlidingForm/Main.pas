unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,
  AnyiQuack,
  AQPControlAnimations; // AnyiQuack-Plugin

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

  AniPlugin := Take(Sender)
    .FinishAnimations
    .Plugin<TAQPControlAnimations>;
  AniPlugin.BoundsAnimation(NewLeft, 0, NewWidth, Screen.WorkAreaHeight,
      500, 0, TAQ.Ease(etBack, emInSnake));
  AniPlugin.BackgroundColorAnimation(NewColor, 1000, 0, TAQ.Ease(etCubic));
  AniPlugin.AlphaBlendAnimation(NewAlphaBlend, 2000, 0, TAQ.Ease(etCircle, emInInverted));
end;

end.

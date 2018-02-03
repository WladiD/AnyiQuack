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
  TypesAniPlugin: TAQPSystemTypesAnimations;
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

  TypesAniPlugin := Take(Sender)
    .FinishAnimations
    .Plugin<TAQPSystemTypesAnimations>;

  // Animate the BoundsRect (position and size) of the form
  TypesAniPlugin
    .RectAnimation(Rect(NewLeft, 0, NewLeft + NewWidth, Screen.WorkAreaHeight),
      function(RefObject: TObject): TRect
      begin
        Result := TForm(RefObject).BoundsRect;
      end,
      procedure(RefObject: TObject; const NewRect: TRect)
      begin
        TForm(RefObject).BoundsRect := NewRect;
      end,
      500, 0, TAQ.Ease(etBack, emInSnake));

  // Animate the background color
  TypesAniPlugin
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

  // Animate the AlphaBlendValue
  TypesAniPlugin.IntegerAnimation(NewAlphaBlend,
    function(RefObject: TObject): Integer
    begin
      Result := TForm(RefObject).AlphaBlendValue;
    end,
    procedure(RefObject: TObject; const NewValue: Integer)
    begin
      TForm(RefObject).AlphaBlendValue := Byte(NewValue);
    end,
    2000, 0, TAQ.Ease(etCircle, emInInverted));
end;

end.

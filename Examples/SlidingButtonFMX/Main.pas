unit Main;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Controls.Presentation,
  FMX.StdCtrls,

  AnyiQuack,
  AQPSystemTypesAnimations;

type
  TMainForm = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    XTrackBar: TTrackBar;
    YTrackBar: TTrackBar;
    Label2: TLabel;
    Label3: TLabel;
    procedure Button1Click(Sender: TObject);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

procedure TMainForm.Button1Click(Sender: TObject);
var
  TypesAniPlugin: TAQPSystemTypesAnimations;
  NewLeft, NewTop: Single;
  ButtonSender: TButton absolute Sender;
begin
  TypesAniPlugin := Take(Sender)
    .CancelAnimations
    .Plugin<TAQPSystemTypesAnimations>;

  NewLeft := Random(Trunc(ClientWidth - ButtonSender.Width));
  NewTop := Random(Trunc(ClientHeight - ButtonSender.Height));

  TypesAniPlugin.SingleAnimation(NewLeft,
    function(RefObject: TObject): Single
    begin
      Result := TButton(RefObject).Position.X;
    end,
    procedure(RefObject: TObject; const NewLeft: Single)
    begin
      TButton(RefObject).Position.X := NewLeft;
    end, Trunc(XTrackBar.Value), 0, TAQ.Ease(etBack));

  TypesAniPlugin.SingleAnimation(NewTop,
    function(RefObject: TObject): Single
    begin
      Result := TButton(RefObject).Position.Y;
    end,
    procedure(RefObject: TObject; const NewLeft: Single)
    begin
      TButton(RefObject).Position.Y := NewLeft;
    end, Trunc(YTrackBar.Value), 0, TAQ.Ease(etElastic));
end;

end.

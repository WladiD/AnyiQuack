unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation;

type
  TForm2 = class(TForm)
    LinearSlideButton: TButton;
    SlideBounceButton: TButton;
    SlideCircleButton: TButton;
    HighWaveButton: TButton;
    LowWaveSlideButton: TButton;
    MiddleWaveSlideButton: TButton;
    BackSlideButton: TButton;
    SinusSlideButton: TButton;
    ElasticSlideButton: TButton;
    QuadSlideButton: TButton;
    SextSlideButton: TButton;
    QuartSlideButton: TButton;
    CubicSlideButton: TButton;
    QuintSlideButton: TButton;
    Label3: TLabel;
    Label2: TLabel;
    YTrackBar: TTrackBar;
    XTrackBar: TTrackBar;
    Label1: TLabel;
    procedure SlideButtonClick(Sender: TObject);
    procedure XTrackBarChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses
  AQPSystemTypesAnimations, AnyiQuack;

{$R *.fmx}

procedure TForm2.SlideButtonClick(Sender: TObject);
var
  ButtonSender: TButton absolute Sender;
begin
  Take(Self)
    .ChildrenChain
    .FilterChain(TButton)
    .CancelAnimations
    .Each(
      function(AQ: TAQ; O: TObject): Boolean
      var
        TypesAniPlugin: TAQPSystemTypesAnimations;
        NewLeft, NewTop: Single;
      begin
        Result := True;
        TypesAniPlugin := Take(O).Plugin<TAQPSystemTypesAnimations>;

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
          end, Trunc(XTrackBar.Value), 0, TAQ.Ease(TEaseType(ButtonSender.Tag))); // Random ease type: TAQ.Ease(TEaseType(Random(Ord(High(TEaseType)))))

        TypesAniPlugin.SingleAnimation(NewTop,
          function(RefObject: TObject): Single
          begin
            Result := TButton(RefObject).Position.Y;
          end,
          procedure(RefObject: TObject; const NewLeft: Single)
          begin
            TButton(RefObject).Position.Y := NewLeft;
          end, Trunc(YTrackBar.Value), 0, TAQ.Ease(TEaseType(ButtonSender.Tag)));
      end);
end;

procedure TForm2.XTrackBarChange(Sender: TObject);
var
  SourceTB: TTrackBar absolute Sender;
  TargetTB: TTrackBar;
begin
  if SourceTB = XTrackBar then
    TargetTB := YTrackBar
  else
    TargetTB := XTrackBar;
  TargetTB.Value := SourceTB.Value;
end;

end.

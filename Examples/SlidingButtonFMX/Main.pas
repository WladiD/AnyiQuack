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
    QuintSlideButton: TButton;
    Label1: TLabel;
    XTrackBar: TTrackBar;
    YTrackBar: TTrackBar;
    Label2: TLabel;
    Label3: TLabel;
    LinearSlideButton: TButton;
    CubicSlideButton: TButton;
    QuartSlideButton: TButton;
    SextSlideButton: TButton;
    QuadSlideButton: TButton;
    ElasticSlideButton: TButton;
    SinusSlideButton: TButton;
    BackSlideButton: TButton;
    MiddleWaveSlideButton: TButton;
    LowWaveSlideButton: TButton;
    HighWaveButton: TButton;
    SlideCircleButton: TButton;
    SlideBounceButton: TButton;
    procedure SlideButtonClick(Sender: TObject);
    procedure TrackBarDblClick(Sender: TObject);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

procedure TMainForm.SlideButtonClick(Sender: TObject);
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

procedure TMainForm.TrackBarDblClick(Sender: TObject);
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

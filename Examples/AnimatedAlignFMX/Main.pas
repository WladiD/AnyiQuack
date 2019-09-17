unit Main;

interface

uses
  System.SysUtils, System.Types, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Controls.Presentation, FMX.ExtCtrls, FMX.ListBox, FMX.Colors,

  AQPControlAnimations, AnyiQuack;

type
  TMainForm = class(TForm)
    TopPanel: TPanel;
    Layout1: TLayout;
    Label1: TLabel;
    PanelSizeTrackBar: TTrackBar;
    Layout2: TLayout;
    Label2: TLabel;
    DisturbedComboBox: TPopupBox;
    Layout3: TLayout;
    Label3: TLabel;
    AnimationDurationTrackBar: TTrackBar;
    Layout4: TLayout;
    Label4: TLabel;
    HoverColorBox: TColorComboBox;
    BottomPanel: TPanel;
    AddPanelButton: TButton;
    RemovePanelButton: TButton;
    Label5: TLabel;
    Layout5: TLayout;
    Layout6: TLayout;
    HoverShakeCheckBox: TCheckBox;
    procedure AddPanelButtonClick(Sender: TObject);
    procedure RemovePanelButtonClick(Sender: TObject);
  private
    FPanelCounter: Integer;
  public
    procedure PanelMouseEnter(Sender: TObject);
    procedure PanelMouseLeave(Sender: TObject);
    procedure PanelHoverHandler(Sender: TObject; MouseOver: Boolean);

    procedure UpdateAlign;

    function GetPanelsAQ: TAQ;
  end;

var
  MainForm: TMainForm;

implementation

uses
  FMX.Objects, System.UITypes, System.Math;

{$R *.fmx}

{$DEFINE FMX}

const
  BoundsAnimationID = 1;
  HoverAnimationID = 2;
  HoverShakeAnimationID = 3;

procedure TMainForm.AddPanelButtonClick(Sender: TObject);
var
  P: TPanel;
  R: TRectangle;
  L: TLabel;
begin
  Inc(FPanelCounter);
  P := TPanel.Create(Self);
  P.Parent := Self;
  P.SetBounds(-100, -100, 100, 100);
  P.HitTest:=True;

  R:=TRectangle.Create(P);
  R.Parent:=P;
  R.Align:=TAlignLayout.Client;
  R.Fill.Kind:=TBrushKind.Solid;
  R.Fill.Color:=TAlphaColorRec.Whitesmoke;
  R.HitTest:=False;

  L:=TLabel.Create(R);
  L.Parent:=R;
  L.Align:=TAlignLayout.Client;
  L.TextSettings.HorzAlign:=TTextAlign.Center;
  L.TextSettings.VertAlign:=TTextAlign.Center;
  L.Text:= Format('Panel #%d', [FPanelCounter]);
  L.HitTest:=False;

  P.OnMouseEnter := PanelMouseEnter;
  P.OnMouseLeave := PanelMouseLeave;

  P.BringToFront;
  TopPanel.BringToFront;
  BottomPanel.BringToFront;

  UpdateAlign;
end;

{ TForm3 }

function TMainForm.GetPanelsAQ: TAQ;
begin
  Result:=Take(MainForm)
    .ChildrenChain
    .FilterChain(
      function(AQ: TAQ; O: TObject): Boolean
      begin
        Result := (O is TPanel) and (TControl(O).Tag = 0);
      end);
end;

procedure TMainForm.PanelHoverHandler(Sender: TObject; MouseOver: Boolean);
var
  AQ: TAQ;
  AQAniPlugin: TAQPControlAnimations;
  ShakeIt: Boolean;
  rect: TRectangle;
  lbl: TLabel;
  control: TControl;
  num: integer;
begin
  lbl:=nil;
  rect:=nil;

  AQ := Take(Sender);

  for control in (Sender as TControl).Controls do
  begin
    if control is TRectangle then
      rect:=control as TRectangle;
    if control is TLabel then
      lbl:=control as TLabel;
  end;

  if MouseOver then
  begin
    ShakeIt := HoverShakeCheckBox.IsChecked and
      not TAQ.HasActiveActors([arAnimation], Sender, BoundsAnimationID);

    AQAniPlugin := AQ
      .CancelAnimations(HoverAnimationID)
      .Plugin<TAQPControlAnimations>;

    if Assigned(lbl) then
      AQAniPlugin.FontColorAnimation<TLabel>(lbl, lbl.FontColor, HoverColorBox.Color xor $FFFFFF, 600,
      HoverAnimationID, TAQ.Ease(etCubic));

    if Assigned(Rect) then
      AQAniPlugin.BackgroundColorAnimation<TRectangle>
          (rect, rect.Fill.Color, HoverColorBox.Color, 300, HoverAnimationID,
            TAQ.Ease(etSinus));

    if ShakeIt then
      AQAniPlugin.ShakeAnimation(3, Floor(PanelSizeTrackBar.Value * 0.1), 2,
        Floor(PanelSizeTrackBar.Value * 0.05), 1000 + Round(AnimationDurationTrackBar.Value),
        BoundsAnimationID);
  end
  else
  begin
    AQAniPlugin := AQ.FinishAnimations(HoverAnimationID).Plugin<TAQPControlAnimations>;

    if Assigned(lbl) then
      AQAniPlugin.FontColorAnimation<TLabel>(lbl, lbl.FontColor,
            TAlphaColorRec.Black, 750, HoverAnimationID, TAQ.Ease(etCubic));

    if Assigned(Rect) then
      AQAniPlugin.BackgroundColorAnimation<TRectangle>
          (rect, TAlphaColorRec.White, 1500, HoverColorBox.Color, HoverAnimationID,
            TAQ.Ease(etSinus));
  end;
end;

procedure TMainForm.PanelMouseEnter(Sender: TObject);
begin
  PanelHoverHandler(Sender, TRUE);
end;

procedure TMainForm.PanelMouseLeave(Sender: TObject);
begin
  PanelHoverHandler(Sender, False);
end;

procedure TMainForm.RemovePanelButtonClick(Sender: TObject);
begin
  GetPanelsAQ
    .SliceChain(-1)
    .Each(
      function(AQ: TAQ; O: TObject): Boolean
      begin
        Result := TRUE;
        Dec(FPanelCounter);
        TControl(O).Tag := 1; // Dadurch wird es für GetPanelsAQ nicht greifbar
        AQ
          .CancelAnimations
          .Plugin<TAQPControlAnimations>
          .BoundsAnimation(Round(TControl(O).Position.X), Height, -1, -1,
            Round(AnimationDurationTrackBar.Value), 0, TAQ.Ease(etQuad),
            procedure(Sender: TObject)
            begin
              Sender.Free;
            end);
      end);
  UpdateAlign;
end;

procedure TMainForm.UpdateAlign;
var
  PanelsAQ: TAQ;
  AHeight, AWidth: Integer;
  PQSize, PIndex: Integer;
  PColumns, PRows, LeftOffset, TopOffset: Word;
begin
  PanelsAQ := GetPanelsAQ;

  RemovePanelButton.Enabled := PanelsAQ.Count > 0;

  AWidth := ClientWidth;
  AHeight := Round(ClientHeight - TopPanel.Height - BottomPanel.Height);
  PQSize := Round(PanelSizeTrackBar.Value);
  DivMod(AWidth, PQSize, PColumns, LeftOffset);
  DivMod(AHeight, PQSize, PRows, TopOffset);
  PColumns := Max(PColumns, 1);
  LeftOffset := (AWidth - (Min(PColumns, PanelsAQ.Count) * PQSize)) div 2;
  TopOffset := Round(((AHeight - (Min(Ceil(PanelsAQ.Count / PColumns), PRows) * PQSize)) div 2) +
    TopPanel.Height);
  PIndex := 0;

  PanelsAQ
    .CancelDelays(BoundsAnimationID)
    .EachDelay(50,
      function(AQ: TAQ; O: TObject): Boolean
      var
        TargetLeft, TargetTop: Integer;
        XTile, YTile, Dummy: Word;
      begin
        Result := True;
        {**
         * Anstehende Animationen beenden oder abbrechen
         *}
        if PIndex = 0 then
        begin
          if DisturbedComboBox.ItemIndex = 0 then
            AQ.CancelAnimations(BoundsAnimationID)
          else
            AQ.FinishAnimations(BoundsAnimationID);
        end;

        YTile := Floor(PIndex/PColumns);
        DivMod(((PIndex - (YTile * PColumns)) + PColumns), PColumns, Dummy, XTile);

        TargetLeft := (XTile * PQSize) + LeftOffset;
        TargetTop := (YTile * PQSize) + TopOffset;

        Take(O)
          .Plugin<TAQPControlAnimations>
          .BoundsAnimation(TargetLeft, TargetTop, PQSize, PQSize,
            Round(AnimationDurationTrackBar.Value), BoundsAnimationID, TAQ.Ease(etElastic));
        Inc(PIndex);
      end, BoundsAnimationID)
    .Die;
end;

end.

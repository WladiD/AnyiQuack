unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, AnyiQuack, AQPControlAnimations, AQPMessages;

type
  TMainForm = class(TForm)
    Label1: TLabel;
    AnimateCheckBox: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure AnimateCheckBoxClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    function LabelBlink(AQ: TAQ; O: TObject): Boolean;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  Tools, AQPStickyTools;

const
  FontColorAniID = 75;
  BGColorAniID = 76;

procedure TMainForm.AnimateCheckBoxClick(Sender: TObject);
begin
  TAQPStickyTools.AnimateStick := AnimateCheckBox.Checked;
end;

procedure TMainForm.FormCreate(Sender: TObject);

  function CreateToolsForm: TToolsForm;
  begin
    Result := TToolsForm.Create(Self);
    Result.PopupParent := Self;
  end;

var
  PaddingLeft, PaddingTop, ExitSizeMoveCount, WindowPosChangedCount: Integer;
  TF: TToolsForm;
  MsgPlugin: TAQPMessages;
begin
  PaddingLeft := GetSystemMetrics(SM_CXSIZEFRAME)
    + GetSystemMetrics(SM_CXBORDER)
    + GetSystemMetrics(SM_CXPADDEDBORDER)
    + 1;
  PaddingTop := GetSystemMetrics(SM_CYSIZEFRAME)
    + GetSystemMetrics(SM_CYBORDER)
    + GetSystemMetrics(SM_CXPADDEDBORDER)
    + 1;

  TF := CreateToolsForm;
  TF.Top := Self.Top;
  TF.Left := Self.Left - 80 - PaddingLeft;
  TF.Height := 300;
  TF.Width := 80;
  TF.Show;

  TF := CreateToolsForm;
  TF.Top := Self.Top + Self.Height + PaddingTop;
  TF.Left := Self.Left;
  TF.Height := 80;
  TF.Width := Self.Width;
  TF.Show;

  TF := CreateToolsForm;
  TF.Top := Self.Top + PaddingTop + GetSystemMetrics(SM_CYCAPTION) - 1;
  TF.Left := Self.Left + Self.ClientWidth - 202;
  TF.Height := 200;
  TF.Width := 200;
  TF.Show;

  ExitSizeMoveCount := 0;
  WindowPosChangedCount := 0;
  MsgPlugin := Take(Self).Plugin<TAQPMessages>;

  MsgPlugin.EachMessage(WM_EXITSIZEMOVE,
    function(AQ: TAQ; O: TObject; Message: TMessage): Boolean
    begin
      Inc(ExitSizeMoveCount);
      Caption := 'WM_EXITSIZEMOVE ' + IntToStr(ExitSizeMoveCount);
      Result := True;
    end, 111);
  MsgPlugin.EachMessage(WM_WINDOWPOSCHANGED,
    function(AQ: TAQ; O: TObject; Message: TMessage): Boolean
    begin
      Inc(WindowPosChangedCount);
      Caption := 'WM_WINDOWPOSCHANGED ' + IntToStr(WindowPosChangedCount);
      Result := True;
    end, 111);
  MsgPlugin.EachMessage(WM_LBUTTONDOWN,
    function(AQ: TAQ; O: TObject; Message: TMessage): Boolean
    begin
      Take(O)
        .CancelAnimations(BGColorAniID)
        .Plugin<TAQPControlAnimations>
        .BackgroundColorAnimation(clBlack, 250, BGColorAniID, TAQ.Ease(etCubic));
      TForm(O).Color := clBlack;
      Result := True;
    end);
  MsgPlugin.EachMessage(WM_LBUTTONUP,
    function(AQ: TAQ; O: TObject; Message: TMessage): Boolean
    begin
      Take(O)
        .CancelAnimations(BGColorAniID)
        .Plugin<TAQPControlAnimations>
        .BackgroundColorAnimation(clBtnFace, 250, BGColorAniID, TAQ.Ease(etCubic));
      Result := True;
    end);

  Take(Label1).Each(LabelBlink);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  Take(Self)
    .ChildrenChain
      .FilterChain(TToolsForm)
        .Plugin<TAQPStickyTools>
        .WorkAQ
      .EndChain
      .Die
    .EndChain;
end;

function TMainForm.LabelBlink(AQ: TAQ; O: TObject): Boolean;
var
  TargetColor: TColor;
begin
  if TLabel(O).Font.Color = clBlack then
    TargetColor := clRed
  else
    TargetColor := clBlack;

  AQ
    .Plugin<TAQPControlAnimations>
    .FontColorAnimation(TargetColor, 500, FontColorAniID, TAQ.Ease(etQuint),
      procedure(Sender: TObject)
      begin
        Take(Sender).EachDelay(200, LabelBlink);
      end);
  Result := False;
end;

end.

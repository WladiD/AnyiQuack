unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, Math, AnyiQuack;

type
  TForm1 = class(TForm)
    ListBox1: TListBox;
    ListBox2: TListBox;
    ListBox3: TListBox;
    ListBox4: TListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    SyncScrollBar: TScrollBar;
    ItemsCountTrackBar: TTrackBar;
    Label1: TLabel;
    Label2: TLabel;
    AnimatedScrollCheckBox: TCheckBox;
    Label3: TLabel;
    AnimationStyleComboBox: TComboBox;
    Panel3: TPanel;
    Label4: TLabel;
    AnimationDurationComboBox: TComboBox;

    procedure ItemsCountTrackBarChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SyncScrollBarChange(Sender: TObject);
  private
    function ListBoxesAQ: TAQ;
  end;

var
  Form1:TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  ItemsCountTrackBarChange(ItemsCountTrackBar);
end;

procedure TForm1.ItemsCountTrackBarChange(Sender: TObject);
begin
  TAQ.Take(Sender)
    .CancelDelays
    .EachDelay(500,
      function(AQ: TAQ; O: TObject): Boolean
      begin
        Result := True;
        SyncScrollBar.SetParams(0, 0, TTrackBar(Sender).Position - 1);
        ListBoxesAQ
          .Each(
            function(AQ: TAQ; O: TObject): Boolean
            var
              LB: TListBox absolute O;
            begin
              Result := True;
              LB.Clear;
              LB.Items.BeginUpdate;
            end)
          .EachRepeat(TTrackBar(O).Position,
            function(AQ: TAQ; O: TObject): Boolean
            var
              LB: TListBox absolute O;
            begin
              Result := True;
              LB.Items.Add(Format('Item #%d', [LB.Items.Count + 1]));
            end)
          .Each(
            function(AQ: TAQ; O: TObject): Boolean
            begin
              Result := True;
              TListBox(O).Items.EndUpdate;
            end);
      end);
end;

function TForm1.ListBoxesAQ: TAQ;
begin
  Result := Take(Form1)
    .ChildrenChain(True)
    .FilterChain(TListBox);
end;

procedure TForm1.SyncScrollBarChange(Sender:TObject);
var
  FirstItemIndex, ScrollItemIndex: Integer;
  ScrollEach: TEachFunction;
begin
  FirstItemIndex := ListBox1.TopIndex;
  ScrollItemIndex := SyncScrollBar.Position;
  Form1.Caption := IntToStr(ScrollItemIndex);

  ScrollEach := function(AQ: TAQ; O: TObject): Boolean
  var
    LB: TListBox absolute O;
  begin
    Result := True;
    if not (O is TListBox) then
      Exit;
    {**
     * Animiert
     *}
    if Assigned(AQ.CurrentInterval) then
      LB.TopIndex := TAQ.EaseInteger(FirstItemIndex, ScrollItemIndex,
        AQ.CurrentInterval.Progress, TEaseType(Ord(AnimationStyleComboBox.ItemIndex)))
    {**
     * Sofort
     *}
    else
      LB.TopIndex := ScrollItemIndex;
  end;

  ListBoxesAQ
    .IfThen(AnimatedScrollCheckBox.Checked)
      .CancelTimers
      .EachTimer((AnimationDurationComboBox.ItemIndex + 1) * 100, ScrollEach)
    .IfElse
      .Each(ScrollEach)
    .IfEnd;
end;

end.

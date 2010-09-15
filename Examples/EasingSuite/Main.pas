unit Main;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, ExtCtrls, StdCtrls, Math,
	GR32, GR32_Image, GR32_Layers,
	AnyiQuack, ComCtrls;

type
	TTrackerLayer = class;

	TMainForm = class(TForm)
		Panel1:TPanel;
		EaseGraphImage:TImage32;
		EaseTypeListBox:TListBox;
		AnimateOnChangeCheckBox:TCheckBox;
		AnimateButton:TButton;
		Panel2:TPanel;
		Label2:TLabel;
		VisPageControl:TPageControl;
		GraphTabSheet:TTabSheet;
		GroupBox1:TGroupBox;
		GroupBox2:TGroupBox;
		EaseModifierListBox:TListBox;
		EaseRealTabSheet:TTabSheet;
		EaseRealImage:TImage32;
		DurationPanel:TPanel;
		DurationTrackBar:TTrackBar;
		Panel3:TPanel;
		XAxisCheckBox: TCheckBox;
		YAxisCheckBox: TCheckBox;
		procedure FormCreate(Sender:TObject);
		procedure UpdateTabSheet(Sender:TObject);
		procedure AnimateButtonClick(Sender:TObject);
		procedure EaseGraphImageMouseMove(Sender:TObject; Shift:TShiftState; X, Y:Integer;
			Layer:TCustomLayer);
		procedure EaseRealImagePaintStage(Sender:TObject; Buffer:TBitmap32; StageNum:Cardinal);
		procedure DurationTrackBarChange(Sender:TObject);
	private
		FBackgroundLayer:TBitmapLayer;
		FGraphLayer:TBitmapLayer;
		FTrackerLayer:TTrackerLayer;
		FEaseRealProgress:Real;

		function GetEaseFunction:TEaseFunction;

		procedure BuildBackground;
		procedure BuildGraph;
		procedure UpdateCurrentTabSheet;

		procedure SetEaseRealProgress(NewProgress:Real);

		property EaseRealProgress:Real read FEaseRealProgress write SetEaseRealProgress;
	end;

	TTrackerLayer = class(TPositionedLayer)
	protected
		FEaseFunction:TEaseFunction;
		FProgress:Real;

		procedure Paint(Buffer: TBitmap32); override;
		procedure SetProgress(NewValue:Real);
	public
		constructor Create(ALayerCollection:TLayerCollection); override;

		property Progress:Real read FProgress write SetProgress;
	end;

var
	MainForm:TMainForm;

implementation

uses
	GR32_Polygons;

{$R *.dfm}

const
	HorizGuideSpace = 30;
	VertGuideSpace = 17;
	HorizGridOffset = 50;
	VertGridOffset = 100;

{** TMainForm **}

procedure TMainForm.AnimateButtonClick(Sender: TObject);
var
	Duration:Integer;
begin
	Duration:=Min(9999, DurationTrackBar.Position);
	AnimateButton.Enabled:=FALSE;
	if VisPageControl.ActivePage = GraphTabSheet then
		Take(FTrackerLayer)
			.CancelAnimations
			.EachAnimation(Duration,
				function(AQ:TAQ; O:TObject):Boolean
				begin
					with TTrackerLayer(O) do
						Progress:=AQ.CurrentInterval.Progress;
					if AQ.CurrentInterval.Progress = 1 then
						AnimateButton.Enabled:=TRUE;
					Result:=TRUE;
				end)
	else if VisPageControl.ActivePage = EaseRealTabSheet then
		Take(Self)
			.CancelAnimations
			.EachAnimation(Duration,
				function(AQ:TAQ; O:TObject):Boolean
				begin
					with TMainForm(O) do
					begin
						EaseRealProgress:=AQ.CurrentInterval.Progress;
						if AQ.CurrentInterval.Progress = 1 then
							AnimateButton.Enabled:=TRUE;
					end;
					Result:=TRUE;
				end);
end;

procedure TMainForm.EaseGraphImageMouseMove(Sender:TObject; Shift:TShiftState; X, Y:Integer;
	Layer:TCustomLayer);
begin
	if (Layer = FTrackerLayer) and AnimateButton.Enabled then
		with FTrackerLayer do
			Progress:=((X - HorizGridOffset) + 1) / (Location.Right - Location.Left);
end;

procedure TMainForm.BuildBackground;
var
	cc, X, Y:Integer;
begin
	with FBackgroundLayer do
	begin
		Bitmap.Clear(clGray32);

		Y:=VertGridOffset;
		for cc:=1 to (Bitmap.Width - (HorizGridOffset * 2)) div HorizGuideSpace do
		begin
			X:=HorizGridOffset + (HorizGuideSpace * cc);
			Bitmap.LineTS(X, Y, X, Bitmap.Height - VertGridOffset, $11FFFFFF);
		end;

		X:=HorizGridOffset;
		for cc:=1 to (Bitmap.Height - (VertGridOffset * 2)) div VertGuideSpace do
		begin
			Y:=VertGridOffset + (VertGuideSpace * cc);
			Bitmap.LineTS(X, Y, Bitmap.Width - HorizGridOffset, Y, $22FFFFFF);
		end;

		Bitmap.FrameRectS(HorizGridOffset, VertGridOffset, Bitmap.Width - HorizGridOffset,
			Bitmap.Height - VertGridOffset, clWhite32);
	end;
end;

procedure TMainForm.BuildGraph;
var
	EaseF:TEaseFunction;
	cc, ProgressSteps:Integer;
	Value:Real;
begin
	EaseF:=(GetEaseFunction);

	with FGraphLayer do
	begin
		Bitmap.Clear(0);
		ProgressSteps:=Max(1, Bitmap.Width);

		Bitmap.MoveToF(0, TAQ.EaseReal(Bitmap.Height, 0, 0, EaseF));
		Bitmap.PenColor:=clBlack32;

		for cc:=0 to ProgressSteps do
		begin
			Value:=TAQ.EaseReal(0, Bitmap.Height, cc / ProgressSteps, EaseF);
			Bitmap.LineToFS(cc, Bitmap.Height - Value);
		end;
	end;
end;

procedure TMainForm.DurationTrackBarChange(Sender: TObject);
begin
	DurationPanel.Caption:=Format('Animation duration (%d ms)', [DurationTrackBar.Position]);

end;

procedure TMainForm.FormCreate(Sender:TObject);
begin
	VisPageControl.TabIndex:=0;

	EaseTypeListBox.Selected[1]:=TRUE;
	EaseModifierListBox.Selected[0]:=TRUE;

	FBackgroundLayer:=TBitmapLayer.Create(EaseGraphImage.Layers);
	FBackgroundLayer.Bitmap.DrawMode:=dmBlend;

	FGraphLayer:=TBitmapLayer.Create(EaseGraphImage.Layers);
	FGraphLayer.Bitmap.DrawMode:=dmBlend;

	FTrackerLayer:=TTrackerLayer.Create(EaseGraphImage.Layers);

	with EaseRealImage.PaintStages[0]^ do
	begin
		Stage:=PST_CUSTOM;
		Parameter:=1;
	end;
	with EaseRealImage.PaintStages.Insert(PST_DRAW_LAYERS)^ do
	begin
		Stage:=PST_CUSTOM;
		Parameter:=2;
	end;

	UpdateCurrentTabSheet;
end;

function TMainForm.GetEaseFunction:TEaseFunction;
begin
	Result:=TAQ.Ease(TEaseType(Ord(EaseTypeListBox.ItemIndex)),
		TEaseModifier(Ord(EaseModifierListBox.ItemIndex)))
end;

procedure TMainForm.UpdateTabSheet(Sender:TObject);
begin
	UpdateCurrentTabSheet;
end;

procedure TMainForm.SetEaseRealProgress(NewProgress:Real);
begin
	if NewProgress = FEaseRealProgress then
		Exit;
	FEaseRealProgress:=NewProgress;
	EaseRealImage.Changed;
end;

procedure TMainForm.EaseRealImagePaintStage(Sender:TObject; Buffer:TBitmap32; StageNum:Cardinal);
const
	TrackerQSize = 100;
var
	X, Y:Real;

	// Nur zum Test
//	procedure DrawCircle(X, Y, Radius:Real);
//	const
//		Steps = 64;
//	var
//		SinResult, CosResult:Extended;
//		Circle:TPolygon32;
//		cc:Integer;
//	begin
//		X:=X + Radius;
//		Y:=Y + Radius;
//
//		Circle:=TPolygon32.Create;
//		try
//			Circle.Closed:=TRUE;
//			Circle.Antialiased:=TRUE;
//			for cc:=0 to Steps do
//			begin
//				SinCos((cc / Steps) * Pi * 2, SinResult, CosResult);
//				Circle.Add(FixedPoint(X + (Radius * SinResult), Y + (Radius * CosResult)));
//			end;
//			Circle.DrawEdge(Buffer, clTrWhite32);
//			Circle.DrawFill(Buffer, clTrBlack32);
//		finally
//			Circle.Free;
//		end;
//	end;
begin
	case EaseRealImage.PaintStages[StageNum].Parameter of
		1: // Background
		begin
			Buffer.Clear(clGray32);
			Buffer.FillRectTS(TrackerQSize, TrackerQSize,
				EaseRealImage.Width - TrackerQSize, EaseRealImage.Height - TrackerQSize,
				clTrWhite32);
		end;
		2: // Animated Circle
		begin
			if XAxisCheckBox.Checked then
				X:=TAQ.EaseReal(TrackerQSize, EaseRealImage.Width - (TrackerQSize * 2),
					EaseRealProgress, (GetEaseFunction))
			else
				X:=(EaseRealImage.Width - TrackerQSize) / 2;
			if YAxisCheckBox.Checked then
				Y:=TAQ.EaseReal(TrackerQSize, EaseRealImage.Height - (TrackerQSize * 2),
					EaseRealProgress, (GetEaseFunction))
			else
				Y:=(EaseRealImage.Height - TrackerQSize) / 2;

			Buffer.FillRectTS(0, Round(Y), EaseRealImage.Width, Round(Y + TrackerQSize),
				$4FFFFFFF);
			Buffer.FillRectTS(Round(X), 0, Round(X + TrackerQSize), EaseRealImage.Height,
				$4FFFFFFF);
			Buffer.FrameRectTS(Round(X) - 1, Round(Y) - 1,
				Round(X + TrackerQSize) + 1, Round(Y + TrackerQSize) + 1, clTrBlack32);
			//DrawCircle(X, Y, TrackerQSize / 2);
		end;
	end;
end;

procedure TMainForm.UpdateCurrentTabSheet;
begin
	EaseGraphImage.BeginUpdate;

	with FBackgroundLayer do
	begin
		Bitmap.SetSize(EaseGraphImage.Width, EaseGraphImage.Height);
		Location:=FloatRect(0, 0, Bitmap.Width, Bitmap.Height);
	end;
	BuildBackground;

	with FGraphLayer do
	begin
		Location:=FloatRect(HorizGridOffset, VertGridOffset,
			EaseGraphImage.Width - HorizGridOffset, EaseGraphImage.Height - VertGridOffset);
		Bitmap.SetSize(Trunc(Location.Right - Location.Left), Trunc(Location.Bottom - Location.Top));
	end;
	BuildGraph;

	with FTrackerLayer do
	begin
		FEaseFunction:=(GetEaseFunction);
		Location:=FloatRect(HorizGridOffset, VertGridOffset,
			EaseGraphImage.Width - HorizGridOffset, EaseGraphImage.Height - VertGridOffset);
	end;

	EaseGraphImage.EndUpdate;
	EaseGraphImage.Changed;

	if AnimateOnChangeCheckBox.Checked then
		AnimateButton.Click;
end;

{** TTrackerLayer **}

constructor TTrackerLayer.Create(ALayerCollection: TLayerCollection);
begin
	inherited Create(ALayerCollection);
end;

procedure TTrackerLayer.Paint(Buffer:TBitmap32);
const
	CrossSize = 5;
var
	StartProgress, ProgressStep, X, Y, AvailWidth:Real;
	EasedProgress:Real;
	DispWidth, DispHeight:Integer;
	ValueColor:TColor32;
	Caption:String;
begin
	with TCustomImage32(LayerCollection.Owner) do
	begin
		DispWidth:=Width;
		DispHeight:=Height;
	end;

	AvailWidth:=Location.Right - Location.Left;
	ProgressStep:=1 / AvailWidth;
	StartProgress:=0;
	X:=Location.Left + (AvailWidth * Progress);
	Y:=TAQ.EaseReal(Location.Bottom, Location.Top, Progress, FEaseFunction);
	EasedProgress:=0;
	ValueColor:=clBlack32;
	{**
	 * Graph-Linie
	 *}
	Buffer.MoveToF(Location.Left,
		TAQ.EaseReal(Location.Bottom, Location.Top, StartProgress, FEaseFunction));

	while StartProgress < Progress do
	begin
		StartProgress:=StartProgress + ProgressStep;
		if StartProgress > 1 then
			StartProgress:=1
		else if StartProgress < 0 then
			StartProgress:=0;
		EasedProgress:=FEaseFunction(StartProgress);
		if (EasedProgress = 1) or (EasedProgress = 0) then
			ValueColor:=clTrBlue32
		else if (EasedProgress > 0) and (EasedProgress < 1) then
			ValueColor:=clTrGreen32
		else
			ValueColor:=clTrRed32;
		Buffer.PenColor:=ValueColor;
		Buffer.LineToFS(Location.Left + (AvailWidth * StartProgress),
			TAQ.EaseReal(Location.Bottom, Location.Top, EasedProgress, nil));
	end;
	{**
	 * Horizontale und vertikale Trackerlinien
	 *}
	Buffer.LineFS(0, Y, DispWidth, Y, ValueColor);
	Buffer.LineFS(X, 0, X, DispHeight, ValueColor);
	Caption:=Format('%.4f', [Progress]);
	Buffer.Textout(Trunc(X + 5), DispHeight - (Buffer.TextHeight(Caption) + 2), Caption);
	Caption:=Format('%.4f', [EasedProgress]);
	Buffer.Textout(2, Trunc(Y + 5), Caption);
	{**
	 * Kreuz
	 *}
	Buffer.LineFS(X - CrossSize, Y - CrossSize, X + CrossSize, Y + CrossSize, ValueColor);
	Buffer.LineFS(X + CrossSize, Y - CrossSize, X - CrossSize, Y + CrossSize, ValueColor);
	{**
	 * Begrenzungen des Wertebalkens
	 *}
	Buffer.LineTS(DispWidth - HorizGridOffset, VertGridOffset, DispWidth, VertGridOffset, clWhite32);
	Buffer.LineTS(DispWidth - HorizGridOffset, DispHeight - 1 - VertGridOffset, DispWidth, DispHeight - 1 - VertGridOffset, clWhite32);
	{**
	 * Wertebalken
	 *}
	if Y < (DispHeight - VertGridOffset) then
		Buffer.FillRectTS(DispWidth - HorizGridOffset, Trunc(Y), DispWidth, DispHeight - VertGridOffset, ValueColor)
	else
		Buffer.FillRectTS(DispWidth - HorizGridOffset, DispHeight - VertGridOffset, DispWidth, Trunc(Y), ValueColor);
end;

procedure TTrackerLayer.SetProgress(NewValue:Real);
begin
	FProgress:=NewValue;
	Changed;
end;

end.

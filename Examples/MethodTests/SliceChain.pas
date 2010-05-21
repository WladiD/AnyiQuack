unit SliceChain;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, StdCtrls,
	AccessQuery,
	AQPControlAnimations; // AccessQuery-Plugin

type
	TForm1 = class(TForm)
		Button1:TButton;
		Button2:TButton;
		Button3:TButton;
		Button4:TButton;
		Button5:TButton;
		Button6:TButton;
		Button7:TButton;
		Button8:TButton;
		CheckBox1:TCheckBox;
		Button9:TButton;
		Button10:TButton;
		procedure Button1Click(Sender:TObject);
		procedure Button2Click(Sender:TObject);
		procedure Button3Click(Sender:TObject);
		procedure Button4Click(Sender:TObject);
		procedure Button5Click(Sender:TObject);
		procedure Button6Click(Sender:TObject);
		procedure Button7Click(Sender:TObject);
		procedure Button8Click(Sender:TObject);
		procedure Button9Click(Sender:TObject);
		procedure Button10Click(Sender:TObject);
	private
		{Private-Deklarationen}
	public
		{Public-Deklarationen}
		procedure SliceTest(StartIndex:Integer; Count:Integer = 0);
	end;

var
	Form1:TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button10Click(Sender:TObject);
begin
	SliceTest(5, 3);
end;

procedure TForm1.Button1Click(Sender:TObject);
begin
	SliceTest(0);
end;

procedure TForm1.Button2Click(Sender:TObject);
begin
	SliceTest(0, 1);
end;

procedure TForm1.Button3Click(Sender:TObject);
begin
	SliceTest(- 1);
end;

procedure TForm1.Button4Click(Sender:TObject);
begin
	SliceTest(- 2);
end;

procedure TForm1.Button5Click(Sender:TObject);
begin
	SliceTest(1, 1);
end;

procedure TForm1.Button6Click(Sender:TObject);
begin
	SliceTest(- 2, 1);
end;

procedure TForm1.Button7Click(Sender:TObject);
begin
	SliceTest(3);
end;

procedure TForm1.Button8Click(Sender:TObject);
begin
	SliceTest(1);
end;

procedure TForm1.Button9Click(Sender:TObject);
begin
	SliceTest(100);
end;

procedure TForm1.SliceTest(StartIndex, Count:Integer);
begin
	Take(Form1)
		.ChildrenChain
		.FilterChain(TButton)
		.SliceChain(StartIndex, Count)
		.FinishAnimations
		.Plugin<TAQPControlAnimations>.ShakeAnimation(1, 10, 0, 0, 300)
		.IfThen(CheckBox1.Checked)
			.DebugMessage
		.IfEnd;
end;

end.

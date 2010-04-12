unit AccessQuery;

interface

uses
	System, SysUtils, Contnrs;

type
	TObjectArray = array of TObject;

	TAQ = class(TObjectList)
	private

	protected

		class function Managed:TAQ;

	public
		constructor Create; overload;

		class function Take(Objects:TObjectArray):TAQ; overload;
		class function Take(AQ:TAQ):TAQ; overload;

	end;

implementation

{** TAQ **}

constructor TAQ.Create;
begin
	inherited Create(FALSE);
end;

class function TAQ.Take(Objects:TObjectArray):TAQ;
var
	cc:Integer;
begin
	Result:=Managed;
	for cc := 0 to Length(Objects) - 1 do
		Result.Add(Objects[cc]);
end;

class function TAQ.Managed:TAQ;
begin
	Result:=TAQ.Create;
end;

class function TAQ.Take(AQ:TAQ):TAQ;
begin
	Result:=Managed;
	Result.Assign(AQ);
	TObjectArray.Create();
end;

end.

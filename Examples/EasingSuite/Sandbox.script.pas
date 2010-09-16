program CustomEaseFunctions;

// Introduction
// ----------------------------------------------------------
// Here you can play and develope your own Ease-Function.
// You can define as many Ease-Functions as you want.
//
// Each Ease-Function...
// - Name must be ending with Ease (e.g. MyEase)
// - Muste be marked as export, because otherwise it will be
//   removed by the scripting engine
// - Must pass and return a Double-Type
// - To be valid, it should return 0 for 0 and 1 for 1, 
//   all left results between 0 and 1 are free
//
// This script will be interpreted by ScriptEngineII (Pascal like)
// from David Hüttig. Licensed unter MPL v1.1.
// http://sourceforge.net/projects/scriptengine2/
//
// Common Math-Functions are available through Math.Function 
// (e.g. Math.Sin, Math.Power, Math.Sqrt)
//
// ----------------------------------------------------------


function JitterEase(Progress:Double):Double; export;
begin
  Result:=Progress + (Math.Sin(Progress * 50 * Pi) * 0.03);
end;


// This Ease-Function is already in AnyiQuack available through
// etBounce, but you can modify and play with it.
function BounceEase(Progress:Double):Double; export;
const
  Base:Double = 7.5625;
begin
  if Progress < (1 / 2.75) then
    Result:=Base * Progress * Progress
  else if Progress < (2 / 2.75) then
  begin
    Progress:=Progress - (1.5 / 2.75);
    Result:=(Base * Progress) * Progress + 0.75;
  end
  else if Progress < (2.5 / 2.75) then
  begin
    Progress:=Progress - (2.25 / 2.75);
    Result:=(Base * Progress) * Progress + 0.9375;
  end
  else
  begin
    Progress:=Progress - (2.625/2.75);
    Result:=(7.5625 * Progress) * Progress + 0.984375;
  end;
end;


// This Ease-Function is already in AnyiQuack available through
// etCircle, but you can modify and play with it.
function CircleEase(Progress:Double):Double; export;
begin
  Result:=1 - Math.Sqrt(1 - Progress * Progress);
end;


// This function is not an Ease-Function, but a helper for 
// the followings
function Pixelize(Progress:Double; Steps:Integer):Double;
var
  Upscaling:Integer;
begin
  Result:=1;
  if (Progress + (1 / Steps)) >= 1 then
    Exit;
  Upscaling:=Math.Ceil(Progress * Steps) mod Steps;
  Result:=Upscaling / Steps;
end;


function Pixelize5Ease(Progress:Double):Double; export;
begin
  Result:=Pixelize(Progress, 5);
end;


function Pixelize10Ease(Progress:Double):Double; export;
begin
  Result:=Pixelize(Progress, 10);
end;


function Pixelize20Ease(Progress:Double):Double; export;
begin
  Result:=Pixelize(Progress, 20);
end;


// Do not remove the following block
begin
end.

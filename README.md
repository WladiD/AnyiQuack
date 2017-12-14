# AnyiQuack
Object-(Time-Based-)Batch processing with closures, using the most modern OOP design patterns for Delphi >= 2010. 
AnyiQuack is ideal for independent and distributed parametric animations, but the exact purpose is not specified.

## Features / quack points
* Most methods operate on the Chain-Pattern
* Chained by chains or branches of conditional chains
* Flexible plugin system for unlimited expandability
* Animation-Plugin for TControl descendants included
* Rationalized easing concept

## Example
```delphi
uses
  ..., AnyiQuack, AQPControlAnimations;
  
// Place some buttons and other controls on the form

procedure TForm1.Button1Click(Sender: TObject);
begin
  // Take the form 
  Take(Form1) 
    // Takes all childrens of Form1 recursive (=True)
    .ChildrenChain(True) 
    // Create a chain with only containing TButton descendants
    .FilterChain(TButton) 
    // If there are already running animations (on the contained buttons) 
    // in the chain, finish them
    .FinishAnimations 
    // Create a plugin chain
    .Plugin<TAQPControlAnimations> 
    // Play a shake animation on all contained buttons
    .ShakeAnimation(1, 10, 2, 5, 300); 
end;

```

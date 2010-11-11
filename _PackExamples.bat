@echo off
pushd Examples
SET Pack="C:\Program Files (x86)\WinRAR\winrar.exe" U -afzip -ep -m5 AnyiQuack.Examples.zip 
%Pack% AnimatedAlign\AnimatedAlign.exe
%Pack% EasingSuite\EasingSuite.exe
%Pack% PulsateEdits\PulsateEdits.exe
%Pack% SlidingForm\SlidingForm.exe
%Pack% StickyTools\StickyTools.exe
%Pack% SyncScroll\SyncScroll.exe

popd
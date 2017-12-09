@echo off
pushd Examples
SET Pack="C:\Program Files\7-Zip\7z.exe" u -tzip -m9 AnyiQuack.Examples.zip 
%Pack% AnimatedAlign\AnimatedAlign.exe
%Pack% EasingSuite\EasingSuite.exe
%Pack% PulsateEdits\PulsateEdits.exe
%Pack% SlidingForm\SlidingForm.exe
%Pack% StickyTools\StickyTools.exe
%Pack% SyncScroll\SyncScroll.exe
%Pack% NotificationWindows\NotificationExample.exe

popd
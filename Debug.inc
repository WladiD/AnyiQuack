{**
 * Gibt diverse Meldungen auf der Debug-Konsole aus
 *
 * Hinweis: Bestimmte Ausgaben erfolgen sehr oft, sodass das gesamte Programm scheinbar langsam
 *          wird. Doch das liegt am immensen Overhead der Windows-Funktion OutputDebugString
 *}
{$DEFINE OutputDebugString}

{$IFDEF OutputDebugString}
	{**
	 * Meldungen über Animationen
	 *}
	{.$DEFINE OutputDebugAnimation}
	{**
	 * Meldungen über aktive Intervalle
	 *}
	{.$DEFINE OutputDebugActiveIntervals}
	{**
	 * Meldungen vom GarbageCollector (GC)
	 *}
	{$DEFINE OutputDebugGarbageCollector}
	{**
	 * Typisierte Meldungen des GarbageCollector (folgend als GC abgekürzt)
	 *}
	{$IFDEF OutputDebugGarbageCollector}
		{**
		 * Meldung, wenn neue gemanagete TAQ-Instanzen erstellt werden
		 *}
		{.$DEFINE OutputDebugGCCreate}
		{**
		 * Meldung, wenn eine gamanagete TAQ-Instanz wiederverwendet wird, weil sie abgelaufen ist
		 *}
		{.$DEFINE OutputDebugGCRecycle}
		{**
		 * Meldung, wenn eine gemanagete TAQ-Instanz freigegeben wird
		 *}
		{.$DEFINE OutputDebugGCFree}
	{$ENDIF}
{$ENDIF}
{**
 * If you want to have smoother animations, you should use the threaded timer approach.
 * Otherwise the windows standard timer is used, which is not so precise.
 *}
{$DEFINE UseThreadTimer}

{**
 * Ensure, that the fastest bool evaluation is used
 *
 * Otherwise AnyiQuack don't work!
 *}
{$BOOLEVAL OFF}

{$IFDEF DEBUG}
  {**
   * Global switch for all debug related messages
   *
   * Enable the subset of needed informations in the following section.
   *
   * Warning: Some messages are send very often. And because the overhead of
   *          Log.d is not so tiny, the performance of the whole application
   *          can be affected.
   *}
  {.$DEFINE OutputDebug}
{$ENDIF}

{$IFDEF OutputDebug}
  {**
   * Messages about animations
   *}
  {.$DEFINE OutputDebugAnimation}

  {**
   * Messages about active intervals
   *}
  {.$DEFINE OutputDebugActiveIntervals}

  {**
   * Switch for all messages, related to the GarbageCollector (GC)
   *
   * Enable the subset of needed informations in the following section.
   *}
  {$DEFINE OutputDebugGarbageCollector}

  {$IFDEF OutputDebugGarbageCollector}
    {**
     * Message, if a new TAQ instance is added to the GarbageCollector
     *}
    {$DEFINE OutputDebugGCCreate}

    {**
     * Message, whether the clean up process of the GarbageCollector is started
     * or when a TAQ instance is removed from the GarbageCollector.
     *}
    {$DEFINE OutputDebugGCFree}
  {$ENDIF}
{$ENDIF}

{**
 * If you want to use AnyiQuark __only__ in FireMonkey applications,
 * enable this definition.
 *
 * Otherwise add the FMX conditional to your Firemonkey project.
 *}
{.$DEFINE FMX}

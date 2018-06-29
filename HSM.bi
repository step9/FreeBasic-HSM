
type QSIG as integer 
' Define the signal of state machine
enum ENUM_Q_SIG 
    Q_EMPTY_SIG = 0 '空消息，用于检测状态之间的父子关系
    /'<定义初始化状态迁移(initial transition)的消息，接受到该消息后，自动迁移到该状态的状态初始
            * 子状态
     '/
    Q_INIT_SIG = 1 
    Q_ENTRY_SIG 'entry 消息
    Q_EXIT_SIG 'exit 消息
    Q_USER_SIG '应用程序使用的状态的起始编号
end enum

' Define the signal of state machine
type QEVENT 
  as QSIG sig 
  as integer ptr pEvent1 
  as integer ptr pEvent2 
  ' TODO: add fields to the event
end type

' define state data type
type QPseudoState as  sub(pEvent as QEVENT ptr)'定义返回为void的函数指针类型
type QState as function(pEvent as QEVENT ptr) as  QPseudoState '定义返回为QPseudoState函数指针的函数指针类型
type TQSTATE as QPseudoState '定义函数指针类型

#define Q_TRIGGER(state, sig) _
   cast(QState,state) (cast(QEVENT ptr,@pkgStdEvt(cast(QSIG,sig))))'向状态state发送 sig信号

' define a transation that don't change the state,
' just treat the pEvent with the target state.
' this is used by concurrent state
#define Q_INIT(target_) Init_(cast(QState,target_)) 
#define Q_TRAN(target_) Q_TranDyc(cast(QState,target_)) 

declare sub Init_(target as QState) 
declare sub Q_Init_(target as TQSTATE ) 
declare function Q_Initial( pQevt as QEVENT ptr) as  QPseudoState 
declare sub Q_Dispatch(pQevt as QEVENT ptr ) 
declare sub Q_TranDyc(target as QState) 

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
dim shared srcState as QState ' source state
dim shared actState as QState ' active state
dim shared pkgStdEvt(3) as QEVENT = _
{ _
   (Q_EMPTY_SIG, 0, 0), _
   (Q_INIT_SIG, 0, 0), _
   (Q_ENTRY_SIG, 0, 0), _
   (Q_EXIT_SIG, 0, 0) _
} 

function Q_Initial( pQevt as QEVENT ptr) as  QPseudoState   
    print "Top_Init;" 
    return 0
end function

/'
 * @ingroup hsm
 * @brief
 * 向状态机发送信号
 * @details
 * 1.如果该状态机处理了该信号，返回0则结束该程序
 * 2.如果状态机为处理该信号，返回父状态的指针，向父状态发送该信号。
 * 3.重复步骤1、2，知道信号得到处理，或到达顶层状态。
 * @param 无
 * @retval  无
 * @note
 * @code
 * @endcode
 '/
sub Q_Dispatch(pQevt as QEVENT ptr) 
    srcState = actState
    while srcState
      srcState = cast(QState,srcState(pQevt)) 
    WEND 
end sub
/'
 * @ingroup hsm
 * @brief
 * 状态机的状态自动转向target
 * @details
 * 状态接收到init消息后条用该函数，
 * @param 无
 * @retval  无
 * @note
 * @code
 * @endcode
 '/
sub Init_(target as QState) 
    actState = target 
end sub

/'*
 * @ingroup hsm
 * @brief
 * 状态机初始化函数
 * @details
 * 1.srcState指向Q_Initial
 * 2.actState 指向制定的状态target
 * 3.如果target非最底层的状态，还要迁移至其初始状态
 * @param 无
 * @retval  无
 * @note
 * @code
 * @endcode
 '/
sub Q_Init_(target as TQSTATE) 
    dim s as QState  

    actState = cast(QState,target)
    srcState = cast(QState,@Q_Initial)

    s = actState     ' save actState in a temporary
    cast(QPseudoState,srcState)(cast(QEVENT ptr,0))    ' top-most initial tran.
                                    ' initial transition must go one level deep

    s = actState                                      ' update the temporary
    Q_TRIGGER(s, Q_ENTRY_SIG)   ' enter the state
    while (0 = Q_TRIGGER(s, Q_INIT_SIG)) 
        ' init handled
        ' initial transition must go one level deep
        s = actState 
        Q_TRIGGER(s, Q_ENTRY_SIG)   ' enter the substate 
    wend
end sub

/'
 * @ingroup hsm
 * @brief
   * 状态机状态迁移
 * @details
   * 状态机的状态需要向上逐级的推出，再向下逐级的进入
 * @param 无
 * @retval  无
 * @note
 * @code
 * @endcode
 '/
sub Q_TranDyc(target as QState) 
     dim as QState entry(8), p, q, s
     dim as QState ptr e, lca 
    s = actState
    while  s <> srcState  
         dim t as QState
         t = cast(QState,Q_TRIGGER(s, Q_EXIT_SIG)) 
         if (t) then 
'            // exit action unhandled, t points to superstate
             s = t  
         else 
'           // exit action handled, elicit superstate
             s = cast(QState,Q_TRIGGER(s, Q_EMPTY_SIG)) 
         end if
     wend
     e = @entry(0)
     *e = 0 
     e += 1
     *e = target                               ' assume entry to target

'    // (a) check source == target (transition to self)
     if (srcState = target) then 
         Q_TRIGGER(srcState, Q_EXIT_SIG)   ' exit source
         goto inLCA 
     end if
'    // (b) check source == target->super
     p = cast(QState,Q_TRIGGER(target, Q_EMPTY_SIG)) 
     if (srcState = p) then goto inLCA 
'    //(c) check source->super == target->super (most common)
     q = cast(QState,Q_TRIGGER(srcState, Q_EMPTY_SIG))
     if (q = p) then 
         Q_TRIGGER(srcState, Q_EXIT_SIG)   ' exit source
         goto inLCA 
     end if
'    // (d) check source->super == target
     if (q = target) then 
         Q_TRIGGER(srcState, Q_EXIT_SIG)            ' exit source
         e -= 1                                     ' not enter the LCA
         goto inLCA 
     end if
'    // (e) check rest of source == target->super->super... hierarchy
     e += 1
     *e = p 
     s = cast(QState,Q_TRIGGER(p, Q_EMPTY_SIG))
     while  s 
         if (srcState = s) then 
             goto inLCA 
         end if
         e += 1
         *e = s 
         s = cast(QState,Q_TRIGGER(s, Q_EMPTY_SIG))
     wend
     Q_TRIGGER(srcState, Q_EXIT_SIG)    ' exit source
'    // (f) check rest of source->super == target->super->super...
     lca = e
     while  *lca   
         if (q = *lca) then 
             e = lca - 1      ' do not enter the LCA
             goto inLCA 
         end if
         lca -= 1
     wend
'    // (g) check each srcState->super->super..for each target...
     s = q
     while s 
         lca = e
 
         while *lca   
             if (s = *lca) then 
                 e = lca - 1    ' do not enter the LCA
                 goto inLCA 
             end if
             lca -= 1
         wend
         Q_TRIGGER(s, Q_EXIT_SIG)    ' exit s
         s = cast(QState,Q_TRIGGER(s, Q_EMPTY_SIG))
     wend
'    assert(0);                                         // malformed HSM
inLCA:         ' now we are in the LCA of srcState and target
     assert(e < @entry(sizeof(entry) / sizeof(entry(0))))  'entry fits
     s = *e
     while (s) 
'        // retrace the entry path in reverse order
         Q_TRIGGER(s, Q_ENTRY_SIG)    ' enter s
         e -= 1
         s = *e
     wend
     actState = target     ' update current state
     while (0 = Q_TRIGGER(target, Q_INIT_SIG)) 
'        // initial transition must go one level deep
         assert(target = Q_TRIGGER(actState, Q_EMPTY_SIG)) 
         target = actState 
         Q_TRIGGER(target, Q_ENTRY_SIG)  ' enter target
     wend
end sub

 

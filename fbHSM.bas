'#CONSOLE ON
#define UNICODE
#INCLUDE ONCE "windows.bi"
'USING Afx
#INCLUDE ONCE "HSM.bi"
declare function s0(e as QEVENT ptr) as TQSTATE
declare function s1(e as QEVENT ptr) as TQSTATE
declare function s2(e as QEVENT ptr) as TQSTATE
declare function s11(e as QEVENT ptr) as TQSTATE
declare function s21(e as QEVENT ptr) as TQSTATE
declare function s211(e as QEVENT ptr) as TQSTATE
declare function Q_Top(e as QEVENT ptr) as TQSTATE
declare sub Initial(e as QEVENT ptr) 
dim shared bFoo as Boolean

enum QSignals  
   A_SIG = Q_USER_SIG 
   B_SIG
   C_SIG
   D_SIG
   E_SIG
   F_SIG
   G_SIG
   H_SIG
end enum

dim shared testQEvt(7) as QEVENT = _
{ _
   (A_SIG, 0, 0), (B_SIG, 0, 0), (C_SIG, 0, 0), (D_SIG, 0, 0), _
   (E_SIG, 0, 0), (F_SIG, 0, 0), (G_SIG, 0, 0), (H_SIG, 0, 0)  _
} 

 

'/**
' * @ingroup app
' * @brief
' * 状态机最顶层的状态
' * @details
' * @param 无
' * @retval  0 到最顶层了，只能自己处理了
' * @note
' * @code
' * @endcode
' */
function Q_Top(e as QEVENT ptr) as TQSTATE 
  return 0 
end function

'/**
' * @ingroup app
' * @brief
' * 状态机初始化，初始化到指定状态S0
' * @details
' * @param 无
' * @retval  无
' * @note
' * @code
' * @endcode
' */
sub Initial(e as QEVENT ptr) 
    bFoo = false 
    Q_Init_(cast(TQSTATE,@s0)) 
end sub


'/**
' * @ingroup app
' * @brief
' * s0状态消息处理函数
' * @details
' * @param e 传送到本状态的消息
' * @retval  0 接收的消息本状态已经处理完毕
' * @retval  其它非0值  消息本状态不做处理，交由父状态进行处理
' * @note
' * @code
' * @endcode
' */
function s0(e as QEVENT ptr) as TQSTATE 
 	if (e <> NULL) then 
 	  select case (e->sig) 
 		case Q_ENTRY_SIG:
 			print "s0-ENTRY;" 
 			return 0  '该处可以作为整个状态机初始化的借口
 		case Q_EXIT_SIG:
 			print "s0-EXIT;" 
 			return 0 
 		case Q_INIT_SIG:
 			print "s0-INIT;" 
 			Q_INIT(@s1)  
 			return 0 
 		case E_SIG:
 			print "s0-E;" 
 			Q_TRAN(@s211) 
 			return 0 
 	  end select
 	end if
 	return cast(TQSTATE,@Q_Top) 
end function

function s1(e as QEVENT ptr) as TQSTATE 
	select case (e->sig)
	case Q_ENTRY_SIG:
 		print "s1-ENTRY;" 
 		return 0 
	case Q_EXIT_SIG:
 		print "s1-EXIT;" 
 		return 0 
	case Q_INIT_SIG:
 		print "s1-INIT;" 
 		Q_INIT(@s11) 
 		return 0 
	case A_SIG:
 		print "s1-A;" 
 		Q_TRAN(@s1) 
 		return 0 
	case B_SIG:
 		print "s1-B;" 
 		Q_TRAN(@s11) 
 		return 0 
	case C_SIG:
 		print "s1-C;" 
 		Q_TRAN(@s2) 
 		return 0 
	case D_SIG:
 		print "s1-D;" 
 		Q_TRAN(@s0) 
 		return 0 
 	case F_SIG:
 		print "s1-F;"  
 		Q_TRAN(@s211) 
 		return 0 
 	end select
 	return cast(TQSTATE,@s0)
end function

function s11(e as QEVENT ptr) as TQSTATE  
    select case (e->sig)  
    case Q_ENTRY_SIG: print "s11-ENTRY;": return 0 
    case Q_EXIT_SIG:  print "s11-EXIT;" : return 0 
    case G_SIG:  print "s11-G;" : Q_TRAN(@s211) : return 0 
    case H_SIG:                 ' internal transition with a guard
       if (bFoo) then ' test the guard condition
          print "s11-H;" 
          bFoo = false 
          return 0 
       end if 
    end select
    return cast(TQSTATE,@s1)
end function

function s2(e as QEVENT ptr) as TQSTATE 
    select case (e->sig)  
    case Q_ENTRY_SIG: print "s2-ENTRY;" : return 0 
    case Q_EXIT_SIG: print "s2-EXIT;" :   return 0 
    case Q_INIT_SIG: print "s2-INIT;" : Q_INIT(@s21) : return 0 
    case C_SIG:      print "s2-C;" :   Q_TRAN(@s1) :  return 0 
    case F_SIG:      print "s2-F;" :   Q_TRAN(@s11) : return 0 
    end select
    return cast(TQSTATE,@s0) 
end function

function s21(e as QEVENT ptr) as TQSTATE  
    select case (e->sig)  
    case Q_ENTRY_SIG: print "s21-ENTRY;" : return 0 
    case Q_EXIT_SIG: print "s21-EXIT;" :   return 0 
    case Q_INIT_SIG:print "s21-INIT;" : Q_INIT(@s211):return 0 
    case B_SIG:     print "s21-B;" :   Q_TRAN(@s211):return 0 
    case H_SIG:                     ' self transition with a guard
       if (bFoo = false) then 
        ' test the guard condition
          print "s21-H;" 
          bFoo = true 
          Q_TRAN(@s21)' self transition
          return 0 
       end if
       'break to return the superstate
    end select
    return cast(TQSTATE,@s2)
end function

function s211(e as QEVENT ptr) as TQSTATE  
    select case (e->sig)  
    case Q_ENTRY_SIG: print "s211-ENTRY;" : return 0 
    case Q_EXIT_SIG:  print "s211-EXIT;" :  return 0 
    case D_SIG: print "s211-D;" : Q_TRAN(@s21) : return 0 
    case G_SIG: print "s211-G;" : Q_TRAN(@s0) :  return 0 
    end select
    return cast(TQSTATE,@s21)
end function
    print "Hiberarchy state machine testing\n" 

    Initial(0) ' trigger initial transition
    while (1) 
 		dim c as string 
 		print "\nSignal<-" 
 		Input   "Please enter a character: ", c
      if c>="a" and c<="h" then 
 		  Q_Dispatch(@testQEvt(asc(c) - asc("a"))) ' dispatch
      end if
 	wend
'	return 0;
PRINT
PRINT "Press any key..."
SLEEP

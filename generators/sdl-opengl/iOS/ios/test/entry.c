#ifdef ___LINKER_INFO
; File: "entry.c", produced by Gambit-C v4.7.2
(
407002
" entry"
((" entry"))
(
"entry"
)
(
)
(
" entry"
" entry#0"
" entry#1"
" entry#2"
"ios-device"
"ios-device-description"
"printf"
)
(
)
(
"current-directory"
"file-exists?"
"load"
"number->string"
"string-append"
)
 ()
)
#else
#define ___VERSION 407002
#define ___MODULE_NAME " entry"
#define ___LINKER_ID ____20_entry
#define ___MH_PROC ___H__20_entry
#define ___SCRIPT_LINE 0
#define ___SYMCOUNT 1
#define ___GLOCOUNT 12
#define ___SUPCOUNT 7
#define ___SUBCOUNT 10
#define ___LBLCOUNT 26
#define ___MODDESCR ___REF_SUB(9)
#include "gambit.h"

___NEED_SYM(___S_entry)

___NEED_GLO(___G__20_entry)
___NEED_GLO(___G__20_entry_23_0)
___NEED_GLO(___G__20_entry_23_1)
___NEED_GLO(___G__20_entry_23_2)
___NEED_GLO(___G_current_2d_directory)
___NEED_GLO(___G_file_2d_exists_3f_)
___NEED_GLO(___G_ios_2d_device)
___NEED_GLO(___G_ios_2d_device_2d_description)
___NEED_GLO(___G_load)
___NEED_GLO(___G_number_2d__3e_string)
___NEED_GLO(___G_printf)
___NEED_GLO(___G_string_2d_append)

___BEGIN_SYM
___DEF_SYM(0,___S_entry,"entry")
___END_SYM

#define ___SYM_entry ___SYM(0,___S_entry)

___BEGIN_GLO
___DEF_GLO(0," entry")
___DEF_GLO(1," entry#0")
___DEF_GLO(2," entry#1")
___DEF_GLO(3," entry#2")
___DEF_GLO(4,"ios-device")
___DEF_GLO(5,"ios-device-description")
___DEF_GLO(6,"printf")
___DEF_GLO(7,"current-directory")
___DEF_GLO(8,"file-exists?")
___DEF_GLO(9,"load")
___DEF_GLO(10,"number->string")
___DEF_GLO(11,"string-append")
___END_GLO

#define ___GLO__20_entry ___GLO(0,___G__20_entry)
#define ___PRM__20_entry ___PRM(0,___G__20_entry)
#define ___GLO__20_entry_23_0 ___GLO(1,___G__20_entry_23_0)
#define ___PRM__20_entry_23_0 ___PRM(1,___G__20_entry_23_0)
#define ___GLO__20_entry_23_1 ___GLO(2,___G__20_entry_23_1)
#define ___PRM__20_entry_23_1 ___PRM(2,___G__20_entry_23_1)
#define ___GLO__20_entry_23_2 ___GLO(3,___G__20_entry_23_2)
#define ___PRM__20_entry_23_2 ___PRM(3,___G__20_entry_23_2)
#define ___GLO_ios_2d_device ___GLO(4,___G_ios_2d_device)
#define ___PRM_ios_2d_device ___PRM(4,___G_ios_2d_device)
#define ___GLO_ios_2d_device_2d_description ___GLO(5,___G_ios_2d_device_2d_description)
#define ___PRM_ios_2d_device_2d_description ___PRM(5,___G_ios_2d_device_2d_description)
#define ___GLO_printf ___GLO(6,___G_printf)
#define ___PRM_printf ___PRM(6,___G_printf)
#define ___GLO_current_2d_directory ___GLO(7,___G_current_2d_directory)
#define ___PRM_current_2d_directory ___PRM(7,___G_current_2d_directory)
#define ___GLO_file_2d_exists_3f_ ___GLO(8,___G_file_2d_exists_3f_)
#define ___PRM_file_2d_exists_3f_ ___PRM(8,___G_file_2d_exists_3f_)
#define ___GLO_load ___GLO(9,___G_load)
#define ___PRM_load ___PRM(9,___G_load)
#define ___GLO_number_2d__3e_string ___GLO(10,___G_number_2d__3e_string)
#define ___PRM_number_2d__3e_string ___PRM(10,___G_number_2d__3e_string)
#define ___GLO_string_2d_append ___GLO(11,___G_string_2d_append)
#define ___PRM_string_2d_append ___PRM(11,___G_string_2d_append)

___DEF_SUB_STR(___X0,1)
               ___STR1(10)
___DEF_SUB_STR(___X1,14)
               ___STR8(105,79,83,32,72,97,114,100)
               ___STR6(119,97,114,101,58,32)
___DEF_SUB_STR(___X2,1)
               ___STR1(10)
___DEF_SUB_STR(___X3,7)
               ___STR7(109,97,105,110,46,111,49)
___DEF_SUB_STR(___X4,7)
               ___STR7(109,97,105,110,46,111,49)
___DEF_SUB_STR(___X5,8)
               ___STR8(109,97,105,110,46,115,99,109)
               ___STR0
___DEF_SUB_STR(___X6,1)
               ___STR1(10)
___DEF_SUB_STR(___X7,33)
               ___STR8(109,97,105,110,46,115,99,109)
               ___STR8(47,109,97,105,110,46,111,49)
               ___STR8(32,58,58,32,110,111,116,32)
               ___STR8(102,111,117,110,100,32,105,110)
               ___STR1(32)
___DEF_SUB_STR(___X8,8)
               ___STR8(109,97,105,110,46,115,99,109)
               ___STR0
___DEF_SUB_VEC(___X9,5)
               ___VEC1(___REF_SYM(0,___S_entry))
               ___VEC1(___REF_PRC(1))
               ___VEC1(___REF_FIX(1))
               ___VEC1(___REF_NUL)
               ___VEC1(___REF_FAL)
               ___VEC0

___BEGIN_SUB
 ___DEF_SUB(___X0)
,___DEF_SUB(___X1)
,___DEF_SUB(___X2)
,___DEF_SUB(___X3)
,___DEF_SUB(___X4)
,___DEF_SUB(___X5)
,___DEF_SUB(___X6)
,___DEF_SUB(___X7)
,___DEF_SUB(___X8)
,___DEF_SUB(___X9)
___END_SUB




#include "stdio.h"
#import "UIDevice+Hardware.h"


#undef ___MD_ALL
#define ___MD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___MR_ALL
#define ___MR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___MW_ALL
#define ___MW_ALL ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_M_COD
___BEGIN_M_HLBL
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__20_entry)
___DEF_M_HLBL(___L1__20_entry)
___DEF_M_HLBL(___L2__20_entry)
___DEF_M_HLBL(___L3__20_entry)
___DEF_M_HLBL(___L4__20_entry)
___DEF_M_HLBL(___L5__20_entry)
___DEF_M_HLBL(___L6__20_entry)
___DEF_M_HLBL(___L7__20_entry)
___DEF_M_HLBL(___L8__20_entry)
___DEF_M_HLBL(___L9__20_entry)
___DEF_M_HLBL(___L10__20_entry)
___DEF_M_HLBL(___L11__20_entry)
___DEF_M_HLBL(___L12__20_entry)
___DEF_M_HLBL(___L13__20_entry)
___DEF_M_HLBL(___L14__20_entry)
___DEF_M_HLBL(___L15__20_entry)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__20_entry_23_0)
___DEF_M_HLBL(___L1__20_entry_23_0)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__20_entry_23_1)
___DEF_M_HLBL(___L1__20_entry_23_1)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__20_entry_23_2)
___DEF_M_HLBL(___L1__20_entry_23_2)
___END_M_HLBL

___BEGIN_M_SW

#undef ___PH_PROC
#define ___PH_PROC ___H__20_entry
#undef ___PH_LBL0
#define ___PH_LBL0 1
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__20_entry)
___DEF_P_HLBL(___L1__20_entry)
___DEF_P_HLBL(___L2__20_entry)
___DEF_P_HLBL(___L3__20_entry)
___DEF_P_HLBL(___L4__20_entry)
___DEF_P_HLBL(___L5__20_entry)
___DEF_P_HLBL(___L6__20_entry)
___DEF_P_HLBL(___L7__20_entry)
___DEF_P_HLBL(___L8__20_entry)
___DEF_P_HLBL(___L9__20_entry)
___DEF_P_HLBL(___L10__20_entry)
___DEF_P_HLBL(___L11__20_entry)
___DEF_P_HLBL(___L12__20_entry)
___DEF_P_HLBL(___L13__20_entry)
___DEF_P_HLBL(___L14__20_entry)
___DEF_P_HLBL(___L15__20_entry)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__20_entry)
   ___IF_NARGS_EQ(0,___NOTHING)
   ___WRONG_NARGS(0,0,0,0)
___DEF_GLBL(___L__20_entry)
   ___SET_GLO(4,___G_ios_2d_device,___PRC(18))
   ___SET_GLO(5,___G_ios_2d_device_2d_description,___PRC(21))
   ___SET_GLO(6,___G_printf,___PRC(24))
   ___SET_STK(1,___R0)
   ___SET_R0(___LBL(2))
   ___ADJFP(4)
   ___POLL(1)
___DEF_SLBL(1,___L1__20_entry)
   ___JUMPGLOSAFE(___SET_NARGS(0),5,___G_ios_2d_device_2d_description)
___DEF_SLBL(2,___L2__20_entry)
   ___SET_R2(___R1)
   ___SET_R3(___SUB(0))
   ___SET_R1(___SUB(1))
   ___SET_R0(___LBL(3))
   ___JUMPGLOSAFE(___SET_NARGS(3),11,___G_string_2d_append)
___DEF_SLBL(3,___L3__20_entry)
   ___SET_R0(___LBL(4))
   ___JUMPGLOSAFE(___SET_NARGS(1),6,___G_printf)
___DEF_SLBL(4,___L4__20_entry)
   ___SET_R0(___LBL(5))
   ___JUMPGLOSAFE(___SET_NARGS(0),4,___G_ios_2d_device)
___DEF_SLBL(5,___L5__20_entry)
   ___SET_R0(___LBL(6))
   ___JUMPGLOSAFE(___SET_NARGS(1),10,___G_number_2d__3e_string)
___DEF_SLBL(6,___L6__20_entry)
   ___SET_R2(___SUB(2))
   ___SET_R0(___LBL(7))
   ___JUMPGLOSAFE(___SET_NARGS(2),11,___G_string_2d_append)
___DEF_SLBL(7,___L7__20_entry)
   ___SET_R0(___LBL(8))
   ___JUMPGLOSAFE(___SET_NARGS(1),6,___G_printf)
___DEF_SLBL(8,___L8__20_entry)
   ___SET_R1(___SUB(3))
   ___SET_R0(___LBL(9))
   ___JUMPGLOSAFE(___SET_NARGS(1),8,___G_file_2d_exists_3f_)
___DEF_SLBL(9,___L9__20_entry)
   ___IF(___FALSEP(___R1))
   ___GOTO(___L18__20_entry)
   ___END_IF
   ___SET_R1(___SUB(4))
   ___SET_R0(___STK(-3))
   ___POLL(10)
___DEF_SLBL(10,___L10__20_entry)
   ___GOTO(___L16__20_entry)
___DEF_SLBL(11,___L11__20_entry)
   ___IF(___FALSEP(___R1))
   ___GOTO(___L17__20_entry)
   ___END_IF
   ___SET_R1(___SUB(5))
   ___SET_R0(___STK(-3))
   ___POLL(12)
___DEF_SLBL(12,___L12__20_entry)
___DEF_GLBL(___L16__20_entry)
   ___ADJFP(-4)
   ___JUMPGLOSAFE(___SET_NARGS(1),9,___G_load)
___DEF_GLBL(___L17__20_entry)
   ___SET_R0(___LBL(13))
   ___JUMPGLOSAFE(___SET_NARGS(0),7,___G_current_2d_directory)
___DEF_SLBL(13,___L13__20_entry)
   ___SET_R2(___R1)
   ___SET_R3(___SUB(6))
   ___SET_R1(___SUB(7))
   ___SET_R0(___LBL(14))
   ___JUMPGLOSAFE(___SET_NARGS(3),11,___G_string_2d_append)
___DEF_SLBL(14,___L14__20_entry)
   ___SET_R0(___STK(-3))
   ___POLL(15)
___DEF_SLBL(15,___L15__20_entry)
   ___ADJFP(-4)
   ___JUMPGLOSAFE(___SET_NARGS(1),6,___G_printf)
___DEF_GLBL(___L18__20_entry)
   ___SET_R1(___SUB(8))
   ___SET_R0(___LBL(11))
   ___JUMPGLOSAFE(___SET_NARGS(1),8,___G_file_2d_exists_3f_)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H__20_entry_23_0
#undef ___PH_LBL0
#define ___PH_LBL0 18
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R0
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__20_entry_23_0)
___DEF_P_HLBL(___L1__20_entry_23_0)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__20_entry_23_0)
   ___IF_NARGS_EQ(0,___NOTHING)
   ___WRONG_NARGS(0,0,0,0)
___DEF_GLBL(___L__20_entry_23_0)
   ___SET_STK(1,___R0)
   ___SET_R0(___LBL(1))
   ___ADJFP(4)
#define ___NARGS 0
___BEGIN_CFUN(int ___result)
___BEGIN_CFUN_BODY
#undef ___AT_END

UIDevice *h=[[UIDevice alloc] init];
int r = [h hardware];
[h release];
___result = r;
                
#ifndef ___AT_END
#define ___AT_END
#endif
___BEGIN_CFUN_INT_TO_SCMOBJ(___result,___CFUN_RESULT)
___CFUN_SET_RESULT
___END_CFUN_INT_TO_SCMOBJ(___result,___CFUN_RESULT)
___END_CFUN_BODY
___CFUN_ERROR
___END_CFUN
#undef ___NARGS
   ___JUMPPRM(___NOTHING,___R0)
___DEF_SLBL(1,___L1__20_entry_23_0)
   ___ADJFP(-4)
   ___JUMPPRM(___NOTHING,___STK(1))
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H__20_entry_23_1
#undef ___PH_LBL0
#define ___PH_LBL0 21
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R0
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__20_entry_23_1)
___DEF_P_HLBL(___L1__20_entry_23_1)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__20_entry_23_1)
   ___IF_NARGS_EQ(0,___NOTHING)
   ___WRONG_NARGS(0,0,0,0)
___DEF_GLBL(___L__20_entry_23_1)
   ___SET_STK(1,___R0)
   ___SET_R0(___LBL(1))
   ___ADJFP(4)
#define ___NARGS 0
___BEGIN_CFUN(char* ___result)
___BEGIN_CFUN_BODY
#undef ___AT_END

UIDevice *h=[[UIDevice alloc] init];
const char *c = [[h hardwareDescription] UTF8String];
[h release];
___result = (char *) c                        ;
                
#ifndef ___AT_END
#define ___AT_END
#endif
___BEGIN_CFUN_NONNULLCHARSTRING_TO_SCMOBJ(___result,___CFUN_RESULT)
___CFUN_SET_RESULT
___END_CFUN_NONNULLCHARSTRING_TO_SCMOBJ(___result,___CFUN_RESULT)
___END_CFUN_BODY
___CFUN_ERROR
___END_CFUN
#undef ___NARGS
   ___JUMPPRM(___NOTHING,___R0)
___DEF_SLBL(1,___L1__20_entry_23_1)
   ___ADJFP(-4)
   ___JUMPPRM(___NOTHING,___STK(1))
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H__20_entry_23_2
#undef ___PH_LBL0
#define ___PH_LBL0 24
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R0
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__20_entry_23_2)
___DEF_P_HLBL(___L1__20_entry_23_2)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__20_entry_23_2)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L__20_entry_23_2)
   ___SET_STK(1,___R1)
   ___SET_STK(2,___R0)
   ___SET_R0(___LBL(1))
   ___ADJFP(8)
#define ___NARGS 1
___BEGIN_CFUN_VOID
#define ___ARG1 ___CFUN_ARG(1)
___BEGIN_CFUN_ARG(1,char* ___arg1)
___BEGIN_CFUN_SCMOBJ_TO_CHARSTRING(___ARG1,___arg1,1)
___BEGIN_CFUN_BODY_CLEANUP
#undef ___AT_END
printf("%s",___arg1);
#ifndef ___AT_END
#define ___AT_END
#endif
___CFUN_SET_RESULT_VOID
___END_CFUN_BODY_CLEANUP
___END_CFUN_SCMOBJ_TO_CHARSTRING(___ARG1,___arg1,1)
___END_CFUN_ARG(1)
#undef ___ARG1
___CFUN_ERROR_CLEANUP_VOID
___END_CFUN_VOID
#undef ___NARGS
   ___JUMPPRM(___NOTHING,___R0)
___DEF_SLBL(1,___L1__20_entry_23_2)
   ___ADJFP(-8)
   ___JUMPPRM(___NOTHING,___STK(2))
___END_P_SW
___END_P_COD

___END_M_SW
___END_M_COD

___BEGIN_LBL
 ___DEF_LBL_INTRO(___H__20_entry," entry",___REF_FAL,16,0)
,___DEF_LBL_PROC(___H__20_entry,0,-1)
,___DEF_LBL_RET(___H__20_entry,___IFD(___RETI,4,0,0x3f1L))
,___DEF_LBL_RET(___H__20_entry,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H__20_entry,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H__20_entry,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H__20_entry,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H__20_entry,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H__20_entry,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H__20_entry,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H__20_entry,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H__20_entry,___IFD(___RETI,4,4,0x3f0L))
,___DEF_LBL_RET(___H__20_entry,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H__20_entry,___IFD(___RETI,4,4,0x3f0L))
,___DEF_LBL_RET(___H__20_entry,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H__20_entry,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H__20_entry,___IFD(___RETI,4,4,0x3f0L))
,___DEF_LBL_INTRO(___H__20_entry_23_0," entry#0",___REF_FAL,2,0)
,___DEF_LBL_PROC(___H__20_entry_23_0,0,-1)
,___DEF_LBL_RET(___H__20_entry_23_0,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_INTRO(___H__20_entry_23_1," entry#1",___REF_FAL,2,0)
,___DEF_LBL_PROC(___H__20_entry_23_1,0,-1)
,___DEF_LBL_RET(___H__20_entry_23_1,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_INTRO(___H__20_entry_23_2," entry#2",___REF_FAL,2,0)
,___DEF_LBL_PROC(___H__20_entry_23_2,1,-1)
,___DEF_LBL_RET(___H__20_entry_23_2,___IFD(___RETN,2,1,0x3L))
___END_LBL

___BEGIN_MOD_PRM
___DEF_MOD_PRM(0,___G__20_entry,1)
___DEF_MOD_PRM(1,___G__20_entry_23_0,18)
___DEF_MOD_PRM(2,___G__20_entry_23_1,21)
___DEF_MOD_PRM(3,___G__20_entry_23_2,24)
___END_MOD_PRM

___BEGIN_MOD_C_INIT
___END_MOD_C_INIT

___BEGIN_MOD_GLO
___DEF_MOD_GLO(0,___G__20_entry,1)
___DEF_MOD_GLO(1,___G__20_entry_23_0,18)
___DEF_MOD_GLO(2,___G__20_entry_23_1,21)
___DEF_MOD_GLO(3,___G__20_entry_23_2,24)
___END_MOD_GLO

___BEGIN_MOD_SYM_KEY
___DEF_MOD_SYM(0,___S_entry,"entry")
___END_MOD_SYM_KEY

#endif

         TITLE 'GVBXWRPT - SAMPLE WRITE EXIT: GATHER EXIT INFO'         
********************************************************************** 
*                                                                      
* (C) COPYRIGHT IBM CORPORATION 2025.                                  
*     Copyright Contributors to the GenevaERS Project.                 
* SPDX-License-Identifier: Apache-2.0                                  
*                                                                      
********************************************************************** 
*                                                                      
*  Licensed under the Apache License, Version 2.0 (the "License");     
*  you may not use this file except in compliance with the License.    
*  You may obtain a copy of the License at                             
*                                                                      
*     http://www.apache.org/licenses/LICENSE-2.0                       
*                                                                      
*  Unless required by applicable law or agreed to in writing, software 
*  distributed under the License is distributed on an "AS IS" BASIS,   
*  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express        
*  or implied.                                                         
*  See the License for the specific language governing permissions     
*  and limitations under the License.                                  
*                                                                      
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
***********************************************************************
*                                                                     *
*  MODULE DESCRIPTION:                                                *
*                                                                     *
*      - THIS IS A WRITE EXIT THAT GATHERS INFORMATION ABOUT ALL THE  *
*        EXITS LOADED INTO A GENEVAERS PASS AND REPORTS THEIR:        *
*                                                                     *
*        1) LINKAGE EDITOR (BINDER) IDENTIFICATION DATE/TIME          *
*        2) LOAD MODULE SIZE                                          *
*        3) ASSEMBLER/COMPILER USED FOR EACH CSECT AND DATE           *
*                                                                     *
*  CALLED MODULES: IBM BINDER FAST API IEWBFDAT                       *
*                                                                     *
***********************************************************************
                        EJECT                                          
*                                                                      
***********************************************************************
*                                                                     *
*           MODULE RETURN CODES AND REGISTER DOCUMENTATION            *
*                                                                     *
***********************************************************************
*                                                                     *
*                                                                     *
*  RETURN CODES:                                                      *
*                                                                     *
*            0  - FOUND                                               *
*            4  - NOT FOUND                                           *
*            8  - END-OF-FILE                                         *
*           12  - DISABLE   VIEW                                      *
*           16  - ABEND     JOB                                       *
*                                                                     *
*  PARAMETERS:                                                        *
*                                                                     *
*        R1:  PARAMETER LIST ADDRESS                                  *
*                                                                     *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*  REGISTER USAGE:                                                    *
*                                                                     *
*        R15 - TEMPORARY WORK    REGISTER                             *
*            - RETURN    CODE                                         *
*                                                                     *
*        R14 - TEMPORARY WORK    REGISTER                             *
*            - RETURN    ADDR                                         *
*                                                                     *
*        R13 - WORK AREA ADDRESS                                      *
*                                                                     *
*        R12 - PROGRAM    BASE   REGISTER                             *
*        R11 - PROGRAM    BASE   REGISTER                             *
*                                                                     *
*        R10 - SUBROUTINE CALL   RETURN   ADDRESS (1ST LEVEL)         *
*                                                                     *
*        R9  - WORK AREA  ANCHOR ADDRESS                              *
*                                                                     *
*        R8  - PARAMETER  LIST   ADDRESS                              *
*                                                                     *
*        R7  - EVENT FILE AREA   ADDRESS                              *
*        R6  -                                                        *
*        R5  - GENERATED TRANSACTION RECORD ADDRESS                   *
*                                                                     *
*        R4  -                                                        *
*        R3  -                                                        *
*        R2  -                                                        *
*                                                                     *
*        R1  - TEMPORARY WORK    REGISTER                             *
*            - PARAMETER LIST    ADDRESS             (UPON ENTRY)     *
*                                                                     *
*        R0  - TEMPORARY WORK    REGISTER                             *
*                                                                     *
***********************************************************************
*
         COPY GVBX95PA
         GVBAUR35 DSECT=YES
         Copy GVBASSRT
         COPY GVBMR95W
         COPY GVBMR95L
*
***********************************************************************
*                                                                     *
*       "GVBXWRPT" - W O R K A R E A  D E F I N I T I O N             *
*                                                                     *
***********************************************************************
*
WKAREA   DSECT
*
WKSVA    DS  18F                  REGISTER   SAVE AREA
WKSVA2   DS  18F                  REGISTER   SAVE AREA
WKSVA3   DS  18F                  REGISTER   SAVE AREA
WKRETC   DS    F
WKRSNC   DS    F
WKEXIDCB DS    A
WKLIBDCB DS    A
WKIEWBF  DS    A
WKMTOKEN DS    F
WKCURSOR DS    F                  Fast data cursor position. 
WKCOUNT  DS    F                  Number of entries obtained
WKRENTWK DS    XL256
WKREC    DS   0CL125
WKRECTXT DS    CL13              'LOOKUP EXIT: '
WKRECXNM DS    CL8
WKRECRST DS    CL104
         DS    XL7
WKDBL    DS    D                  DOUBLEWORD WORK AREA
         DS    D
WKLTBEGN DS    A
WKLTCNT  DS    F
WKNUMSLT DS    F
WKEMINU1 DS    F                  One minus number slots (bubble sort)
WKBLASTE DS    CL9                Last entry to process
WKSTAT1  DS    X
WKSTAT2  DS    X
         DS    X
*
WKPLIST  DS  32F  
WKXTAB   DS 256XL9                256 user exit entries
WKPGMNM  DS    CL100              Member, path or entry point name
*
WKLENGTH EQU   *-WKAREA
*
***********************************************************************
*                                                                     *
*        REGISTER EQUATES:                                            *
*                                                                     *
***********************************************************************
*                                                                      
         YREGS                                                         
*                                                                      
***********************************************************************
*                                                                     *
*        REGISTER SAVE AREA OFFSETS:                                  *
*                                                                     *
***********************************************************************
*                                                                      
RSABP    EQU   4
RSAFP    EQU   8
RSA14    EQU   12
RSA15    EQU   16
RSA0     EQU   20
RSA1     EQU   24
RSA2     EQU   28
*
GVBXWRPT RMODE ANY
GVBXWRPT AMODE 31
GVBXWRPT CSECT
         J     CODE
XWRPTEYE GVBEYE GVBXWRPT
*
CODE     STM   R14,R12,RSA14(R13) SAVE  CALLER'S  REGISTERS
*
         LA    R10,0(,R15)        SET   PROGRAM   BASE    REGISTER
         LA    R11,4095(,R10)     SET   PROGRAM   BASE    REGISTER
         LA    R11,1(,R11)
         USING GVBXWRPT,R10,R11 --------------------------------------- 
*                                                                      
         LR    R8,R1              LOAD  PARAMETER LIST    ADDRESS      
         USING GENPARM,R8       -------------------------------------- 
***********************************************************************
*  REFERENCE TO CALLING PARAMETERS                                    *
***********************************************************************
         L     R7,GPENVA          LOAD ENVIRONMENT INFO ADDRESS        
         USING GENENV,R7 --------------------------------------------- 
*                                                                      
         LR    R2,R13             PRESERVE CALLER'S RSA ADDRESS        
*                                                                      
***********************************************************************
*  CHECK FOR INIT PHASE. RETURN IF SO.                                *
***********************************************************************
         CLI   GPPHASE,C'I'       INITIALISATION PHASE ?               
         JNE   OPRDCLTE           CONTINUE PROCESSING OP/RD/CL/TE      
*                                                                      
INITPHAS EQU   *                                                       
         LHI   R15,0              SET  RETURN CODE FOR OPEN            
         L     R14,GPRTNCA        LOAD RETURN CODE ADDRESS             
         ST    R15,0(,R14)                                             
         LR    R13,R2             RESTORE  R13                         
         J     RETURNIN           RETURN INIT                          
*                                                                      
OPRDCLTE EQU   *                  OP/RD/CL/TE                          
         L     R9,GPWORKA         LOAD  WORK AREA POINTER ADDRESS      
         L     R12,0(,R9)         LOAD  POINTER   VALUE                
         USING WKAREA,R12 -------------------------------------------- 
         LTR   R12,R12            ALLOCATED  ???                       
         JP    CHAIN              YES - BYPASS ALLOCATION              
*                                                                      
***********************************************************************
*  ALLOCATE "GVBXWRPT" WKAREA   IF NOT ALREADY ALLOCATED (PER "WR")   *
***********************************************************************
WORKALLO EQU   *                                                       
         LHI   R0,WKLENGTH+8       LOAD  WORK AREA SIZE                
*                                                                      
         STORAGE OBTAIN,LENGTH=(0),COND=NO,CHECKZERO=YES               
*                                                                      
         MVC   0(8,R1),WORKEYEB                                        
         LA    R12,8(,R1)                                              
         ST    R12,0(,R9)         SAVE  WORK AREA ADDRESS (POINTER)    
*                                                                      
         CIJE  R15,X'14',ALLCLEAN                                      
         LR    R0,R12             ZERO  WORK AREA                      
         LHI   R1,WKLENGTH                                             
         SR    R14,R14                                                 
         SR    R15,R15                                                 
         MVCL  R0,R14                                                  
*                                                                      
ALLCLEAN EQU   *                                                       
         LA    R13,WKSVA          NEW   SAVE AREA                      
         ST    R13,RSAFP(,R2)     SET   FORWARD  POINTER IN OLD        
         ST    R2,RSABP(,R13)     SET   BACKWARD POINTER IN NEW        
         J     MAINLINE                                                
*                                                                     *
*                                                                      
***********************************************************************
*  CHAIN REGISTER SAVE AREAS TOGETHER                                 *
***********************************************************************
CHAIN    EQU   *                                                       
         LA    R13,WKSVA          NEW   SAVE AREA                      
         ST    R13,RSAFP(,R2)     SET   FORWARD  POINTER IN OLD        
         ST    R2,RSABP(,R13)     SET   BACKWARD POINTER IN NEW        
*                                                                      
***********************************************************************
*  COMMON MAINLINE PROCESSING                                         *
***********************************************************************
MAINLINE EQU   *                                                       
***********************************************************************
*  CHECK FOR OPEN PHASE, RETURN IF SO                                 *
***********************************************************************
         CLI   GPPHASE,C'O'       TEST FOR OPEN PHASE                  
         JE    RETURN0            NOTHING TO DO                        
         CLI   GPPHASE,C'R'       TEST FOR READ PHASE                  
         JE    RETURN0            NOTHING TO DO                        
         CLI   GPPHASE,C'C'       TEST FOR CLOSE PHASE                 
         JNE   RETURN0            NOTHING TO DO                        
*                                                                      
***********************************************************************
*    TERM PHASE PROCESSING
***********************************************************************
*
         CLC   GPTHRDNO,=H'1'
         JNE   RETURN0
*
         TM    WKSTAT1,X'80'
         JO    RETURN0
* 
***********************************************************************
*    LOAD FAST API for BINDER
***********************************************************************
*
         LOAD  EP=IEWBFDAT                   Issue LOAD.              
         ST    R0,WKIEWBF                    Save entry point address
*
***********************************************************************
*    OBTAIN DCB's and OPEN SYSLIB and REPORT FILE
***********************************************************************
*
         LA    R0,LIBDCBL
         GETMAIN RU,LV=(0),LOC=BELOW
         ST    R1,WKLIBDCB
*
         L     R2,WKLIBDCB
         USING IHADCB,R2
         MVC   0(LIBDCBL,R2),LIBDCB
*
         LA    R1,LIBDCBE-LIBDCB(,R2)
         ST    R1,DCBDCBE
*
         MVC   WKRENTWK(8),OPENPARM
         OPEN  ((R2),INPUT),MODE=31,MF=(E,WKRENTWK)
         TM    48(R2),X'10'
         JO    A0040
         MVC   WKRETC,=F'12'
         J     A0900
A0040    EQU    *
         DROP  R2 IHADCB
*
*
         LA    R0,EXIDCBL
         GETMAIN RU,LV=(0),LOC=BELOW
         ST    R1,WKEXIDCB
*
         L     R2,WKEXIDCB
         USING IHADCB,R2
         MVC   0(EXIDCBL,R2),EXIDCB
*
         LA    R1,EXIDCBE-EXIDCB(,R2)
         ST    R1,DCBDCBE
*
         MVC   WKRENTWK(8),OPENPARM
         OPEN  ((R2),OUTPUT),MODE=31,MF=(E,WKRENTWK)
         TM    48(R2),X'10'
         JO    A0042
         MVC   WKRETC,=F'12'
         J     A0900
A0042    EQU    *
         DROP  R2 IHADCB
*
***********************************************************************
*    REFERENCE MAIN THREAD AND BUILD SORTED LIST OF USER EXITS
***********************************************************************
*
         L     R1,GP_THRD_WA
         USING THRDAREA,R1
         L     R1,THRDMAIN
         MVC   WKLTBEGN,LTBEGIN
         MVC   WKLTCNT,LTCOUNT
         DROP  R1 THRDAREA
*
         L     R15,=A(EXITLST)
         BASR  R14,R15
*
***********************************************************************
*    PROCESS USER EXIT LIST TO PRODUCE REPORT (OMITTING DUPLICATES)
***********************************************************************
*
         L     R2,WKNUMSLT        Number slots including duplicates
         LAY   R3,WKXTAB         => first slot
         LTR   R2,R2
         JP    RPTLOOP
*
         MVC   WKREC,SPACEX
         MVC   WKREC,=C'No user exits found'
         LA    R0,WKREC
         L     R1,WKEXIDCB
         PUT   (1),(0)
         MVC   WKRETC,=F'4'
         J     A0890
*
RPTLOOP  EQU   *
         CLC   WKBLASTE,0(R3)   Same as previous slot processed ?
         JE    RPTLP02          (won't be 1st time through)
         CLI   0(R3),C'W'       Write exit
         JE    RPT030
         CLI   0(R3),C'L'       Lookup exit
         JE    RPT040
         CLI   0(R3),C'R'       Read exit
         JE    RPT050
*
         MVC   WKREC,SPACEX
         MVC   WKREC(38),=C'Exit type: X not recognized (XXXXXXXX)'
         MVC   WKREC+11(2),0(R3)
         MVC   WKREC+29(8),1(R3)
         LA    R0,WKREC
         L     R1,WKEXIDCB
         PUT   (1),(0)
         MVC   WKRETC,=F'16'
         J     A0890
*
RPT030   EQU   *
         TM    WKSTAT1,X'01'
         JO    RPT060
         OI    WKSTAT1,X'01'
         MVC   WKREC,SPACEX
         LA    R0,WKREC
         L     R1,WKEXIDCB
         PUT   (1),(0)
         MVC   WKREC(14),=C'Write exit(s):'
         J     RPT054
RPT040   EQU   *
         TM    WKSTAT1,X'02'
         JO    RPT060
         OI    WKSTAT1,X'02'
         MVC   WKREC,SPACEX
         LA    R0,WKREC
         L     R1,WKEXIDCB
         PUT   (1),(0)
         MVC   WKREC(15),=C'Lookup exit(s):'
         J     RPT054
RPT050   EQU   *
         TM    WKSTAT1,X'04'
         JO    RPT060
         OI    WKSTAT1,X'04'
         MVC   WKREC,SPACEX
         LA    R0,WKREC
         L     R1,WKEXIDCB
         PUT   (1),(0)
         MVC   WKREC(13),=C'Read exit(s):'
RPT054   EQU   *
         LA    R0,WKREC
         L     R1,WKEXIDCB
         PUT   (1),(0)
         MVC   WKREC,SPACEX
         MVC   WKREC(66),=C'Module             Component        VRL    X
                  Date       Time        Size'
         LA    R0,WKREC
         L     R1,WKEXIDCB
         PUT   (1),(0)
         MVC   WKREC,SPACEX
         MVC   WKREC(66),=C'--------           ----------       -----  X
                  -------    ----------  --------' 
         LA    R0,WKREC
         L     R1,WKEXIDCB
         PUT   (1),(0)
         MVC   WKREC,SPACEX
         LA    R0,WKREC
         L     R1,WKEXIDCB
         PUT   (1),(0)
*
*   call routine with R3 => desired slot
RPT060   EQU   *
         L     R15,=A(EXITRPT)
         BASR  R14,R15
*
RPTLP02  EQU   *
         MVC   WKBLASTE,0(R3)    Keep last entry processed for ref
         LA    R3,9(,R3) 
         BRCT  R2,RPTLOOP
*
         OI    WKSTAT1,X'80'     Indicates report completed
***********************************************************************
*    CLOSE REPORT AND SYSLIB FILES
***********************************************************************
*
A0890    EQU   *
         L     R2,WKEXIDCB
         MVC   WKRENTWK(8),OPENPARM
         CLOSE ((R2)),MODE=31,MF=(E,WKRENTWK)
*
         L     R2,WKLIBDCB
         MVC   WKRENTWK(8),OPENPARM
         CLOSE ((R2)),MODE=31,MF=(E,WKRENTWK)
*
***********************************************************************
*    TERMINATION - free DCB's
***********************************************************************
*
A0900    EQU   *
         LA    R0,EXIDCBL
         L     R1,WKEXIDCB
         FREEMAIN RU,LV=(0),A=(1)
*
         LA    R0,LIBDCBL
         L     R1,WKLIBDCB
         FREEMAIN RU,LV=(0),A=(1)
*
A0990    EQU   *
         LHI   R15,0              SET  RETURN CODE
         J     RETURN
*
***********************************************************************
*  RETURN TO CALLER (GVBMR95)                                         *
***********************************************************************
RETURN0  EQU   *                                                       
         XGR   R15,R15            ZERO RETURN CODE                     
         J     RETURN                                                  
*                                                                      
RETURN8  EQU   *                                                       
         LHI   R15,8              ZERO RETURN CODE                     
*                                                                      
RETURN   EQU   *                                                       
         L     R14,GPRTNCA        LOAD RETURN CODE  ADDRESS            
         ST    R15,0(,R14)                                             
         L     R13,RSABP(,R13)    RESTORE REGISTER  R13                
*                                                                      
RETURNIN EQU   *                  RETURN INITIALIZATION                
         L     R14,RSA14(,R13)    RESTORE REGISTER  R14                
         LM    R0,R12,RSA0(R13)   RESTORE REGISTERS R0 - R12           
         BSM   0,R14              RETURN                               
*                                                                      
         DROP  R8                                                      
*
***********************************************************************
*  EXITLST: GET LIST FOR REPORT OF GENERAERS EXTRACT PHASE USER EXITS *
*           AND SORT LIST OF EXITS, LEAVING DUPLICATES IN LIST.       *
***********************************************************************
EXITLST  DS    0H
         STM   R14,R12,12(R13)
         LA    R2,WKSVA2
         ST    R2,RSAFP(,R13)     SET   FORWARD  POINTER IN OLD
         ST    R13,RSABP(,R2)     SET   BACKWARD POINTER IN NEW
         LR    R13,R2             NEW   SAVE AREA
*
         LLGT  R8,WKLTBEGN        LOAD LOGIC  TABLE ADDRESS
         USING LOGICTBL,R8
*
         LAY   R3,WKXTAB
         LGF   R7,WKLTCNT         LOAD LOGIC  TABLE ENTRY COUNT
LTLOOP   EQU   *
         CLC   LTFUNC(2),=CL2'WR'
         JNE   LT0100
         CLC   LTWRNAME,SPACEX    EXIT SPECIFIED ???
         JE    LT0990             no, go
*      WR_EX processin
         MVI   0(R3),C'W'      
         MVC   1(8,R3),LTWRNAME
         J     LT0900
*
LT0100   EQU   *
         CLC   LTFUNC,=CL4'LUEX'
         JNE   LT0200
         CLC   LTLUNAME,SPACEX    EXIT SPECIFIED ???
         JE    LT0990             no, go
*      LU_EX processing
         MVI   0(R3),C'L'      
         MVC   1(8,R3),LTLUNAME
         J     LT0900
*
LT0200   EQU   *
         CLC   LTFUNC(2),=CL2'RE'
         JNE   LT0990
         CLC   LTRENAME,SPACEX
         JE    LT0990
*      RE_EX processing
         MVI   0(R3),C'R'      
         MVC   1(8,R3),LTRENAME
*
LT0900  EQU   *                   Write exit information
         LA    R3,9(,R3)          Next tab entry
*
LT0990  EQU   *
         LGH   R0,LTROWLEN
         AGR   R8,R0              ADVANCE TO NEXT ROW (31-BIT ADDR)
         BRCT  R7,LTLOOP
         DROP  R8 LOGICTBL
*
*   Sort the table, leavingany duplicates included
*
         LR    R4,R3
         LAY   R3,WKXTAB
         SR    R4,R3              R4 == LENGTH OF FILLED SLOTS
         XR    R0,R0
         LR    R1,R4
         D     R0,=F'9'           R1 == QUOTIENT
         ST    R1,WKNUMSLT
         C     R1,=F'1'           More than one entry ?
         JNH   BUBBLEX            No, don't do anything
         BCTR  R1,0               ONE LESS THAN NUMBER OF ENTRIES
         ST    R1,WKEMINU1        KEEP THIS !!
         LAY   R3,WKXTAB
*
*        R1 == one less than number of entries, R3 => table
BUBBLELO EQU   *               --Loop (outer loop)
         XR    R0,R0              RESET swap indicator
*
BUBBLELP EQU   *               --Loop (inner loop)
         CLC   0(9,R3),9(R3)
         JNH   BUBBLE02           Equal or less than next slot
*                                 Greater than next slot
BUBBLE01 EQU   *
         LA    R0,1               Indicate swap
         XC    0(9,R3),9(R3)
         XC    9(9,R3),0(R3)
         XC    0(9,R3),9(R3)
BUBBLE02 EQU   *
         LA    R3,9(,R3)          NEXT SLOT
         BRCT  R1,BUBBLELP
*
         L     R1,WKEMINU1        Number slots minus one
         LAY   R3,WKXTAB
         LTR   R0,R0              Any swaps ?
         JNZ   BUBBLELO           Yes, do it again
BUBBLEX  EQU   *
*
         L     R13,RSABP(,R13)    OLD   SAVE AREA                       
         LM    R14,R12,12(R13)                                          
         BR    R14                                                      
*
***********************************************************************
*  EXITRPT: REPORT OF GENERAERS EXTRACT PHASE USER EXITS FROM LIST    *
*           R3=> current exit table entry                             *
***********************************************************************
EXITRPT  DS    0H
         STM   R14,R12,12(R13)
         LA    R2,WKSVA2
         ST    R2,RSAFP(,R13)     SET   FORWARD  POINTER IN OLD
         ST    R13,RSABP(,R2)     SET   BACKWARD POINTER IN NEW
         LR    R13,R2             NEW   SAVE AREA
*
*********************************************************************** 
* Process SB - Start session with a BLDL identifier - the issue RC    * 
*********************************************************************** 
         MVC   WKPGMNM(8),1(R3)               Get member name           
         XC    WKMTOKEN,WKMTOKEN              Zero out MTOKEN           
         L     R15,WKIEWBF
         L     R2,WKLIBDCB
         CALL (15),(SB,WKMTOKEN,(R2),WKPGMNM),VL,                      *
               MF=(E,WKPLIST)                 Call fast data API            
*
*   Call RC
         L     R15,WKIEWBF                                                 
         CALL (15),(RC,WKMTOKEN,WKRETC,WKRSNC),VL,                     *
               MF=(E,WKPLIST)                 Call fast data API
*
         CLC   WKRETC,=XL4'00000000'
         JNE   SB_001
         CLC   WKRSNC,=XL4'00000000'
         JE    SB_002
*
SB_001   EQU   *
         MVC   WKREC,SPACEX
         MVC   WKREC(L'MSG_RC),MSG_RC         Build RC message
         LAY   R15,FORMAT_HEX
         CALL (15),(WKREC+4,WKRETC),                                   *
               MF=(E,WKPLIST)                 format return
         LAY   R15,FORMAT_HEX
         CALL (15),(WKREC+17,WKRSNC),                                  *
               MF=(E,WKPLIST)                 and reason codes
         LA    R0,WKREC
         L     R1,WKEXIDCB
         PUT   (1),(0)
         MVC   WKRETC,=F'8'
         J     GI_090
*
SB_002   EQU   *
***********************************************************************
* Process GB - get B_IDRB (module) data                               *
***********************************************************************
IEWBIDB_BASE EQU R2                      Base register for BIDB buffer
IDB_BASE     EQU R3                      Base register for BIDB entry
         IEWBUFF FUNC=GETBUF,TYPE=IDRB   Get memory for BIDB buffer
         IEWBUFF FUNC=INITBUF,TYPE=IDRB  Init IDB buffer
         XC    WKCURSOR,WKCURSOR         Zero out cursor
GB_LOOP  EQU   *
         L     R15,WKIEWBF
         CALL (15),(GD,WKMTOKEN,B_BIDB_VSTRING,0,(IEWBIDB_BASE),       *
               WKCURSOR,WKCOUNT,0),VL,MF=(E,WKPLIST) Call fast data
         ST    R15,WKRETC                  Save return
         ST    R0,WKRSNC                   and reason codes
*
         CLC   WKRETC,=F'4'
         JNE   GB_BADRC                    We want only RETCODE=4
         CLC   WKRSNC,=XL4'10800001'       and RSNCODE='10800001'X
         JE    GB_OK                         (more data)
         CLC   WKRSNC,=XL4'10800002'       or RSNCODE='10800002'X
         JE    GB_OK                         (no more data)
GB_BADRC EQU   *                           Other codes are invalid
         MVC   WKREC,SPACEX
         MVC   WKREC(L'MSG_RC),MSG_RC      Build RC message
         LAY   R15,FORMAT_HEX
         CALL (15),(WKREC+4,WKRETC),                                   *
               MF=(E,WKPLIST)              format return
         LAY   R15,FORMAT_HEX
         CALL (15),(WKREC+17,WKRSNC),                                  *
               MF=(E,WKPLIST)              and reason codes
*
         LA    R0,WKREC
         L     R1,WKEXIDCB
         PUT   (1),(0)
         MVC   WKRETC,=F'8'
         J     FREE_BIDB                   Free buffer and
*                                          read the next command
GB_OK    EQU   *
         CLC   WKRSNC,=XL4'10800001'
         JE    GB_LOOP                     If there are more entries
*                                          call fast data again
         MVC   WKREC,SPACEX
         MVC   WKREC(L'MSG_RC),MSG_GB      Build GB message
         MVC   WKREC+00(8),WKPGMNM
         MVC   WKREC+19(10),IDB_BINDER_ID
         MVC   WKREC+36(2),IDB_VERSION
         MVI   WKREC+38,C'.'
         MVC   WKREC+39(2),IDB_RELEASE
         MVC   WKREC+46(4),IDB_DATE_BOUND
         MVI   WKREC+50,C'.'
         MVC   WKREC+51(3),IDB_DATE_BOUND+4
         MVC   WKREC+57(2),IDB_TIME_BOUND
         MVI   WKREC+59,C':'
         MVC   WKREC+60(2),IDB_TIME_BOUND+2
         MVI   WKREC+62,C':'
         MVC   WKREC+63(2),IDB_TIME_BOUND+4
         LAY   R15,FORMAT_HEX
         CALL (15),(WKREC+68,IDB_MODULE_SIZE),                         *
               MF=(E,WKPLIST)              format return
         LA    R0,WKREC
         L     R1,WKEXIDCB
         PUT   (1),(0)
FREE_BIDB EQU  *                                                         
         IEWBUFF FUNC=FREEBUF,TYPE=IDRB    Free IDT buffer.              
*
*   Call RC
         L     R15,WKIEWBF
         CALL (15),(RC,WKMTOKEN,WKRETC,WKRSNC),VL,                     *
               MF=(E,WKPLIST)                 Call fast data API
*
         CLC   WKRETC,=F'4'
         JNE   GB_001                      We want only RETCODE=4
         CLC   WKRSNC,=XL4'10800002'       or RSNCODE='10800002'X
         JE    GB_002                        (no more data)
GB_001   EQU   *
         MVC   WKREC,SPACEX
         MVC   WKREC(L'MSG_RC),MSG_RC         Build RC message
         LAY   R15,FORMAT_HEX
         CALL (15),(WKREC+4,WKRETC),                                   *
               MF=(E,WKPLIST)                 format return
         LAY   R15,FORMAT_HEX
         CALL (15),(WKREC+17,WKRSNC),                                  *
               MF=(E,WKPLIST)                 and reason codes
*
         LA    R0,WKREC
         L     R1,WKEXIDCB
         PUT   (1),(0)
         MVC   WKRETC,=F'8'
*
GB_002   EQU   *
***********************************************************************
* Process GI - get B_IDRL (csect) data                                *
***********************************************************************
*
IEWBIDL_BASE EQU R2                      Base register for BIDB buffer
IDL_BASE     EQU R3                      Base register for BIDB entry
         IEWBUFF FUNC=GETBUF,TYPE=IDRL   Get memory for BIDB buffer
         IEWBUFF FUNC=INITBUF,TYPE=IDRL  Init IDB buffer
         XC    WKCURSOR,WKCURSOR         Zero out cursor
GI_LOOP  EQU   *
         L     R15,WKIEWBF
         CALL (15),(GD,WKMTOKEN,B_BIDL_VSTRING,0,(IEWBIDL_BASE),       *
               WKCURSOR,WKCOUNT,0),VL,MF=(E,WKPLIST) Call fast data
         ST    R15,WKRETC                  Save return
         ST    R0,WKRSNC                   and reason codes
*
         CLC   WKRETC,=F'4'
         JNE   GI_BADRC                    We want only RETCODE=4
         CLC   WKRSNC,=XL4'10800001'       and RSNCODE='10800001'X
         JE    GI_OK                       (more data)
         CLC   WKRSNC,=XL4'10800002'       or RSNCODE='10800002'X
         JE    GI_OK                       (no more data)
GI_BADRC EQU   *                           Other codes are invalid
         MVC   WKREC,SPACEX
         MVC   WKREC(L'MSG_RC),MSG_RC      Build RC message
         LAY   R15,FORMAT_HEX
         CALL (15),(WKREC+4,WKRETC),                                   *
               MF=(E,WKPLIST)              format return
         LAY   R15,FORMAT_HEX
         CALL (15),(WKREC+17,WKRSNC),                                  *
               MF=(E,WKPLIST)              and reason codes
*
         LA    R0,WKREC
         L     R1,WKEXIDCB
         PUT   (1),(0)
         J     FREE_BIDL                   Free buffer
*
GI_OK    EQU   *
         L     R5,IDLH_ENTRY_COUNT
IDL_ENTRY_LOOP EQU *
         CLC   IDL_PID_ID(4),=XL4'00000000'
         JE    GI_IDL_010
         MVC   WKREC,SPACEX
         MVC   WKREC(L'MSG_RC),MSG_GI      Build GI message
         L     R15,IDL_RESIDENT_PTR
         LH    R14,IDL_RESIDENT_CHARS
         BCTR  R14,0
         EX    R14,GI_MVC
         MVC   WKREC+19(10),IDL_PID_ID
         MVC   WKREC+36(2),IDL_VERSION
         MVI   WKREC+38,C'.'
         MVC   WKREC+39(2),IDL_MOD_LEVEL
         MVC   WKREC+46(4),IDL_DATE_PROCESSED
         MVI   WKREC+50,C'.'
         MVC   WKREC+51(3),IDL_DATE_PROCESSED+4
*         MVC   WKREC+57(9),IDL_TIME_PROCESSED
         LA    R0,WKREC
         L     R1,WKEXIDCB
         PUT   (1),(0)                     Print GD message
*
         BCTR  R5,0                        One less
         A     R3,IDLH_ENTRY_LENG        ->Next entry
         L     R0,IDLH_BUFFER_LENG
         AR    R0,R2                     ->End of buffer
         CR    R3,R0                       Past end of buffer
         JNL   GI_IDL_010                  - Exit loop
         LTR   R5,R5                       More to go
         JP    IDL_ENTRY_LOOP              - Loop back
*
GI_IDL_010 EQU *
         MVC   WKREC,SPACEX
         LA    R0,WKREC
         L     R1,WKEXIDCB
         PUT   (1),(0)                     Print GD message
*
         CLC   WKRSNC,=XL4'10800001'       If more entries
         JE    GI_LOOP                     then
*
FREE_BIDL EQU  *
         IEWBUFF FUNC=FREEBUF,TYPE=IDRL    Free IDRL buffer
*
*   Call RC
         L     R15,WKIEWBF
         CALL (15),(RC,WKMTOKEN,WKRETC,WKRSNC),VL,                     *
               MF=(E,WKPLIST)                 Call fast data API
*
         CLC   WKRETC,=F'4'
         JNE   GI_001                      We want only RETCODE=4
         CLC   WKRSNC,=XL4'10800002'       or RSNCODE='10800002'X
         JE    GI_002                        (no more data)
GI_001   EQU   *
         MVC   WKREC,SPACEX
         MVC   WKREC(L'MSG_RC),MSG_RC         Build RC message
         LAY   R15,FORMAT_HEX
         CALL (15),(WKREC+4,WKRETC),                                   *
               MF=(E,WKPLIST)                 format return
         LAY   R15,FORMAT_HEX
         CALL (15),(WKREC+17,WKRSNC),                                  *
               MF=(E,WKPLIST)                 and reason codes
*
         LA    R0,WKREC
         L     R1,WKEXIDCB
         PUT   (1),(0)
GI_002   EQU   *
*
*
GI_090   EQU   *
         L     R13,RSABP(,R13)    OLD   SAVE AREA
         LM    R14,R12,12(R13)
         BR    R14
*
*********************************************************************** 
MVCR5R14 MVC   0(0,R5),0(R14)     * * * * E X E C U T E D * * * *
GI_MVC   MVC   WKREC+0(0),0(R15)           MVC template
*
***********************************************************************
* Format hex value                                                    *
*********************************************************************** 
FORMAT_HEX DS 0H                                                       
         STM   R14,R12,12(R13)    Save registers
*
         LM    R2,R3,0(R1)        Arguments 1 and 2
         UNPK  WKDBL(9),0(5,R3)
         TR    WKDBL(8),HEXTAB
         MVC   0(8,R2),WKDBL
*
         LM    R14,R12,12(R13)
         BR    R14
*********************************************************************** 
*                                                                     * 
*        C O N S T A N T S                                            * 
*                                                                     * 
*********************************************************************** 
         DS   0D                                                        
MODE31   DS   0XL4                                                      
OPENPARM DC    XL8'8000000000000000'
*
***********************************************************************
* B_TEXT class name represented as vstring                            *
***********************************************************************
B_TEXT_VSTRING        DC H'6',C'B_TEXT'
B_BIDB_VSTRING        DC H'6',C'B_IDRB'
B_BIDL_VSTRING        DC H'6',C'B_IDRL'
***********************************************************************
* Fast data access request codes                                      *
***********************************************************************
SB       DC    C'SB',X'0001'                                              
SJ       DC    C'SJ',X'0001'                                              
SQ       DC    C'SQ',X'0001'                                              
GC       DC    C'GC',X'0001'                                              
GD       DC    C'GD',X'0001'                                              
GE       DC    C'GE',X'0001'                                              
GN       DC    C'GN',X'0001'                                              
RC       DC    C'RC',X'0001'                                              
EN       DC    C'EN',X'0001'                                              
***********************************************************************
* GN call types                                                       *
***********************************************************************
NTYPE_SECTIONS        DC C'S'
NTYPE_CLASSES         DC C'C'
***********************************************************************
* Fast data mappings and buffer templates                             *
***********************************************************************
ESDBUF   IEWBUFF FUNC=MAPBUF,TYPE=ESD,VERSION=6,SIZE=8                 
CUIBUF   IEWBUFF FUNC=MAPBUF,TYPE=CUI,VERSION=6,BYTES=40960            
NAMBUF   IEWBUFF FUNC=MAPBUF,TYPE=NAME,VERSION=6,SIZE=8                
TXTBUF   IEWBUFF FUNC=MAPBUF,TYPE=TEXT,VERSION=6,BYTES=2048            
IDRBUF   IEWBUFF FUNC=MAPBUF,TYPE=IDRB,VERSION=6,BYTES=4072            
IDLBUF   IEWBUFF FUNC=MAPBUF,TYPE=IDRL,VERSION=7,BYTES=40960           
***********************************************************************
* Hexadecimal characters                                              *
***********************************************************************
HEXCHARS              DC C'0123456789ABCDEF'
*
WORKEYEB DC    CL8'GVBXWRPT'
*
EXIDCB   DCB   DSORG=PS,DDNAME=EXTRXRPT,MACRF=(PM),DCBE=EXIDCBE,       X
               RECFM=FB,LRECL=125
EXIDCBE  DCBE  RMODE31=BUFF
EXIDCBL  EQU   *-EXIDCB
*
LIBDCB   DCB   DSORG=PO,DDNAME=SYSLIB,MACRF=R,DCBE=LIBDCBE,            X
               RECFM=U
LIBDCBE  DCBE  RMODE31=BUFF
LIBDCBL  EQU   *-LIBDCB
*
*********************************************************************** 
* Messages                                                            * 
*********************************************************************** 
MSG_STARTUP           DC C'Z/OS BINDER FAST DATA API DEMO'              
MSG_SYSIN_FAILED      DC C'COULD NOT OPEN SYSIN'                        
MSG_SYSLIB_FAILED     DC C'COULD NOT OPEN SYSLIB'                       
MSG_INVALID_COMMAND   DC C'INVALID COMMAND'                             
MSG_RC                DC C'RET=12345678 RSN=12345678'                   
MSG_ALL_OK            DC C'ALL OK'                                      
MSG_MTOKEN            DC C'MTOKEN=12345678'                             
MSG_GE                DC C'ESD TYPE=12 NAME='                           
MSG_ECHO_PREFIX       DC C'* '                                          
MSG_CSVQUERY_FAILED   DC C'CSVQUERY FAILED'                             
MSG_GN                DC C'NAME='                                       
MSG_GC                DC C'MEMBER='                                     
MSG_GI                DC C'X        BUILT BY: XXXXXXXXXX VERS: XXXX ON:+
                XXXXXXX AT: XXXXXXXXX'                                  
MSG_GD                DC C'B_TEXT DATA SIZE='                           
MSG_GB                DC C'X        BOUND BY: XXXXXXXXXX VERS: XXXX ON:+
                XXXXXXX AT: XXXXXX'                                     
SPACEX   DC    CL256' '
*
HEXTAB   DS   0F
         ORG   *+240
         DC    CL16'0123456789ABCDEF'
*
         LTORG ,       
         DS   0F       
         DCBD  DSORG=PS
*                      
         IHADCBE       
*                      
*                      
         END           

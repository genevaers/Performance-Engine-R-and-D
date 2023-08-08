         TITLE 'GVBXP01 - LOGICAL RECORD "PASTE-UP" ROUTINE'
**********************************************************************
*
* (C) COPYRIGHT IBM CORPORATION 2005, 2021.
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
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*  GVBXP01  - RETURNS THE ADDRESS OF THE LOGICAL RECORD CORRESPONDING *
*             TO  THE PROVIDED KEY.  A RETURN CODE INDICATES  IF  A   *
*             MATCHING RECORD WAS FOUND OR NOT.                       *
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
*        R15 - TEMPORARY WORK REGISTER                                *
*            - RETURN    CODE                                         *
*                                                                     *
*        R14 - TEMPORARY WORK REGISTER                                *
*            - RETURN    ADDR                                         *
*                                                                     *
*        R13 - WORK AREA ADDRESS                                      *
*                                                                     *
*        R12 -                                                        *
*        R11 - PROGRAM   BASE REGISTER                                *
*                                                                     *
*        R10 -                                                        *
*        R9  -                                                        *
*                                                                     *
*        R8  - GENEVA    PARAMETER   LIST     ADDRESS                 *
*                                                                     *
*        R7  - EXTRACT   RECORD      ADDRESS                          *
*        R6  - INPUT     RECORD      ADDRESS                          *
*                                                                     *
*        R5  - COMPARE   INSTRUCTION ADDRESS                          *
*        R4  - KEY  PARAMETER   PAIR ADDRESS                          *
*                                                                     *
*        R3  - CURRENT   LOOK-UP KEY FIELD    ADDRESS                 *
*        R2  -                                                        *
*                                                                     *
*        R1  - TEMPORARY WORK        REGISTER                         *
*            - PARAMETER LIST        ADDRESS         (UPON ENTRY)     *
*                                                                     *
*        R0  - TEMPORARY WORK        REGISTER                         *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*     "GVBXP01" - W O R K A R E A   D E F I N I T I O N               *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
WORKAREA DSECT
*
SAVEAREA DS  18F                  REGISTER   SAVE AREA
*
DBLWORK  DS    D                  DOUBLEWORD WORK AREA
*
KEYCNT   DS    HL02               KEY       COUNT
PARMLEN  DS    HL02               PARAMETER STRING LEN
*
KEY01CLC DS    XL06               KEY   1  COMPARE
KEY01AHI DS    XL04               KEY   1  ADVANCE
KEY02CLC DS    XL06               KEY   2  COMPARE
KEY02AHI DS    XL04               KEY   2  ADVANCE
KEY03CLC DS    XL06               KEY   3  COMPARE
KEY03AHI DS    XL04               KEY   3  ADVANCE
KEY04CLC DS    XL06               KEY   4  COMPARE
KEY04AHI DS    XL04               KEY   4  ADVANCE
KEY05CLC DS    XL06               KEY   5  COMPARE
KEY05AHI DS    XL04               KEY   5  ADVANCE
KEY06CLC DS    XL06               KEY   6  COMPARE
KEY06AHI DS    XL04               KEY   6  ADVANCE
KEY07CLC DS    XL06               KEY   7  COMPARE
KEY07AHI DS    XL04               KEY   7  ADVANCE
KEY08CLC DS    XL06               KEY   8  COMPARE
KEY08AHI DS    XL04               KEY   8  ADVANCE
KEY09CLC DS    XL06               KEY   9  COMPARE
KEY09AHI DS    XL04               KEY   9  ADVANCE
KEY10CLC DS    XL06               KEY  10  COMPARE
KEY10AHI DS    XL04               KEY  10  ADVANCE
*
SAVEPARM DS    CL256              STARTUP  PARAMETER  STRING
*
UR33PARM DS   0A                  GVBUR33  PARAMETER  LIST
         DS    A                  STRING              ADDRESS
         DS    A                  STRING   LENGTHADDRESS
         DS    A                  ENVIRON  VARIABLE   POINTER ADDRESS
*
UR20PARM DS    A                  GVBUR20  PARAMETER  LIST
*
UR20P1   GVBUR20P PRE=UR20,DSECT=N
*UR20FC   DS    HL02               FUNCTION CODE
*UR20RC   DS    HL02               RETURN   CODE
*UR20ERRC DS    HL02               ERROR    CODE
*UR20RECL DS    HL02               RECORD   LENGTH
*UR20RECA DS    AL04               RECORD   AREA      ADDRESS
*UR20RBN  DS    FL04               RELATIVE BLOCK     NUMBER
*UR20DDN  DS    CL08               FILE     DDNAME
*UR20OPT1 DS    CL01              I/O MODE(I=INPUT, O=OUTPUT, D=DIRECT)
*UR20OPT2 DS    CL01
*UR20NBUF DS    HL02               NUMBER   OF I/O    BUFFERS
*UR20WPTR DS    AL04               WORK     AREA      POINTER
*
ABNDFLAG DS    CL01               OPEN ABEND         FLAG
EOFFLAG  DS    CL01               "END-OF-FILE"      FLAG
*
PRNTLEN  DS    HL02               DISPLAY  TEXT   LENGTH "WTO"
PRNTLINE DS   0CL133              CONTROL  REPORT PRINT  LINE
PRNTCC   DS    CL001              CARRIAGE CONTROL
PRNTTEXT DS    CL132              PRINT    TEXT
*
WTOPARM  WTO   TEXT=(R2),MF=L
WTOPARML EQU   *-WTOPARM
*
WORKLEN  EQU   *-WORKAREA
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        GENEVA PROVIDED PARAMETER LIST                               *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         COPY GVBX95PA
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        LOOK-UP EXIT STARTUP PARAMETER DATA                          *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
PARMDATA DSECT
*
STARTUP  DS   0CL32               GENEVA  STARTUP  PARAMETER STRING
PDDDNAME DS    CL08               PHYSICAL FILE    DDNAME
         DS    CL01
PDOPENAB DS    CL01               OPEN    ABEND    INDICATOR
         DS    CL01
PDKEYP01 DS    CL03               KEY     POSITION
         DS    CL01
PDKEYL01 DS    CL03               KEY     LENGTH
         DS    CL01
PDKEYP02 DS    CL03               KEY     POSITION
         DS    CL01
PDKEYL02 DS    CL03               KEY     LENGTH
         DS    CL01
PDKEYP03 DS    CL03               KEY     POSITION
         DS    CL01
PDKEYL03 DS    CL03               KEY     LENGTH
         DS    CL01
PDKEYP04 DS    CL03               KEY     POSITION
         DS    CL01
PDKEYL04 DS    CL03               KEY     LENGTH
         DS    CL01
PDKEYP05 DS    CL03               KEY     POSITION
         DS    CL01
PDKEYL05 DS    CL03               KEY     LENGTH
         DS    CL01
PDKEYP06 DS    CL03               KEY     POSITION
         DS    CL01
PDKEYL06 DS    CL03               KEY     LENGTH
         DS    CL01
PDKEYP07 DS    CL03               KEY     POSITION
         DS    CL01
PDKEYL07 DS    CL03               KEY     LENGTH
         DS    CL01
PDKEYP08 DS    CL03               KEY     POSITION
         DS    CL01
PDKEYL08 DS    CL03               KEY     LENGTH
         DS    CL01
PDKEYP09 DS    CL03               KEY     POSITION
         DS    CL01
PDKEYL09 DS    CL03               KEY     LENGTH
         DS    CL01
PDKEYP10 DS    CL03               KEY     POSITION
         DS    CL01
PDKEYL10 DS    CL03               KEY     LENGTH
                        SPACE 5
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        REGISTER EQUATES:                                            *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
                        SPACE 3
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        REGISTER SAVE AREA OFFSETS:                                  *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
RSABP    EQU   4
RSAFP    EQU   8
RSA14    EQU   12
RSA15    EQU   16
RSA0     EQU   20
RSA1     EQU   24
*
KEYMAX   EQU   10
*
                        EJECT
         PRINT NOGEN
*
GVBXP01  RMODE 31
GVBXP01  AMODE 31
GVBXP01  CSECT
         J     CODE
XP01EYE  GVBEYE GVBXP01
*
CODE     STM   R14,R12,RSA14(R13) SAVE  CALLER'S  REGISTERS
*
         LR    R11,R15            SET   PROGRAM   BASE  REGISTERS
         USING GVBXP01,R11
*
         LR    R8,R1              LOAD  PARAMETER LIST  ADDRESS
         USING GENPARM,R8
*
         LLGT  R6,GPEVENTA        Load pointer to Event Record address
         LG    R6,0(,R6)          Load Input Record address
         L     R7,GPEXTRA         LOAD  EXTRACT RECORD  ADDRESS
                        SPACE 3
         LR    R10,R13            SAVE  CALLER'S  RSA     ADDRESS
*
***********************************************************************
*  CHECK FOR CLOSE PHASE, RETURN RC=0 IF SO                           *
***********************************************************************
         L     R14,GPENVA         LOAD ENVIRONMENT INFO ADDRESS
         USING GENENV,R14
         CLI   GPPHASE,C'C'       TEST FOR CLOSE  PHASE
         BRE   CLOSE              CONTINUE PROCESSING
*
         L     R2,GPWORKA         LOAD  WORK AREA POINTER ADDRESS
         L     R13,0(,R2)         LOAD  POINTER   VALUE
         USING WORKAREA,R13
         LTR   R13,R13            ALLOCATED  ???
         BRP   CHAIN              YES - BYPASS ALLOCATION
*
***********************************************************************
*  CHECK FOR OPEN PHASE, RETURN IF SO                                 *
***********************************************************************
         CLI   GPPHASE,C'O'       TEST FOR OPEN PHASE
         BRNE  WORKALLO           CONTINUE PROCESSING
         DROP  R14
*
CLOSE    SR    R15,R15            ZERO RETURN   CODE
         L     R14,GPRTNCA        LOAD RETURN CODE  ADDRESS
         ST    R15,0(,R14)
*
         LR    R13,R10            RESTORE  R13
         B     RETURNOP           RETURN
*
***********************************************************************
*  ALLOCATE "GVBXP01" WORKAREA IF NOT ALREADY ALLOCATED               *
***********************************************************************
WORKALLO LHI   R0,WORKLEN         LOAD  WORK AREA SIZE
         GETMAIN R,LV=(0),LOC=(ANY)
         LR    R13,R1
         ST    R13,0(,R2)         SAVE  WORK AREA ADDRESS (POINTER)
*
         LR    R0,R13             ZERO  WORK AREA
         LHI   R1,WORKLEN
         SR    R14,R14
         SR    R15,R15
         MVCL  R0,R14
*
         ST    R13,RSAFP(,R10)    SET   FORWARD  POINTER IN OLD
         ST    R10,RSABP(,R13)    SET   BACKWARD POINTER IN NEW
*                                                                     *
*        If SNAP is required, use SNAP=Y so that MR95 allocates and   *
*        opens the file.                                              *
*        The SNAPDCB address is saved in SNAPDCBA                     *
*                                                                     *
         BRAS  R10,INITWORK       INITIALIZE WORK AREA
*
*        LA    R9,40-1(,R8)
*          ly  R2,SNAPDCBA
*        SNAP  DCB=(R2),PDATA=(REGS),ID=100,STORAGE=((R8),(R9))
*
         B     READLOOP
                        EJECT
CHAIN    ST    R13,RSAFP(,R10)    SET   FORWARD  POINTER IN OLD
         ST    R10,RSABP(,R13)    SET   BACKWARD POINTER IN NEW
*
*        LA    R9,40-1(,R8)
*          ly  R2,SNAPDCBA
*        SNAP  DCB=(R2),PDATA=(REGS),ID=111,STORAGE=((R8),(R9))
*
READLOOP CLI   EOFFLAG,C'Y'       END-OF-FILE ???
         BRE   NOTFOUND           YES - NOT FOUND
*
         Llgt  R1,UR20RECA        LOAD CURRENT RECORD ADDRESS
*
         L     R3,GPKEYA          LOAD KEY ADDRESS
         AHI   R3,4               SKIP LR ID
*
         LA    R5,KEY01CLC        LOAD 1ST COMPARE    ADDRESS
         LH    R15,KEYCNT
*
COMPLOOP EX    R0,0(,R5)
         BRL   NOTFOUND
         BRH   READ
         EX    R0,L'MDLCLC(,R5)   ADVANCE LOOK-UP KEY FIELD ADDRESS
         AHI   R5,L'MDLCLC+L'MDLAHI
         BRCT  R15,COMPLOOP
*
FOUND    L     R14,GPBLOCKA
         STG   R1,0(,R14)
                        SPACE 3
RETURN   SR    R15,R15            ZERO RETURN CODE
*
RESTORE  L     R14,GPRTNCA        LOAD RETURN CODE  ADDRESS
         ST    R15,0(,R14)
*
         L     R13,RSABP(,R13)    RESTORE REGISTER  R13
RETURNOP L     R14,RSA14(,R13)    RESTORE REGISTER  R14
         LM    R0,R12,RSA0(R13)   RESTORE REGISTERS R0 - R12
         BSM   0,R14              RETURN
*
NOTFOUND L     R14,GPBLOCKA
         MVC   0(8,R14),HEXFF
*
         LA    R15,4              SET RC=4 (NOT  FOUND)
         B     RESTORE            RETURN
*
READ     BRAS  R9,READNEXT
         B     READLOOP
                        EJECT
         USING WORKAREA,R13
*
INITWORK L     R14,MDLWTOA        INITIALIZE  WTO PARAMETER AREA
         MVC   WTOPARM(WTOPARML),0(R14)
*
         L     R14,GPSTARTA       LOAD    STARTUP STRING ADDRESS
         LA    R2,SAVEPARM        LOAD    STARTUP DATA   ADDRESS
         USING PARMDATA,R2
*
         MVI   SAVEPARM+0,C' '
         MVC   SAVEPARM+1(L'SAVEPARM-1),SAVEPARM
         MVC   SAVEPARM(L'STARTUP),0(R14)
*
         ST    R2,UR33PARM+0
*
         LHI   R0,L'SAVEPARM
         STH   R0,PARMLEN
         LA    R0,PARMLEN
         ST    R0,UR33PARM+4
*
         L     R14,GPENVA
         LA    R0,GPENVVA-GENENV(,R14)
         ST    R0,UR33PARM+8
         LA    R1,UR33PARM
         L     R15,GVBUR33
         BASR  R14,R15
*
         LA    R4,PDKEYP01
         LA    R5,KEY01CLC
         LHI   R15,KEYMAX
*
KEYLOOP  CLC   0(L'PDKEYP01,R4),SPACES
         BRE   OPENDDN
*
         MVC   0(L'MDLCLC,R5),MDLCLC
         PACK  DBLWORK,0(L'PDKEYP01,R4)
         OI    DBLWORK+L'DBLWORK-1,X'0F'
         CVB   R0,DBLWORK
         BCTR  R0,0
         STH   R0,4(,R5)
         OI    4(R5),X'10'
*
         MVC   L'MDLCLC(L'MDLAHI,R5),MDLAHI
         PACK  DBLWORK,L'PDKEYP01+1(L'PDKEYL01,R4)
         OI    DBLWORK+L'DBLWORK-1,X'0F'
         CVB   R0,DBLWORK
         STH   R0,L'MDLCLC+2(,R5)
         BCTR  R0,0
         STC   R0,1(,R5)
*
         LH    R0,KEYCNT
         AHI   R0,1
         STH   R0,KEYCNT
*
         AHI   R4,L'PDKEYP01+1+L'PDKEYL01+1
         AHI   R5,L'MDLCLC+L'MDLAHI
         BRCT  R15,KEYLOOP
*
OPENDDN  MVC   UR20DDN,PDDDNAME   COPY    DDNAME
         MVC   ABNDFLAG,PDOPENAB  COPY    OPEN   ABEND FLAG
         DROP  R2
*
         LA    R0,UR20FC          BUILD   PARAMETER    LIST
         O     R0,MODE31
         ST    R0,UR20PARM
*
         MVC   UR20FC,H000        FUNCTION  = "OPEN"
         MVI   UR20OPT1,C'I'
         MVI   UR20OPT2,C' '
         MVC   UR20NBUF,H020
*
         LA    R1,UR20PARM
         llgf  R15,GVBUR20
         BASSM R14,R15
*
         LH    R15,UR20RC
         LTR   R15,R15
         BRZ   INITREAD
*
         MVI   EOFFLAG,C'Y'       BAD OPEN - TREAT AS END-OF-FILE
*
         CHI   R15,8              EMPTY ???
         BRNE  CHKABEND           NO  - BYPASS SPECIAL    CASE
*
         CLI   ABNDFLAG,C'E'      ABEND ON  EMPTY  ERROR  ???
         BRE   ERROR              YES - ABEND
         BRC   15,INITEXIT        NO  - BYPASS ABEND
*
CHKABEND CLI   ABNDFLAG,C'Y'      ABEND ON  OPEN   ERROR  ???
         BRNE  INITEXIT           NO  - BYPASS ABEND
*
ERROR    MVI   PRNTLINE,C' '      BLANK OUT PRINT  LINE
         MVC   PRNTLINE+1(L'PRNTLINE-1),PRNTLINE
         L     R14,OPENMSGA       ERROR MESSAGE STRUCTURE
         LH    R15,0(,R14)        LOAD  THE TEXT   LENGTH
         STH   R15,PRNTLEN
         BCTR  R15,0              DECREMENT   LENGTH  FOR "EX"  INSTR
         EX    R15,WTOMSG         MOVE  MESSAGE TO  PRINT  LINE
*
         MVC   PRNTLINE+41(8),UR20DDN
*
         WTO   TEXT=PRNTLEN,MF=(E,WTOPARM)
*
         LHI   R15,16
         BRC   15,RESTORE
*
INITREAD BRAS  R9,READNEXT        READ FIRST RECORD
*
INITEXIT BR    R10                RETURN
*
WTOMSG   MVC   PRNTLINE(0),2(R14) * * * *   E X E C U T E D   * * * *
                        SPACE 5
READNEXT MVC   UR20FC,H008        FUNCTION  = "READ"
*
         LA    R1,UR20PARM
         llgf  R15,GVBUR20
         BASSM R14,R15
*
         LH    R15,UR20RC
         LTR   R15,R15
         BRZ   READEXIT
*
         MVI   EOFFLAG,C'Y'       BAD OPEN - TREAT AS END-OF-FILE
*
READEXIT BR    R9                 RETURN
*
         DROP  R8
                        EJECT
MDLWTOA  DC    A(MDLWTO)        MODEL WTO PARAMETERS
OPENMSGA DC    A(OPENMSG)       OPEN  ERROR  MESSAGE
*
MDLCLC   CLC   0(0,R3),0(0)
MDLAHI   AHI   R3,0
*
GVBUR20  DC    V(GVBUR20)
GVBUR33  DC    V(GVBUR33)
*
HEXFF    DC    4X'FF'
MODE31   DC    XL4'80000000'
*
H000     DC    H'000'
H008     DC    H'008'
H020     DC    H'020'
*
SPACES   DC    CL8' '
*
MDLWTO   WTO   TEXT=(R2),MF=L   MODEL WTO TO DISPLAY CONSOLE MESSAGES
                        SPACE 3
OPENMSG  DC    AL2(L'OPENMSGT)
OPENMSGT DC    C'GVBXP01(01U) - UNABLE TO OPEN(OR EMPTY): XXXXXXXX'
                        SPACE 3
         DCBD  DSORG=PS
                        SPACE 3
GVBXP01  CSECT
         LTORG
*
*
         END

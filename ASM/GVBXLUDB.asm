         TITLE 'GVBXLUDB  DB2 direct lookup exit'
********************************************************************#VB
*   SAFR                                                              *
********************************************************************#VE
*
* (c) Copyright IBM Corporation 2021.
*     Copyright Contributors to the GenevaERS Project.
* SPDX-License-Identifier: Apache-2.0
*
***********************************************************************
*
*   Licensed under the Apache License, Version 2.0 (the "License");
*   you may not use this file except in compliance with the License.
*   You may obtain a copy of the License at
*
*     http://www.apache.org/licenses/LICENSE-2.0
*
*   Unless required by applicable law or agreed to in writing, software
*   distributed under the License is distributed on an "AS IS" BASIS,
*   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
*   or implied.
*   See the License for the specific language governing permissions and
*   limitations under the License.
***********************************************************************
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*   GVBXLUDB - Reads DB2 table based on a supplied key value          *
*                                                                     *
*  RETURN CODES:                                                      *
*                                                                     *
*            0  - SUCCESSFUL, DB2 ROW RETURNED                        *
*            4  - DB2 ROW NOT FOUND                                   *
*            8  - SKIP TO NEXT SOURCE RECORD                          *
*           12  - ERROR, DISABLE VIEW                                 *
*           16  - ERROR, ABORT                                        *
*                                                                     *
*                                                                     *
*  REGISTER USAGE:                                                    *
*                                                                     *
*        R15 - TEMPORARY WORK REGISTER                                *
*            - RETURN    CODE                                         *
*        R14 - TEMPORARY WORK REGISTER                                *
*            - ERROR MESSAGE NUMBER                                   *
*            - RETURN    ADDRESS                                      *
*        R13 - REGISTER  SAVE AREA  ADDRESS (THREAD WORK AREA)        *
*        R12 - PROGRAM   BASE REGISTER                                *
*        R10 - INTERNAL  SUBROUTINE  RETURN ADDRESS (1ST LEVEL)       *
*        R9  - TEMPORARY WORK REGISTER                                *
*        R8  - ADDRESS OF GENPARM                                     *
*        R7  - SQL DESCRIPTOR AREA ADDRESS ("SQLDA")                  *
*        R6  - LOOKUP KEY ADDRESS                                     *
*        R5  - SQL CALL  WORK AREA ADDRESS                            *
*        R4  - WORK      REGISTER                                     *
*        R3  - WORK      REGISTER                                     *
*        R2  - WORK      REGISTER                                     *
*        R1  - TEMPORARY WORK REGISTER                                *
*        R0  - TEMPORARY WORK REGISTER                                *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*  To customise this exit:                                            *
*  1) Modify the SQL declaration for the table to be accessed         *
*     (search for EXEC SQL DECLARE)                                   *
*  2) Modify the SQL cursor SELECT statement (search for SQL CURSORS) *
*     The DB2 columns selected must match the LR fields, specifically *
*     the data types and lengths                                      *
*  3) Modify the host variables defined in the WORKAREA               *
*     (search for WORKREC)                                            *
*  4) Modify the plan name if required (search for DB2PLAN). It must  *
*     match a plan name in the bind JCL.                              *
*                                                                     *
***********************************************************************
*                                                                     *
*       "GVBXLUDB - W O R K A R E A   D E F I N I T I O N             *
*                                                                     *
***********************************************************************
WORKAREA DSECT
         DS    xl(savf4sa_len)    Format 4 save area
*
* Host variables
*
* The following field is used in the SELECT WHERE clause.
* The data type and length must match that of the DB2 field
*
WORKNUM  DS    CL5
*
* The following fields must match the data types and lengths
*  of the returned DB2 row
*
WORKREC  DS    0D                 RETURNED DB2 ROW IS PUT HERE
WORKEMPN DS    CL5                EMPLOYEE_NUMBER,
WORKEMPD DS    CL5                EMPLOYEE_DEPT,
WORKNAML DS    CL20               EMPLOYEE_NAM_LAST,
WORKNAMF DS    CL20               EMPLOYEE_NAM_FIRST,
WORKNAMM DS    CL1                EMPLOYEE_NAM_MI,
WORKJOBC DS    CL2                EMPLOYEE_JOB_CODE
WORKEMPA DS    CL56               EMPLOYEE_ADDRESS
WORKEMPP DS    CL10               EMPLOYEE_PHONE
WORKHIRE DS    CL8                EMPLOYEE_HIRE_DTE
WORKEFFD DS    CL8                EFFECTIVE_DTE
WORKENDD DS    CL8                END_DTE
*
         DS    0D
         EXEC  SQL INCLUDE SQLCA
SQLCALEN EQU   *-SQLCA
*
SQLDADDR DS    F                  ADDRESS OF SQLDA
SQLWADDR DS    F                  ADDRESS OF SQL WORK AREA
*
CAFFUNC  DS    CL12               CURRENTLY  EXECUTING CALL ATTACH FUNC
DBTRMECB DS    F                  DB2  TERMINATION ECB
DBSTRECB DS    F                  DB2  STARTUP     ECB
RIBPTR   DS    A                  RELEASE  INFORMATION BLOCK ADDRESS
TERMOP   DS    CL4
*
CAFPARM1 DS    A                  PARM LIST USED TO CALL CAF
CAFPARM2 DS    A
CAFPARM3 DS    A
CAFPARM4 DS    A
CAFPARM5 DS    A
*
WORKGENP DS    F                  SAVE ADDRESS OF GENPARM
*
DSNALI   DS    A                  DB2     INTERFACE (ASSEMBLER)
DSNHLI2  DS    A                  DB2     INTERFACE (HIGHER LEVEL LANG)
*
* Error message processing
*
PACKCODE DS    CL5                REASON CODE
REASCODE DS    CL9                READABLE REASON CODE
DBLWORK  DS    D                  DOUBLEWORD  WORK AREA
DBLWORK2 DS    D                  DOUBLEWORD  WORK AREA
WORKMSG  DS    CL80               WORK AREA FOR ERROR MSG
*
* Error message routine parm list
*
MSG_AREA GVBMSG PREFIX=MMM,MF=L
*
WORKLEN  EQU   *-WORKAREA
*
***********************************************************************
* LOOKUP KEY                                                          *
***********************************************************************
LKUPKEY  DSECT                 LOOK-UP  KEY
KYLRID   DS    FL04            LOGICAL  RECORD  ID
KYDATA   DS    CL230           STRING   DATA
*
         Copy  GVBASSRT
         Copy  GVBX95PA
         Copy  GVBLOGIT
         Copy  GVBRPTIT
         Copy  GVBUTEQU
*
         Copy  ASMMSP
LEAVE    OpSyn ASM_LEAVE
         ASMMREL ON
         SYSSTATE ARCHLVL=2
         IEABRCX DEFINE
         IHASAVER DSECT=YES,SAVER=NO,SAVF4SA=YES,TITLE=NO
*
*****************************************************
* SQL DECLARATION FOR VIEW EMPLOYEE                 *
*****************************************************
         EXEC SQL DECLARE EMPLOYEE TABLE                               +
               ( EMPLOYEE_NUMBER  CHAR(5),                             +
               EMPLOYEE_DEPT    CHAR(5),                               +
               EMPLOYEE_NAM_LAST CHAR(20),                             +
               EMPLOYEE_NAM_FIRST CHAR(20),                            +
               EMPLOYEE_NAM_MI    CHAR(1),                             +
               EMPLOYEE_JOB_CODE  CHAR(2),                             +
               EMPLOYEE_ADDRESS   CHAR(56),                            +
               EMPLOYEE_PHONE     CHAR(10),                            +
               EMPLOYEE_HIRE_DTE  CHAR(8),                             +
               EFFECTIVE_DTE      CHAR(8),                             +
               END_DTE            CHAR(8))
*
*****************************************************
* SQL CURSORS                                       *
*****************************************************
         EXEC  SQL DECLARE LUROW CURSOR FOR                            +
                SELECT *                                               +
                FROM EMPLOYEE                                          +
                WHERE EMPLOYEE_NUMBER=:WORKNUM
*
         YREGS                    Register equates
*
GVBXLUDB RMODE 31
GVBXLUDB AMODE 31
         ENTRY DSNHLI
GVBXLUDB CSECT
         j     start              get to the code
static   loctr                    define the static section
XLUDEYE  GVBEYE GVBXLUDB
code     loctr                    and the code
*
START    DS    0H
PREV     USING SAVF4SA,R13        MAP THE SAVE AREA
         STMG  R14,R12,PREV.SAVF4SAG64RS14 SAVE CALLERS REGISTERS
         LARL  R12,GVBXLUDB
         USING (GVBXLUDB,code),R12
         LA    R8,0(,R1)          LOAD  PARAMETER LIST ADDRESS
         USING GENPARM,R8
*
         LLGT  R9,GPWORKA         LOAD  WORK  AREA ANCHOR ADDR
         LLGT  R14,GPENVA         GET ENVIROMENT INFO AREA PTR
         USING GENENV,R14           AND MAP
         IF (LT,R1,0(,R9),NP)     ANY WA ALLOCATED
***********************************************************************
*  ALLOCATE "GVBXLUDB" WORKAREA IF NOT ALREADY ALLOCATED (PER THREAD) *
***********************************************************************
           LHI R0,WORKLEN+8       LOAD   WORK AREA SIZE
           STORAGE OBTAIN,LENGTH=(0),LOC=(ANY),CHECKZERO=YES GET WORKA
           IF  CIJ,R15,NE,X'14'   NOT ZEROED?
             LGR  R10,R1            SAVE ADDRESS
             LGR  R0,R1             ZERO  WORK  AREA
             LGHI R1,WORKLEN+8
             SGR  R14,R14
             SGR  R15,R15
             MVCL R0,R14
             LGR R1,R10            RESTORE POINTER
           ENDIF
*
           MVC  0(l'modname,R1),MODNAME
           AGHI R1,L'MODNAME      MOVE POINTER PAST
           USING workarea,R1
           USING SAVF4SA,WORKAREA
*
           STG  R13,SAVF4SAPREV     SAVE OLD SAVE AREA IN CURRENT
           STG  R1,PREV.SAVF4SANEXT SAVE CURRENT SAVE AREA IN OLD
           MVC  SAVF4SAID(4),=A(SAVF4SAID_VALUE) SET 'F4SA' IN AREA
           LGR  R13,R1             GET NEW WORKAREA INTO R13
           ST   R13,0(,R9)         SAVE  WORK  AREA ADDRESS
*                                                                     *
           BRAS R10,INITWORK      INITIALIZE WORK   AREA
*
         ELSE
***********************************************************************
*    CHAIN REGISTER SAVE AREAS TOGETHER                               *
*    CHECK FOR CHANGE IN EVENT RECORD ADDRESS                         *
***********************************************************************
           STG   R13,SAVF4SAPREV     SET   FORWARD  POINTER IN OLD
           STG   R1,PREV.SAVF4SANEXT SET   BACKWARD POINTER IN NEW
           LGR   R13,R1           GET NEW WORKAREA INTO R13
           DROP  R1
           DROP  PREV
           USING WORKAREA,R13
           USING SAVF4SA,WORKAREA
*
         ENDIF
***********************************************************************
*  CHECK which phase requested                                        *
***********************************************************************
         ST    R8,WORKGENP        SAVE ADDRESS OF GENPARM
*
         llgt  R14,GPENVA         LOAD ENVIRONMENT INFO ADDRESS
         USING GENENV,R14

         CLI   GPPHASE,C'O'       OPEN PHASE
         je    fopen
*
         CLI   GPPHASE,C'C'       CLOSE PHASE
         je    fclose
         DROP  R14
*
FLOOKUP  DS    0H                 LOOKUP PHASE
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        SELECT AND FETCH THE ROW (THE LOOKUP)                        *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         llgt  R6,GPKEYA          LOAD LOOK-UP KEY ADDRESS
         USING LKUPKEY,R6
*
         MVC   WORKNUM,KYDATA
         DROP  R6
**********************************************************************
* OPEN DB2 CURSOR                                                    *
**********************************************************************
         L     R5,SQLWADDR        SAVE WORK AREA ADDRESS
         USING SQLDSECT,R5
         MVC   CAFFUNC,OPEN
*
         MVC   CAFFUNC,OPENC      INDICATE CURRENT FUNCTION
*
         EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL OPEN LUROW
*
         L     R15,SQLCODE         SUCCESSFUL OPEN ???
         LTR   R15,R15
         BNZ   FLOPERR             NO - ERROR
*
**********************************************************************
* FETCH ROW                                                          *
**********************************************************************
         MVC   CAFFUNC,FETCH      INDICATE CURRENT FUNCTION
*
         EXEC SQL FETCH LUROW INTO :WORKEMPN ,                         +
                                    :WORKEMPD ,                        +
                                    :WORKNAML ,                        +
                                    :WORKNAMF ,                        +
                                    :WORKNAMM ,                        +
                                    :WORKJOBC ,                        +
                                    :WORKEMPA ,                        +
                                    :WORKEMPP ,                        +
                                    :WORKHIRE ,                        +
                                    :WORKEFFD ,                        +
                                    :WORKENDD
*
         L     R15,SQLCODE         SUCCESSFUL OPEN ???
         LTR   R15,R15
         BNZ   FLFTERR             NO - ERROR
*
***********************************************************************
*  RETURN A POINTER TO THE ROW FETCHED                                *
***********************************************************************
FOUND    DS    0H
         L     R8,WORKGENP        ADDRESS OF GENPARM
         USING GENPARM,R8
*
         Llgt  R14,GPBLOCKA       PASS ROW ADDRESS TO CALLER
         LA    R0,WORKREC
         stg   R0,0(,R14)
**********************************************************************
* CLOSE CURSOR                                                       *
**********************************************************************
         MVC   CAFFUNC,CLOSE       INDICATE CURRENT CAF FUNCTION
         EXEC  SQL CLOSE LUROW
*
RETURN   SGR   R15,R15            ZERO RETURN CODE
*
RETURNRC L     R14,GPRTNCA        LOAD RETURN CODE ADDRESS
         ST    R15,0(,R14)
*
         LG    R13,SAVF4SAPREV         RESTORE CALLER SAVE AREA
         LG    R14,SAVF4SAG64RS14      RESTORE CALLER'S R14
         LMG   R2,R12,SAVF4SAG64RS2    RESTORE CALLER'S R2- R12
         BSM   0,R14              RETURN
*
***********************************************************************
*  RETURN NOT FOUND CONDITION - RC=4                                  *
***********************************************************************
NOTFOUND DS    0H
*
         LGF   R14,GPBLOCKA
         LLGTR R14,R14
         MVC   0(8,R14),FFFF      64 bit address
*
         MVC   CAFFUNC,CLOSE       INDICATE CURRENT CAF FUNCTION
         EXEC  SQL CLOSE LUROW
*
         LGHI  R15,4              SET RC=4 (NOT  FOUND)
         BRU   RETURNRC           RETURN
*
**********************************************************************
* ERROR PROCESSING
**********************************************************************
FLOPERR  DS    0H
         LA    R14,SQL_OPEN_FAIL   OPEN CURSOR FAILED
         B     FLERROR
FLFTERR  DS    0H
         LA    R14,SQL_FETCH_FAIL  FETCH FAILED
         B     FLERROR
*
FLERROR  DS    0H
         CVD   R15,DBLWORK         STORE THE RETURN CODE (PACKED)
         MVC   REASCODE(8),SPACES
         UNPK  REASCODE(4),DBLWORK+4(4)
         OI    REASCODE+3,X'F0'    FORCE A DISPLAYABLE ZONE
         LTR   R15,R15             NEGATIVE  SQLCODE ???
         BP    *+8                 NO  - BYPASS MOVE
         MVI   REASCODE+0,C'-'
*
RTNERROR DS    0H
*
* Write error message to log
*
         L     R6,GPENVA          ADDRESS OF GENPARM
         USING GENENV,R6
         L     R2,GPFILEA         ADDRESS OF GENPARM
         USING GENFILE,R2
*
         GVBMSG LOG,MSGNO=(R14),SUBNO=3,GENENV=GENENV,                 +
               SUB1=(modname,8),                                       +
               SUB2=(REASCODE,8),                                      +
               SUB3=(GPDDNAME,8),                                      +
               MF=(E,MSG_AREA)
         J     RTNXTRA
*
RTNERROR2 DS    0h  ERROR MESSAGE WITH ONLY 2 PARMS
*
         GVBMSG LOG,MSGNO=(R14),SUBNO=2,GENENV=GENENV,                 +
               SUB1=(modname,8),                                       +
               SUB2=(REASCODE,8),                                      +
               MF=(E,MSG_AREA)
RTNXTRA  DS    0h
*
* If there is any info from a translated SQLCODE, write that to log
*
         if oc,workmsg(4),workmsg,nz
           if clc,workmsg(4),eq,ffff  did XLATE fail?

             GVBMSG LOG,MSGNO=SQL_XLATE_FAIL,SUBNO=2,                  +
               GENENV=GENENV,                                          +
               SUB1=(modname,8),                                       +
               SUB2=(WORKMSG+4,4),                                     +
               MF=(E,MSG_AREA)
           else
             GVBMSG LOG,MSGNO=DB2_TRANSLATED_SQLCODE,SUBNO=2,          +
               GENENV=GENENV,                                          +
               SUB1=(modname,8),                                       +
               SUB2=(WORKMSG,64),                                      +
               MF=(E,MSG_AREA)
           endif
         endif
*
         LHI   R15,12
*
         B     RETURNRC           RETURN THE RC
         DROP  R2,R6
*
FOPEN    DS    0H                 OPEN PHASE
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        I N I T I A L I Z E   W O R K   A R E A S                    *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         MVC   SQLCAID,=CL8'SQLCA'
         LA    R0,SQLCALEN
         ST    R0,SQLCABC
*
         L     R0,SQLDALEN        ALLOCATE  SQL  DESCRIPTOR    AREA
         GETMAIN RU,LV=(0),LOC=(ANY)
         ST    R1,SQLDADDR        SAVE SQL  DESCRIPTOR AREA ADDRESS
         LR    R7,R1
         USING SQLDA,R7
*
         LR    R0,R7              INITIALIZE  "SQLDA" (LOW  VALUES)
         L     R1,SQLDALEN
         SR    R14,R14
         SR    R15,R15
         MVCL  R0,R14
*
         MVC   SQLDAID,=CL8'SQLDA'
         MVC   SQLDABC,SQLDALEN
         MVC   SQLN,H750
*
         L     R0,SQLDSIZ         ALLOCATE  SQL  WORK  AREA
         GETMAIN R,LV=(0)
         ST    R1,SQLWADDR        SAVE WORK AREA ADDRESS
         LR    R5,R1
         USING SQLDSECT,R5
*
         DROP  R7
*
         BAS   R10,DB2CONN        ESTABLISH  DB2  CONNECTION
*
         J     RETURN
         DROP  R5
*
***********************************************************************
*                                                                     *
*        I N I T I A L I Z E   W O R K   A R E A                      *
*                                                                     *
***********************************************************************
         USING WORKAREA,R13
*
INITWORK DS    0H
*
         LOAD  EP=DSNALI          LOAD  DB2   ASSEMBLER INTERFACE
         ST    R0,DSNALI
*
         LOAD  EP=DSNHLI2         LOAD  DB2   HIGHER  LEVEL  LANG ADDR
         ST    R0,DSNHLI2
*
         BR    R10
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        E S T A B L I S H   D B 2   C O N N E C T I O N              *
*                                                                     *
*              - CONNECT                                              *
*                                                                     *
*                ESTABLISH A CONNECTION BETWEEN AN APPLICATION'S      *
*                ADDRESS SPACE AND A SPECIFIED DB2 SUBSYSTEM.         *
*                                                                     *
*              - OPEN THREAD                                          *
*                                                                     *
*                ESTABLISH THE SPECIFIED DB2PLAN AS A USER OF DB2     *
*                SERVICES AND ALLOCATE RESOURCES FOR SQL CALLS.       *
*                THIS CAN BE CALLED 'CREATING A THREAD'.              *
*                                                                     *
*              - TRANSLATE                                            *
*                                                                     *
*                CLARIFIES A RESOURCE UNAVAILABLE CONDITION.          *
*                IT IS PERFORMED AFTER AN OPEN FAILURE.               *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
**********************************************************************
* CONNECT TO DB2                                                     *
**********************************************************************
DB2CONN  DS    0H
         LA    R1,CONNECT          CONNECT    FUNCTION
         LA    R2,DBSUBSYS         SUBSYSTEM  NAME
         LA    R3,DBTRMECB         DB2 STARTUP     ECB
         LA    R4,DBSTRECB         DB2 TERMINATION ECB
         LA    R9,RIBPTR           RELEASE INFORMATION BLOCK
         STM   R1,R4,CAFPARM1
         ST    R9,CAFPARM5
         OI    CAFPARM5,X'80'
*
         MVC   CAFFUNC,CONNECT     INDICATE CURRENT CAF FUNCTION
*
         LA    R1,CAFPARM1         CALL "DSNALI" - CONNECT
         L     R15,DSNALI
         BASR  R14,R15
*
         LTR   R15,R15             SUCCESSFUL  CONNECT ???
         BZ    CONNOPEN            YES - CONTINUE
*    Already connected ?
         C     R0,X0C10201
         BE    CONNOPEN
*
         LA    R14,DB2_CONNECT_FAIL NO  - CONNECT  FAILED
         ST    R0,PACKCODE         STORE THE REASON CODE
         UNPK  REASCODE(9),PACKCODE(5)
         TR    REASCODE(8),XTAB
         B     RTNERROR
**********************************************************************
* OPEN THREAD                                                        *
**********************************************************************
*
CONNOPEN LA    R1,OPEN             OPEN    THREAD  FUNCTION
         LA    R2,DBSUBSYS         SUBSYSTEM NAME
         LA    R3,DB2PLAN          DB2PLAN   NAME
         STM   R1,R3,CAFPARM1
         OI    CAFPARM3,X'80'
*
         MVC   CAFFUNC,OPEN        INDICATE CURRENT CAF FUNCTION
*
         LA    R1,CAFPARM1         CALL "DSNALI" - OPEN THREAD
         L     R15,DSNALI
         BASR  R14,R15
*
         LTR   R15,R15             SUCCESSFUL OPEN ???
         BZ    CONNEXIT
*    Thread already open ?
         C     R0,X0C10202
         BE    CONNEXIT
         LA    R14,DB2_OPEN_THREAD_FAIL ASSUME OPEN FAILED
**********************************************************************
* TRANSLATE ERROR CODE                                               *
**********************************************************************
XLATEERR DS    0H
         ST    R0,PACKCODE         STORE THE REASON CODE
         CVD   R15,DBLWORK         STORE THE RETURN CODE (PACKED)
         UNPK  DBLWORK(4),DBLWORK+4(4)
         OI    DBLWORK+3,X'F0'     FORCE A DISPLAYABLE ZONE
         LTR   R15,R15             NEGATIVE RETURN CODE ???
         BP    *+8                 NO  - BYPASS  MINUS SIGN
         MVI   DBLWORK+0,C'-'
         ST    R14,DBLWORK2        SAVE ORIGINAL ERROR MESSAGE CODE
*
         UNPK  REASCODE(9),PACKCODE(5)
         TR    REASCODE(8),XTAB
*
         LA    R1,XLATE            TRANSLATE   RETURN  CODE   FUNCTION
         LA    R2,SQLCA            SQL  COMMUNICATION  AREA
         STM   R1,R2,CAFPARM1
         LA    R0,DBLWORK+0
         ST    R0,CAFPARM3
         LA    R0,PACKCODE
         ST    R0,CAFPARM4
         OI    CAFPARM4,X'80'
*
         MVC   CAFFUNC,XLATE       INDICATE CURRENT CAF FUNCTION
*
         LA    R1,CAFPARM1         CALL "DSNALI" - TRANSLATE "SQLCODE"
         L     R15,DSNALI
         BASR  R14,R15
* SQL_XLATE_FAIL
         if LTR,R15,R15,nz         UNSUCCESSFUL XLATE ?
*          LA  R14,SQL_XLATE_FAIL  TRANSLATE FAILED
           CVD R15,DBLWORK         STORE THE RETURN CODE (PACKED)
           MVC WORKMSG+4(8),SPACES
           UNPK WORKMSG+4(4),DBLWORK+4(4)
           OI  WORKMSG+4+3,X'F0'   FORCE A DISPLAYABLE ZONE
           LTR R15,R15             NEGATIVE  SQLCODE ???
           BP  *+8                 NO  - BYPASS MOVE
           MVI WORKMSG+4,C'-'
           mvc WORKMSG(4),FFFF     Indicate XLATE failed
*
           L   R14,DBLWORK2         LOAD ERROR MESSAGE ADDRESS
         else
*
* Error text contains "<SQLERRM>......      <Return code>"
*
           MVC WORKMSG+00(50),SQLERRM+2 BUILD ERROR MESSAGE
           MVI WORKMSG+50,C'.'
           MVC WORKMSG+51(5),WORKMSG+50
           MVC WORKMSG+56(8),SPACES
           MVC WORKMSG+60(4),DBLWORK+0
*
           L   R14,DBLWORK2         LOAD ERROR MESSAGE ADDRESS
         endif
         B     RTNERROR            DISPLAY ORIGINAL MESSAGE
*
CONNEXIT EQU   *
         BSM   0,R10              RETURN
*
FCLOSE   DS    0H                 CLOSE PHASE
**********************************************************************
* CLOSE THREAD                                                       *
**********************************************************************
         MVC   TERMOP,SYNC         SET   DB2 TERMINATION  OPTION
*
         LA    R1,CLOSE            BUILD PARAMETER  LIST
         LA    R2,TERMOP
         STM   R1,R2,CAFPARM1
         OI    CAFPARM2,X'80'
*
         LA    R1,CAFPARM1         CALL "DSNALI" - CLOSE  THREAD
         L     R15,DSNALI
         BASR  R14,R15
**********************************************************************
* DISCONNECT                                                         *
**********************************************************************
         LA    R1,DISC             BUILD PARAMETER  LIST
         ST    R1,CAFPARM1
         OI    CAFPARM1,X'80'
*
         LA    R1,CAFPARM1         CALL "DSNALI" - DISCONNECT FROM DB2
         L     R15,DSNALI
         BASR  R14,R15
*
*        free sqldaddr area
         L     R0,SQLDALEN        SQL DESCRIPTOR AREA length
         l     R1,SQLDADDR        Load SQL DESCRIPTOR AREA ADDRESS
         STORAGE RELEASE,LENGTH=(0),ADDR=(1)
*        free sqlwaddr area
         L     R0,SQLDSIZ         SQL WORK AREA length
         l     R1,SQLWADDR        load WORK AREA ADDRESS
         STORAGE RELEASE,LENGTH=(0),ADDR=(1)
*
         J     RETURN
         DROP  R8
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        "DUMMY" DSNHLI ENTRY POINT FOR "EXEC SQL" GENERATED CODE     *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         USING *,R15
DSNHLI   L     R15,DSNHLI2
         BR    R15
         DROP  R15
         DROP  R13
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        C O N S T A N T S                                            *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
static   loctr                    define the static section
DBSUBSYS DC    CL4'DM12'          SUBSYSTEM   NAME
DB2PLAN  DC    CL8'GVBXPLAN'      DB2PLAN  NAME
F10K     equ   10240
*
F4       DC    F'04'
F8       DC    F'08'
*
X0C10201 DC    XL4'00C10201'
X0C10202 DC    XL4'00C10202'
MODE31   DC    XL4'80000000'
*
SQLDALEN DC    F'33016'           16 + (750 COLUMNS * 44)
*
DSNTIAR  DC    V(DSNTIAR)
*
H750     DC    H'750'
FFFF     DC    XL8'FFFFFFFFFFFFFFFFF'
*
         ORG   *-240              TRANSLATE TABLE FOR BINARY TO HEX
XTAB     EQU   *                       (VIRTUAL ORIGIN)
         ORG   *+240
         DC    C'0123456789ABCDEF'
*
*ENEVA   DC    CL8'GENEVA  '      MAJOR  ENQ NODE
*NQSTAT  DC    CL8'ENQSTAT '      MINOR  ENQ NODE
TRACNAME DC    CL8'MR95TRAC'      MINOR  ENQ NODE  (MR95 TRACE FILE)
LOGNAME  DC    CL8'MR95LOG'       MINOR  ENQ NODE  (MR95 LOG FILE)
SYNC     DC    CL4'SYNC'          DB2 THREAD TERMINATION OPTION
SPACES   DC    CL32'   '
*
                        SPACE 3
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        CALL  ATTACH  FUNCTION  REQUEST  NAMES                       *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
CONNECT  DC    CL12'CONNECT      ' CONNECT      TO   DB2
DISC     DC    CL12'DISCONNECT   ' DISCONNECT   FROM DB2
OPEN     DC    CL12'OPEN         ' OPEN   (CREATE    THREAD)
OPENC    DC    CL12'OPEN CURSOR  ' OPEN CURSOR
CLOSE    DC    CL12'CLOSE        ' CLOSE  (TERMINATE THREAD)
CLOSEC   DC    CL12'CLOSE CURSOR ' CLOSE CURSOR
XLATE    DC    CL12'TRANSLATE    ' TRANSLATE    OPEN ERRORS
FETCH    DC    CL12'FETCH        ' FETCH    DB2 ROW
SETDEG   DC    CL12'SETDEGREE    ' SET PARALLEL PROCESSING DEGREE
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        D A T A   C O N T R O L   B L O C K S                        *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
SNAPDCB  DCB   DSORG=PS,DDNAME=SNAPDATA,MACRF=(W),                     X
               RECFM=VBA,LRECL=125,BLKSIZE=1632
*
         DCBD  DSORG=PS
         ihadcbe
*
GVBXLUDB CSECT
         EXEC  SQL INCLUDE SQLDA
GVBXLUDB CSECT
*
static   loctr
         LTORG
*
         END

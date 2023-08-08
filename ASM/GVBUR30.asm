         TITLE 'GVBUR30 - DB2 TABLE READ ROUTINE'
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
*                                                                     *
*  GVBUR30 - DYNAMICALLY ESTABLISHES A CONNECTION TO DB2, ISSUES      *
*            A "PREPARE" FOR THE SQL TEXT, AND THEN SAVES THE         *
*            ATTRIBUTES OF EACH COLUMN;                               *
*                                                                     *
*            THE ROWS FROM THE DB2 TABLE ARE RETRIEVED  USING         *
*            SQL "OPEN CURSOR & FETCH" STATEMENTS                     *
*                                                                     *
*  NOTE:     GVBUR30 RUNS MOSTLY IN 31-BIT ADDRESSING MODE.           *
*                                                                     *
*                                                                     *
*  REGISTER USAGE:                                                    *
*                                                                     *
*        R15 - TEMPORARY WORK REGISTER                                *
*            - RETURN    CODE                                         *
*                                                                     *
*        R14 - TEMPORARY WORK REGISTER                                *
*            - INTERNAL  SUBROUTINE  RETURN ADDRESS (3RD LEVEL)       *
*            - RETURN    ADDRESS                                      *
*                                                                     *
*        R13 - REGISTER  SAVE AREA  ADDRESS (GVBUR30 WORK AREA)       *
*                                                                     *
*        R12 - PROGRAM   BASE REGISTER                                *
*        R11 - PROGRAM   BASE REGISTER                                *
*                                                                     *
*        R10 - INTERNAL  SUBROUTINE  RETURN ADDRESS (1ST LEVEL)       *
*        R9  - INTERNAL  SUBROUTINE  RETURN ADDRESS (2ND LEVEL)       *
*            - DB2/SQL   CODE SEGMENT BASE  REGISTER                  *
*            - WORK      REGISTER                                     *
*                                                                     *
*        R8  - CALL      PARAMETER    AREA  ADDRESS                   *
*                                                                     *
*        R7  - DDNAME    WORK AREA ADDRESS                            *
*            - ROW/RECORD     DATA ADDRESS                            *
*                                                                     *
*        R6  - SQL  CALL WORK AREA ADDRESS ("SQLCA")                  *
*                                                                     *
*        R5  - SQL   DATA     AREA ADDRESS ("SQLDA")                  *
*                                                                     *
*        R4  - WORK  REGISTER                                         *
*            - SQL   VARIABLE DEFN    AREA ("SQLVARN")                *
*                                                                     *
*        R3  - WORK  REGISTER                                         *
*        R2  - WORK  REGISTER                                         *
*                                                                     *
*        R1  - PARAMETER LIST ADDRESS                                 *
*            - TEMPORARY WORK REGISTER                                *
*                                                                     *
*        R0  - TEMPORARY WORK REGISTER                                *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*                                                                     *
*  FUNCTION CODES:                                                    *
*                                                                     *
*        00  - OPEN                                                   *
*        04  - CLOSE                                                  *
*        08  - READ  SEQUENTIAL                                       *
*                                                                     *
*                                                                     *
*  RETURN CODES:                                                      *
*                                                                     *
*        00  - SUCCESSFUL                                             *
*        04  - SUCCESSFUL (WARNING: SEE ERROR CODE)                   *
*        08  - END-OF-TABLE                                           *
*        16  - PERMANENT   ERROR   (SEE ERROR CODE)                   *
*                                                                     *
*                                                                     *
*  ERROR CODES:                                                       *
*                                                                     *
*        00  - SUCCESSFUL                                             *
*        01  - BAD  WORK AREA   POINTER                               *
*        02  - UNDEFINED FUNCTION  CODE (SB:  0,4,8)                  *
*        03  - UNDEFINED I/O       MODE (SB: "I")                     *
*        04  - TABLE ALREADY     OPENED                               *
*        05  - OPEN  FOR OUTPUT  FAILED                               *
*        06  - OPEN  FOR INPUT   FAILED                               *
*        07  - TABLE NEVER       OPENED                               *
*        08  - TABLE ALREADY     CLOSED                               *
*        09  - BAD   ROW         LENGTH                               *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         Push  PRINT
         Print OFF,NOGEN,NOPRINT
         Copy  ASMMSP
LEAVE    OpSyn ASM_LEAVE
         ASMMREL ON
         SYSSTATE ARCHLVL=2
         IEABRCX DEFINE
         Pop   PRINT
*
         Copy  GVBUTEQU
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        P A R A M E T E R   L I S T                                  *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
PARMLIST DSECT                 PARAMETER  LIST    DEFINITION
*
PARMAPTR DS    A               CALL  PARAMETER    AREA    ADDRESS
PARMTXTL DS    A               SQL   TEXT         LENGTH  ADDRESS
PARMTXTA DS    A               SQL   TEXT         ADDRESS
PARMSUBS DS    A               DB2   SUBSYSTEM    NAME    ADDRESS
                        SPACE 5
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        P A R A M E T E R   A R E A                                  *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
PARMAREA GVBUR20P PRE=PARM,DSECT=Y
*PARMAREA DSECT                 CALL  PARAMETER    AREA
*
*PARMFC   DS    HL02           FUNCTION CODE
*PARMRC   DS    HL02           RETURN   CODE
*PARMERRC DS    HL02           ERROR    CODE
*PARMRECL DS    HL02           RECORD   LENGTH
*PARMRECA DS    AL04           RECORD   AREA      ADDRESS
*PARMRBN  DS    FL04           RELATIVE BLOCK     NUMBER
*PARMDDN  DS    CL08           FILE     DDNAME
*PARMOPT1 DS    CL01           I/O MODE(I=INPUT/SQL)
*PARMOPT2 DS    CL01           (F=SQL FORMATTED,U=UNFORMATTED/INTERNAL)
*PARMNBUF DS    HL02           NUMBER   OF I/O    BUFFERS
*PARMWPTR DS    AL04           WORK     AREA      POINTER
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        "GVBUR30"    W O R K   A R E A                               *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
WORKAREA DSECT
*
SAVEAREA DS  18F                  REGISTER  SAVE  AREA
*
DBLWORK  DS    D                  TEMPORARY DOUBLEWORD  WORK AREA
DBLWORK2 DS    D                  TEMPORARY DOUBLEWORD  WORK AREA
*
SAVEPARM DS    A                  PARAMETER LIST  ADDR
*
WORKDPTR DS    A                  FIRST    DDNAME    WORK AREA
WORKMODE DS    A                  CALLER'S MODE     (24/31 BIT)
*
WORKACTV DS    F                  ACTIVE CURSOR  COUNT
*
DSNALI   DS    A                  DB2     INTERFACE (ASSEMBLER)
DSNHLI2  DS    A                  DB2     INTERFACE (HIGHER LEVEL LANG)
*
RENTPARM DS    XL128              RE-ENTRANT   CALL  PARAMETER    AREA
*
*ORKMSGL DS    HL02               WTO     MESSAGE  AREA
WORKMSG  DS    CL164
*
*TOPARM  WTO   TEXT=(R2),MF=L
*TOPARML EQU   *-WTOPARM
*
MSG_AREA GVBMSG PREFIX=MMM,MF=L
*
WORKLEN  EQU   *-WORKAREA
                        SPACE 5
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        D D N A M E   W O R K   A R E A                              *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
DDNWORK  DSECT                    DDNAME  SPECIFIC   WORKAREA
*
WORKNEXT DS    A                  NEXT    DDNAME     WORKAREA
WORKINDX DS    F                  DB2     BRANCH     TABLE INDEX
WORKDDN  DS    CL08               LOGICAL DDNAME
*
WORKSQLW DS    A                  SQL     WORK  AREA ADDRESS
WORKSQLD DS    A                  SQL     DESCRIPTOR AREA  ADDRESS
*
WORKCI#  DS    PL06               DB2     CONTROL INTERVAL COUNT
WORKCNT  DS    PL06               DB2     RECORD/ROW       COUNT
WORKROWL DS    F                  DB2     RECORD LENGTH   (CURRENT ROW)
WORKROWA DS    A                  DB2     RECORD ADDRESS  (CURRENT ROW)
*
CALLPARM DS   0A                  CALL    PARAMETER AREA
CALLP01  DS    A
CALLP02  DS    A
CALLP03  DS    A
CALLP04  DS    A
CALLP05  DS    A
*
PACKCODE DS    CL05               REASON CODE FROM HERE
REASCODE DS    CL09               READABLE RETURN CODE (HEX)
DBSUBSYS DS    CL04               SUBSYSTEM  NAME
DBTRMECB DS    F                  DB2 TERMINATION  ECB
DBSTRECB DS    F                  DB2 STARTUP      ECB
RIBPTR   DS    A                  RELEASE  INFORMATION BLOCK ADDRESS
DB2PLAN  DS    CL08               DB2 PLAN NAME
DBEDITPR DS    CL08               DB2 EDIT PROCEDURE   NAME
TERMOP   DS    CL04               TERMINATION  OPTION
*
CAFFUNC  DS    CL12               CURRENTLY EXECUTING CALL ATTACH FUNC
*
RECADDR  DS    A                  CURRENT RECORD    ADDRESS
RECLEN   DS    F                  CURRENT RECORD    LENGTH
RECEND   DS    A                  CURRENT RECORD    END
*
EDITPROC DS    A                  "EDITPROC"     PROGRAM ADDRESS
EDITPARM DS   0A                  "EDITPROC"     PARAMETER  LIST
EDITPRM1 DS    A                      A(EXPLWA)
EDITPRM2 DS    A                      A(EDITCODE)
*
EXPLWA   DS    A                  EXIT WORK AREA ADDRESS
EXPLWL   DS    F                  EXIT WORK AREA LENGTH
EXPLRSV1 DS    H                  (RESERVED)
EXPLRC1  DS    H                  RETURN    CODE
EXPLRC2  DS    F                  REASON    CODE
EXPLARC  DS    F
EXPLSSNM DS    CL8
EXPLCONN DS    CL8
EXPLTYPE DS    CL8
*
EDITCODE DS    F                  FUNCTION CODE
EDITROW  DS    A                  ROW DESCRIPTOR ADDR - A(RFMTNFLD)
         DS    F                  (RESERVED)
EDITILTH DS    F                  ROW LENGTH   - INPUT
EDITIPTR DS    A                  ROW ADDRESS  - INPUT
EDITOLTH DS    F                  ROW LENGTH   - OUTPUT
EDITOPTR DS    A                  ROW ADDRESS  - OUTPUT
*
RFMTNFLD DS    F                  COLUMN COUNT
RFMTAFLD DS    A                  COLUMN DESCRIPTOR TABLE ADDRESS
RFMTTYPE DS    X                  ROW    TYPE
         DS    X                  (RESERVED)
*
SQLFORM  DS    C                  FORMAT OPTION ("F","U")
ACCMODE  DS    C                  ACCESS MODE   ("I")
*
PROCWA   DS    CL256              "EDITPROC"   WORK AREA
*
         DS   0D
         EXEC  SQL INCLUDE SQLCA
SQLCALEN EQU   *-SQLCA
*
SQLBUFFR DS    H,CL960            SQL STATEMENT LENGTH, STATEMENT TEXT
         ORG   SQLBUFFR
         DS    HL2
SQLTEXT  DS    CL976
         ORG
*
DDNWORKL EQU   *-DDNWORK
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        D B 2   F I E L D   F O R M A T   T A B L E   M A P          *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
FFMTTBL  DSECT                  FIELD  FORMAT  TABLE  ENTRY
*
FFMTFLEN DS    F                COLUMN LENGTH
FFMTFTYP DS    X                COLUMN DATA    TYPE
FFMTNULL DS    X                COLUMN DATA    ATTRIBUTE
FFMTFNAM DS    CL18             COLUMN NAME
*
FFMTTBLL EQU   *-FFMTTBL
                        EJECT
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
                        SPACE 2
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
                        SPACE 2
MODEOFF  EQU   296         ACCESS   MODE    OFFSET ("B" OR "S")
SUBSOFF  EQU   297         DB2 SUBSYSTEM ID OFFSET IN REFERENCE RECORD
TEXTOFF  EQU   301         DB2 SQL  TEXT    OFFSET IN REFERENCE RECORD
FORMOFF  EQU   1296        FORMAT   OPTION  OFFSET ("F" OR "U")
PROCOFF  EQU   1297        DB2 EDIT PROC    OFFSET
                        EJECT
         EXEC  SQL DECLARE SQLTXT00 STATEMENT
         EXEC  SQL DECLARE DB2CSR00 CURSOR FOR SQLTXT00
*
         EXEC  SQL DECLARE SQLTXT01 STATEMENT
         EXEC  SQL DECLARE DB2CSR01 CURSOR FOR SQLTXT01
*
         EXEC  SQL DECLARE SQLTXT02 STATEMENT
         EXEC  SQL DECLARE DB2CSR02 CURSOR FOR SQLTXT02
*
         EXEC  SQL DECLARE SQLTXT03 STATEMENT
         EXEC  SQL DECLARE DB2CSR03 CURSOR FOR SQLTXT03
*
         EXEC  SQL DECLARE SQLTXT04 STATEMENT
         EXEC  SQL DECLARE DB2CSR04 CURSOR FOR SQLTXT04
*
         EXEC  SQL DECLARE SQLTXT05 STATEMENT
         EXEC  SQL DECLARE DB2CSR05 CURSOR FOR SQLTXT05
*
         EXEC  SQL DECLARE SQLTXT06 STATEMENT
         EXEC  SQL DECLARE DB2CSR06 CURSOR FOR SQLTXT06
*
         EXEC  SQL DECLARE SQLTXT07 STATEMENT
         EXEC  SQL DECLARE DB2CSR07 CURSOR FOR SQLTXT07
*
         EXEC  SQL DECLARE SQLTXT08 STATEMENT
         EXEC  SQL DECLARE DB2CSR08 CURSOR FOR SQLTXT08
*
         EXEC  SQL DECLARE SQLTXT09 STATEMENT
         EXEC  SQL DECLARE DB2CSR09 CURSOR FOR SQLTXT09
*
         EXEC  SQL DECLARE SQLTXT10 STATEMENT
         EXEC  SQL DECLARE DB2CSR10 CURSOR FOR SQLTXT10
*
         EXEC  SQL DECLARE SQLTXT11 STATEMENT
         EXEC  SQL DECLARE DB2CSR11 CURSOR FOR SQLTXT11
*
         EXEC  SQL DECLARE SQLTXT12 STATEMENT
         EXEC  SQL DECLARE DB2CSR12 CURSOR FOR SQLTXT12
*
         EXEC  SQL DECLARE SQLTXT13 STATEMENT
         EXEC  SQL DECLARE DB2CSR13 CURSOR FOR SQLTXT13
*
         EXEC  SQL DECLARE SQLTXT14 STATEMENT
         EXEC  SQL DECLARE DB2CSR14 CURSOR FOR SQLTXT14
*
         EXEC  SQL DECLARE SQLTXT15 STATEMENT
         EXEC  SQL DECLARE DB2CSR15 CURSOR FOR SQLTXT15
*
         EXEC  SQL DECLARE SQLTXT16 STATEMENT
         EXEC  SQL DECLARE DB2CSR16 CURSOR FOR SQLTXT16
*
         EXEC  SQL DECLARE SQLTXT17 STATEMENT
         EXEC  SQL DECLARE DB2CSR17 CURSOR FOR SQLTXT17
*
         EXEC  SQL DECLARE SQLTXT18 STATEMENT
         EXEC  SQL DECLARE DB2CSR18 CURSOR FOR SQLTXT18
*
         EXEC  SQL DECLARE SQLTXT19 STATEMENT
         EXEC  SQL DECLARE DB2CSR19 CURSOR FOR SQLTXT19
*
         EXEC  SQL DECLARE SQLTXT20 STATEMENT
         EXEC  SQL DECLARE DB2CSR20 CURSOR FOR SQLTXT20
*
         EXEC  SQL DECLARE SQLTXT21 STATEMENT
         EXEC  SQL DECLARE DB2CSR21 CURSOR FOR SQLTXT21
*
         EXEC  SQL DECLARE SQLTXT22 STATEMENT
         EXEC  SQL DECLARE DB2CSR22 CURSOR FOR SQLTXT22
*
         EXEC  SQL DECLARE SQLTXT23 STATEMENT
         EXEC  SQL DECLARE DB2CSR23 CURSOR FOR SQLTXT23
*
         EXEC  SQL DECLARE SQLTXT24 STATEMENT
         EXEC  SQL DECLARE DB2CSR24 CURSOR FOR SQLTXT24
*
         EXEC  SQL DECLARE SQLTXT25 STATEMENT
         EXEC  SQL DECLARE DB2CSR25 CURSOR FOR SQLTXT25
*
         EXEC  SQL DECLARE SQLTXT26 STATEMENT
         EXEC  SQL DECLARE DB2CSR26 CURSOR FOR SQLTXT26
*
         EXEC  SQL DECLARE SQLTXT27 STATEMENT
         EXEC  SQL DECLARE DB2CSR27 CURSOR FOR SQLTXT27
*
         EXEC  SQL DECLARE SQLTXT28 STATEMENT
         EXEC  SQL DECLARE DB2CSR28 CURSOR FOR SQLTXT28
*
         EXEC  SQL DECLARE SQLTXT29 STATEMENT
         EXEC  SQL DECLARE DB2CSR29 CURSOR FOR SQLTXT29
*
         EXEC  SQL DECLARE SQLTXT30 STATEMENT
         EXEC  SQL DECLARE DB2CSR30 CURSOR FOR SQLTXT30
*
         EXEC  SQL DECLARE SQLTXT31 STATEMENT
         EXEC  SQL DECLARE DB2CSR31 CURSOR FOR SQLTXT31
*
         EXEC  SQL DECLARE SQLTXT32 STATEMENT
         EXEC  SQL DECLARE DB2CSR32 CURSOR FOR SQLTXT32
*
         EXEC  SQL DECLARE SQLTXT33 STATEMENT
         EXEC  SQL DECLARE DB2CSR33 CURSOR FOR SQLTXT33
*
         EXEC  SQL DECLARE SQLTXT34 STATEMENT
         EXEC  SQL DECLARE DB2CSR34 CURSOR FOR SQLTXT34
*
         EXEC  SQL DECLARE SQLTXT35 STATEMENT
         EXEC  SQL DECLARE DB2CSR35 CURSOR FOR SQLTXT35
*
         EXEC  SQL DECLARE SQLTXT36 STATEMENT
         EXEC  SQL DECLARE DB2CSR36 CURSOR FOR SQLTXT36
*
         EXEC  SQL DECLARE SQLTXT37 STATEMENT
         EXEC  SQL DECLARE DB2CSR37 CURSOR FOR SQLTXT37
*
         EXEC  SQL DECLARE SQLTXT38 STATEMENT
         EXEC  SQL DECLARE DB2CSR38 CURSOR FOR SQLTXT38
*
         EXEC  SQL DECLARE SQLTXT39 STATEMENT
         EXEC  SQL DECLARE DB2CSR39 CURSOR FOR SQLTXT39
*
         EXEC  SQL DECLARE SQLTXT40 STATEMENT
         EXEC  SQL DECLARE DB2CSR40 CURSOR FOR SQLTXT40
*
         EXEC  SQL DECLARE SQLTXT41 STATEMENT
         EXEC  SQL DECLARE DB2CSR41 CURSOR FOR SQLTXT41
*
         EXEC  SQL DECLARE SQLTXT42 STATEMENT
         EXEC  SQL DECLARE DB2CSR42 CURSOR FOR SQLTXT42
*
         EXEC  SQL DECLARE SQLTXT43 STATEMENT
         EXEC  SQL DECLARE DB2CSR43 CURSOR FOR SQLTXT43
*
         EXEC  SQL DECLARE SQLTXT44 STATEMENT
         EXEC  SQL DECLARE DB2CSR44 CURSOR FOR SQLTXT44
*
         EXEC  SQL DECLARE SQLTXT45 STATEMENT
         EXEC  SQL DECLARE DB2CSR45 CURSOR FOR SQLTXT45
*
         EXEC  SQL DECLARE SQLTXT46 STATEMENT
         EXEC  SQL DECLARE DB2CSR46 CURSOR FOR SQLTXT46
*
         EXEC  SQL DECLARE SQLTXT47 STATEMENT
         EXEC  SQL DECLARE DB2CSR47 CURSOR FOR SQLTXT47
*
         EXEC  SQL DECLARE SQLTXT48 STATEMENT
         EXEC  SQL DECLARE DB2CSR48 CURSOR FOR SQLTXT48
*
         EXEC  SQL DECLARE SQLTXT49 STATEMENT
         EXEC  SQL DECLARE DB2CSR49 CURSOR FOR SQLTXT49
*
         EXEC  SQL DECLARE SQLTXT50 STATEMENT
         EXEC  SQL DECLARE DB2CSR50 CURSOR FOR SQLTXT50
*
         EXEC  SQL DECLARE SQLTXT51 STATEMENT
         EXEC  SQL DECLARE DB2CSR51 CURSOR FOR SQLTXT51
*
         EXEC  SQL DECLARE SQLTXT52 STATEMENT
         EXEC  SQL DECLARE DB2CSR52 CURSOR FOR SQLTXT52
*
         EXEC  SQL DECLARE SQLTXT53 STATEMENT
         EXEC  SQL DECLARE DB2CSR53 CURSOR FOR SQLTXT53
*
         EXEC  SQL DECLARE SQLTXT54 STATEMENT
         EXEC  SQL DECLARE DB2CSR54 CURSOR FOR SQLTXT54
*
         EXEC  SQL DECLARE SQLTXT55 STATEMENT
         EXEC  SQL DECLARE DB2CSR55 CURSOR FOR SQLTXT55
*
         EXEC  SQL DECLARE SQLTXT56 STATEMENT
         EXEC  SQL DECLARE DB2CSR56 CURSOR FOR SQLTXT56
*
         EXEC  SQL DECLARE SQLTXT57 STATEMENT
         EXEC  SQL DECLARE DB2CSR57 CURSOR FOR SQLTXT57
*
         EXEC  SQL DECLARE SQLTXT58 STATEMENT
         EXEC  SQL DECLARE DB2CSR58 CURSOR FOR SQLTXT58
*
         EXEC  SQL DECLARE SQLTXT59 STATEMENT
         EXEC  SQL DECLARE DB2CSR59 CURSOR FOR SQLTXT59
*
         EXEC  SQL DECLARE SQLTXT60 STATEMENT
         EXEC  SQL DECLARE DB2CSR60 CURSOR FOR SQLTXT60
*
         EXEC  SQL DECLARE SQLTXT61 STATEMENT
         EXEC  SQL DECLARE DB2CSR61 CURSOR FOR SQLTXT61
*
         EXEC  SQL DECLARE SQLTXT62 STATEMENT
         EXEC  SQL DECLARE DB2CSR62 CURSOR FOR SQLTXT62
*
         EXEC  SQL DECLARE SQLTXT63 STATEMENT
         EXEC  SQL DECLARE DB2CSR63 CURSOR FOR SQLTXT63
*
         EXEC  SQL DECLARE SQLTXT64 STATEMENT
         EXEC  SQL DECLARE DB2CSR64 CURSOR FOR SQLTXT64
*
         EXEC  SQL DECLARE SQLTXT65 STATEMENT
         EXEC  SQL DECLARE DB2CSR65 CURSOR FOR SQLTXT65
*
         EXEC  SQL DECLARE SQLTXT66 STATEMENT
         EXEC  SQL DECLARE DB2CSR66 CURSOR FOR SQLTXT66
*
         EXEC  SQL DECLARE SQLTXT67 STATEMENT
         EXEC  SQL DECLARE DB2CSR67 CURSOR FOR SQLTXT67
*
         EXEC  SQL DECLARE SQLTXT68 STATEMENT
         EXEC  SQL DECLARE DB2CSR68 CURSOR FOR SQLTXT68
*
         EXEC  SQL DECLARE SQLTXT69 STATEMENT
         EXEC  SQL DECLARE DB2CSR69 CURSOR FOR SQLTXT69
*
         EXEC  SQL DECLARE SQLTXT70 STATEMENT
         EXEC  SQL DECLARE DB2CSR70 CURSOR FOR SQLTXT70
*
         EXEC  SQL DECLARE SQLTXT71 STATEMENT
         EXEC  SQL DECLARE DB2CSR71 CURSOR FOR SQLTXT71
*
         EXEC  SQL DECLARE SQLTXT72 STATEMENT
         EXEC  SQL DECLARE DB2CSR72 CURSOR FOR SQLTXT72
*
         EXEC  SQL DECLARE SQLTXT73 STATEMENT
         EXEC  SQL DECLARE DB2CSR73 CURSOR FOR SQLTXT73
*
         EXEC  SQL DECLARE SQLTXT74 STATEMENT
         EXEC  SQL DECLARE DB2CSR74 CURSOR FOR SQLTXT74
*
         EXEC  SQL DECLARE SQLTXT75 STATEMENT
         EXEC  SQL DECLARE DB2CSR75 CURSOR FOR SQLTXT75
*
         EXEC  SQL DECLARE SQLTXT76 STATEMENT
         EXEC  SQL DECLARE DB2CSR76 CURSOR FOR SQLTXT76
*
         EXEC  SQL DECLARE SQLTXT77 STATEMENT
         EXEC  SQL DECLARE DB2CSR77 CURSOR FOR SQLTXT77
*
         EXEC  SQL DECLARE SQLTXT78 STATEMENT
         EXEC  SQL DECLARE DB2CSR78 CURSOR FOR SQLTXT78
*
         EXEC  SQL DECLARE SQLTXT79 STATEMENT
         EXEC  SQL DECLARE DB2CSR79 CURSOR FOR SQLTXT79
*
         EXEC  SQL DECLARE SQLTXT80 STATEMENT
         EXEC  SQL DECLARE DB2CSR80 CURSOR FOR SQLTXT80
*
         EXEC  SQL DECLARE SQLTXT81 STATEMENT
         EXEC  SQL DECLARE DB2CSR81 CURSOR FOR SQLTXT81
*
         EXEC  SQL DECLARE SQLTXT82 STATEMENT
         EXEC  SQL DECLARE DB2CSR82 CURSOR FOR SQLTXT82
*
         EXEC  SQL DECLARE SQLTXT83 STATEMENT
         EXEC  SQL DECLARE DB2CSR83 CURSOR FOR SQLTXT83
*
         EXEC  SQL DECLARE SQLTXT84 STATEMENT
         EXEC  SQL DECLARE DB2CSR84 CURSOR FOR SQLTXT84
*
         EXEC  SQL DECLARE SQLTXT85 STATEMENT
         EXEC  SQL DECLARE DB2CSR85 CURSOR FOR SQLTXT85
*
         EXEC  SQL DECLARE SQLTXT86 STATEMENT
         EXEC  SQL DECLARE DB2CSR86 CURSOR FOR SQLTXT86
*
         EXEC  SQL DECLARE SQLTXT87 STATEMENT
         EXEC  SQL DECLARE DB2CSR87 CURSOR FOR SQLTXT87
*
         EXEC  SQL DECLARE SQLTXT88 STATEMENT
         EXEC  SQL DECLARE DB2CSR88 CURSOR FOR SQLTXT88
*
         EXEC  SQL DECLARE SQLTXT89 STATEMENT
         EXEC  SQL DECLARE DB2CSR89 CURSOR FOR SQLTXT89
*
         EXEC  SQL DECLARE SQLTXT90 STATEMENT
         EXEC  SQL DECLARE DB2CSR90 CURSOR FOR SQLTXT90
*
         EXEC  SQL DECLARE SQLTXT91 STATEMENT
         EXEC  SQL DECLARE DB2CSR91 CURSOR FOR SQLTXT91
*
         EXEC  SQL DECLARE SQLTXT92 STATEMENT
         EXEC  SQL DECLARE DB2CSR92 CURSOR FOR SQLTXT92
*
         EXEC  SQL DECLARE SQLTXT93 STATEMENT
         EXEC  SQL DECLARE DB2CSR93 CURSOR FOR SQLTXT93
*
         EXEC  SQL DECLARE SQLTXT94 STATEMENT
         EXEC  SQL DECLARE DB2CSR94 CURSOR FOR SQLTXT94
*
         EXEC  SQL DECLARE SQLTXT95 STATEMENT
         EXEC  SQL DECLARE DB2CSR95 CURSOR FOR SQLTXT95
*
         EXEC  SQL DECLARE SQLTXT96 STATEMENT
         EXEC  SQL DECLARE DB2CSR96 CURSOR FOR SQLTXT96
*
         EXEC  SQL DECLARE SQLTXT97 STATEMENT
         EXEC  SQL DECLARE DB2CSR97 CURSOR FOR SQLTXT97
*
         EXEC  SQL DECLARE SQLTXT98 STATEMENT
         EXEC  SQL DECLARE DB2CSR98 CURSOR FOR SQLTXT98
*
         EXEC  SQL DECLARE SQLTXT99 STATEMENT
         EXEC  SQL DECLARE DB2CSR99 CURSOR FOR SQLTXT99
                        EJECT
         PRINT NOGEN
*
GVBUR30  RMODE 24
GVBUR30  AMODE 31
         ENTRY DSNHLI
GVBUR30  CSECT
*
         j     code
UR30EYE  GVBEYE GVBUR30
static   loctr
code     loctr
         STM   R14,R12,RSA14(R13) SAVE  CALLER'S  REGISTERS
*
         LR    R11,R15            SET   PROGRAM   BASE  REGISTERS
         LA    R12,4095(,R11)
         LA    R12,1(,R12)
         USING GVBUR30,R11,R12
*
         L     R8,0(,R1)          LOAD  PARMAREA  ADDRESS
         USING PARMAREA,R8
*
         XC    PARMRC,PARMRC      INITIALIZE RETURN  CODE
         XC    PARMERRC,PARMERRC  INITIALIZE ERROR   CODE
*
         LR    R10,R13            SAVE  OLD  RSA  ADDRESS
         L     R13,PARMWPTR       LOAD  WORKAREA  ADDRESS
         USING WORKAREA,R13
*
         LTR   R13,R13            WORK  AREA    ALLOCATED   ???
         BRP   CHAIN              YES   BYPASS  ALLOCATION
                        SPACE 3
***********************************************************************
*  ALLOCATE "GVBUR30" WORK AREA                                       *
***********************************************************************
ALLOCWRK LR    R2,R1              SAVE  PARAMETER LIST  ADDRESS
*
         LHI   R0,WORKLEN         LOAD  WORKAREA  LENGTH
         GETMAIN R,LV=(0),LOC=(ANY)
         ST    R1,PARMWPTR        SAVE  WORKAREA  ADDRESS
         LR    R13,R1             SET   WORKAREA  BASE  REGISTER
*
         LR    R0,R13             LOAD  TARGET    ADDRESS
         LHI   R1,WORKLEN         LOAD  TARGET    LENGTH
         SR    R14,R14
         SR    R15,R15
         MVCL  R0,R14             ZERO  WORKAREA
*
         ST    R10,RSABP(,R13)    SET   BACKWARD  POINTER IN NEW RSA
         ST    R13,RSAFP(,R10)    SET   FORWARD   POINTER IN OLD RSA
*
*        OPEN  (SNAPDCB,(OUTPUT)),MODE=31
*
         BRAS  R10,INITWORK       INITIALIZE WORK AREA
*
*        LA    R10,WORKLEN(,R13)
*        SNAP  DCB=SNAPDCB,PDATA=(REGS),ID=200,STORAGE=((R13),(R10))
*
         LR    R1,R2              RESTORE PARAMETER  LIST  ADDR
         BRU   SELFUNC            SELECT  FUNCTION
                        EJECT
***********************************************************************
*  CHAIN REGISTER SAVE AREAS TOGETHER                                 *
*  BRANCH TO REQUESTED FUNCTION                                       *
***********************************************************************
CHAIN    ST    R10,RSABP(,R13)    SET  BACKWARD POINTER IN NEW RSA
         ST    R13,RSAFP(,R10)    SET  FORWARD  POINTER IN OLD RSA
*
*        LA    R10,WORKLEN(,R13)
*        SNAP  DCB=SNAPDCB,PDATA=(REGS),ID=205,STORAGE=((R13),(R10))
                        SPACE 3
SELFUNC  ST    R1,SAVEPARM        SAVE PARMLIST ADDRESS
*
         LH    R15,PARMFC         LOAD FUNCTION CODE
         CH    R15,H008           VALID   RANGE ???
         BRH   BADFUNC
*
         LA    R14,BRNCHTBL(R15)  SWITCH  TO 31-BIT  MODE (IF NOT)
         O     R14,MODE31
         BSM   0,R14
*
BRNCHTBL BRU   FUNCOP             00 -  OPEN
         BRU   FUNCCL             04 -  CLOSE
         BRU   FUNCRS             08 -  READ  SEQUENTIAL  (LOCATE)
*
BADFUNC  MVC   PARMRC,H016        INDICATE TERMINAL ERROR
         MVC   PARMERRC,H002      02 - BAD FUNCTION CODE
         BRU   RETURN             RETURN - INDICATE ERROR
                        EJECT
***********************************************************************
*                                                                     *
* FUNCTION:   OPEN THE DB2 CURSOR                                     *
*                                                                     *
***********************************************************************
*
FUNCOP   LA    R1,PARMDDN         DDNAME WORKAREA ALREADY ALLOCATED ???
         BRAS  R10,LOCDDN
         USING DDNWORK,R7
         LTR   R7,R7
         BRP   OPDB2CON
*
         LA    R1,PARMDDN         NO  -  ALLOCATE NEW DDNAME WORK  AREA
         BRAS  R10,ALLOCATE
                        SPACE 3
OPDB2CON BRAS  R10,DB2CONN        ESTABLISH   DB2 CONNECTION
*
         L     R0,WORKROWL        RETURN ROW  LEN
         STH   R0,PARMRECL
*
*        LA    R15,DDNWORKL(,R7)
*        SNAP  DCB=SNAPDCB,PDATA=(REGS),ID=251,STORAGE=((R7),(R15))
*
OPBEGIN  DS    0H
*  "BEGIN FETCHING ROWS"   MESSAGE
         GVBMSG WTO,MSGNO=FETCH_ROWS,SUBNO=2,                          +
               SUB1=(modname,8),                                       +
               SUB2=(WORKDDN,8),                                       +
               MSGBUFFER=(WORKMSG,L'WORKMSG),                          +
               MF=(E,MSG_AREA)
*
         BRU   RETURN             RETURN
*
         DROP  R7
                        EJECT
***********************************************************************
*                                                                     *
* FUNCTION:   CLOSE THE DB2 CURSOR                                    *
*                                                                     *
***********************************************************************
*
FUNCCL   LA    R1,PARMDDN         DDNAME WORKAREA ALREADY ALLOCATED ???
         BRAS  R10,LOCDDN
         USING DDNWORK,R7
         LTR   R7,R7
         BRNP  NOTOPEN
*
*        SNAP  DCB=SNAPDCB,PDATA=(REGS),ID=230
*
         BRAS  R10,DB2CLOSE       DISCONNECT
*
CLEXIT   EQU   *
*        LA    R10,WORKLEN(,R13)
*        SNAP  DCB=SNAPDCB,PDATA=(REGS),ID=235,STORAGE=((R13),(R10))
*
         BRU   RETURN             RETURN
                        SPACE 3
NOTOPEN  MVC   PARMRC,H004        WARNING
         MVC   PARMERRC,H007      07 - FILE NEVER   OPENED
*
         BRU   RETURN             RETURN
                        SPACE 3
NOTFOUND MVC   PARMRC,H008        END-OF-TABLE (NOT FOUND)
         BRU   RETURN             RETURN
*
         DROP  R7
                        EJECT
***********************************************************************
*                                                                     *
*  THIS SECTION OF CODE IS ENTERED WHEN READING THE NEXT ROW IN A     *
*  DB2 TABLE USING AN "SQL" STATEMENT                                 *
*                                                                     *
***********************************************************************
*
FUNCRS   L     R7,WORKDPTR        LOAD ADDRESS OF FIRST DDNAME WORKAREA
         USING DDNWORK,R7
*
RSLOCATE LTR   R7,R7              END-OF-CHAIN ???
         BRNP  NOTOPEN            YES - DDNAME NOT  FOUND
*
         CLC   WORKDDN,PARMDDN    CORRECT WORK AREA ???
         BRE   RSNXTROW           YES - RETURN NEXT RECORD
         L     R7,WORKNEXT        ADVANCE TO NEXT WORK AREA
         BRU   RSLOCATE           LOOP UNTIL CORRECT ENTRY FOUND
*
RSNXTROW DS    0H
*
         L     R6,WORKSQLW        LOAD SQL WORK AREA   ADDRESS
         USING SQLDSECT,R6
*
         L     R5,WORKSQLD        LOAD SQL DESCRIPTOR  AREA ADDRESS
         USING SQLDA,R5
*
         MVC   CAFFUNC,FETCH      INDICATE CURRENT CAF FUNCTION
*
*        L     R15,WORKINDX
*        SNAP  DCB=SNAPDCB,PDATA=(REGS),ID=252
*
         L     R15,WORKINDX
         L     R9,FETCHTBL(R15)
         BR    R9
*
FETCHTBL DC    A(FETCH00)         FETCH USING CURSOR 00
         DC    A(FETCH01)         FETCH USING CURSOR 01
         DC    A(FETCH02)         FETCH USING CURSOR 02
         DC    A(FETCH03)
         DC    A(FETCH04)
         DC    A(FETCH05)
         DC    A(FETCH06)
         DC    A(FETCH07)
         DC    A(FETCH08)
         DC    A(FETCH09)
         DC    A(FETCH10)
         DC    A(FETCH11)
         DC    A(FETCH12)
         DC    A(FETCH13)
         DC    A(FETCH14)
         DC    A(FETCH15)
         DC    A(FETCH16)
         DC    A(FETCH17)
         DC    A(FETCH18)
         DC    A(FETCH19)
         DC    A(FETCH20)
         DC    A(FETCH21)
         DC    A(FETCH22)
         DC    A(FETCH23)
         DC    A(FETCH24)
         DC    A(FETCH25)
         DC    A(FETCH26)
         DC    A(FETCH27)
         DC    A(FETCH28)
         DC    A(FETCH29)
         DC    A(FETCH30)
         DC    A(FETCH31)
         DC    A(FETCH32)
         DC    A(FETCH33)
         DC    A(FETCH34)
         DC    A(FETCH35)
         DC    A(FETCH36)
         DC    A(FETCH37)
         DC    A(FETCH38)
         DC    A(FETCH39)
         DC    A(FETCH40)
         DC    A(FETCH41)
         DC    A(FETCH42)
         DC    A(FETCH43)
         DC    A(FETCH44)
         DC    A(FETCH45)
         DC    A(FETCH46)
         DC    A(FETCH47)
         DC    A(FETCH48)
         DC    A(FETCH49)
         DC    A(FETCH50)
         DC    A(FETCH51)
         DC    A(FETCH52)
         DC    A(FETCH53)
         DC    A(FETCH54)
         DC    A(FETCH55)
         DC    A(FETCH56)
         DC    A(FETCH57)
         DC    A(FETCH58)
         DC    A(FETCH59)
         DC    A(FETCH60)
         DC    A(FETCH61)
         DC    A(FETCH62)
         DC    A(FETCH63)
         DC    A(FETCH64)
         DC    A(FETCH65)
         DC    A(FETCH66)
         DC    A(FETCH67)
         DC    A(FETCH68)
         DC    A(FETCH69)
         DC    A(FETCH70)
         DC    A(FETCH71)
         DC    A(FETCH72)
         DC    A(FETCH73)
         DC    A(FETCH74)
         DC    A(FETCH75)
         DC    A(FETCH76)
         DC    A(FETCH77)
         DC    A(FETCH78)
         DC    A(FETCH79)
         DC    A(FETCH80)
         DC    A(FETCH81)
         DC    A(FETCH82)
         DC    A(FETCH83)
         DC    A(FETCH84)
         DC    A(FETCH85)
         DC    A(FETCH86)
         DC    A(FETCH87)
         DC    A(FETCH88)
         DC    A(FETCH89)
         DC    A(FETCH90)
         DC    A(FETCH91)
         DC    A(FETCH92)
         DC    A(FETCH93)
         DC    A(FETCH94)
         DC    A(FETCH95)
         DC    A(FETCH96)
         DC    A(FETCH97)
         DC    A(FETCH98)
         DC    A(FETCH99)
*
FETCHRC  LTR   R15,R15            SUCCESSFUL ???
         BRZ   RSFETROW           YES - CONTINUE
*
         L     R15,SQLCODE
         CVD   R15,DBLWORK         STORE THE RETURN CODE (PACKED)
         MVC   REASCODE(8),SPACES
         UNPK  REASCODE(4),DBLWORK+4(4)
         OI    REASCODE+3,X'F0'    FORCE A DISPLAYABLE ZONE
         LTR   R15,R15             NEGATIVE  SQLCODE ???
         BP    *+8                 NO  - BYPASS MOVE
         MVI   REASCODE+0,C'-'
*
         LA    R14,SQL_FETCH_FAIL
         XC    PARMRECA,PARMRECA  INDICATE NO RECORD
         BRU   RTNERROR
*
RSFETROW LA    R4,SQLVAR          LOAD SQL DESCRIPTOR AREA ADDRESS
         USING SQLVARN,R4
*
         L     R2,WORKROWA        LOAD HOST  VARIABLE ADDRESS (1ST COL)
         ST    R2,PARMRECA
         L     R0,WORKROWL
         STH   R0,PARMRECL
*
*        LR    R10,R2
*        A     R10,WORKROWL
*        BCTR  R10,0
*        SNAP  DCB=SNAPDCB,ID=206,STORAGE=((R2),(R10))
*
         AP    WORKCNT,P001       INCREMENT  TRANSACTION RECORD COUNT
*
         BRU   RETURN             RETURN
*
         DROP  R4
         DROP  R5
         DROP  R6
                        EJECT
***********************************************************************
*  RETURN TO CALLING PROGRAM                                          *
***********************************************************************
RETURN   EQU   *
*        LA    R9,PARMAREA
*        LA    R10,PARMWPTR+3
*        SNAP  DCB=SNAPDCB,PDATA=(REGS),ID=210,                       X
*              STORAGE=((R9),(R10))
         LH    R15,PARMRC         LOAD    RETURN CODE
         L     R13,RSABP(,R13)    RESTORE REGISTER 13
         L     R14,RSA14(,R13)    RESTORE REGISTER 14
         LM    R0,R12,RSA0(R13)   RESTORE REGISTERS 0,1, ... ,12
         BSM   0,R14              RETURN
                        SPACE 3
***********************************************************************
*  DISPLAY ERROR MESSAGE                                              *
***********************************************************************
RTNERROR DS    0h
*
         GVBMSG WTO,MSGNO=(R14),SUBNO=3,                               +
               SUB1=(modname,8),                                       +
               SUB2=(REASCODE,8),                                      +
               SUB3=(WORKDDN,8),                                       +
               MSGBUFFER=(WORKMSG,L'WORKMSG),                          +
               MF=(E,MSG_AREA)
         J     RTNXTRA
*
RTNERROR2 DS    0h  ERROR MESSAGE WITH ONLY 2 PARMS
*
         GVBMSG WTO,MSGNO=(R14),SUBNO=2,                               +
               SUB1=(modname,8),                                       +
               SUB2=(REASCODE,8),                                      +
               MSGBUFFER=(WORKMSG,L'WORKMSG),                          +
               MF=(E,MSG_AREA)
*
RTNXTRA  DS    0h
*
* If there is any info from a translated SQLCODE, write that to log
*
         if oc,workmsg(4),workmsg,nz
           if clc,workmsg(4),eq,ffff  did XLATE fail?

             GVBMSG WTO,MSGNO=SQL_XLATE_FAIL,SUBNO=2,                  +
               SUB1=(modname,8),                                       +
               SUB2=(WORKMSG+4,4),                                     +
               MSGBUFFER=(WORKMSG,L'WORKMSG),                          +
               MF=(E,MSG_AREA)
           else
             GVBMSG WTO,MSGNO=DB2_TRANSLATED_SQLCODE,SUBNO=2,          +
               SUB1=(modname,8),                                       +
               SUB2=(WORKMSG,64),                                      +
               MSGBUFFER=(WORKMSG,L'WORKMSG),                          +
               MF=(E,MSG_AREA)
           endif
         endif
*
         LA    R15,8              SET  RETURN CODE REGISTER TO 8
         STH   R15,PARMRC         INDICATE PERMANENT ERROR
         BRU   RETURN
*
         DROP  R7
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        I N I T I A L I Z E   W O R K   A R E A                      *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
INITWORK SR    R14,R14            SAVE  CALLER'S  ADDRESSING MODE
         BSM   R14,0
         ST    R14,WORKMODE
*
*        MVC   WTOPARM(WTOPARML),MDLWTO
*
         LOAD  EP=DSNALI          LOAD  DB2   ASSEMBLER INTERFACE
         ST    R0,DSNALI
*
         LOAD  EP=DSNHLI2         LOAD  DB2   HIGHER  LEVEL  LANG ADDR
         ST    R0,DSNHLI2
*
         BR    R10                RETURN
                        EJECT
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
*                                                                     *
*              - PREPARE                                              *
*                                                                     *
*                DYNAMICALLY BIND THE SQL STATEMENT FOR EXECUTION.    *
*                                                                     *
*              - OPEN CURSOR                                          *
*                                                                     *
*                INITIALIZE THE DB2 CURSOR ASSOCIATED WITH THE        *
*                SQL STATEMENT.                                       *
*                                                                     *
*              - TRANSLATE                                            *
*                                                                     *
*                PROVIDE A DISPLAYABLE TEXT MESSAGE FOR A DB2         *
*                RESOURCE UNAVAILABLE ERROR CONDITION.                *
*                                                                     *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
                        SPACE 3
**********************************************************************
* CONNECT TO DB2                                                     *
**********************************************************************
         USING DDNWORK,R7
*
DB2CONN  L     R6,WORKSQLW        LOAD SQL WORK AREA ADDRESS
         USING SQLDSECT,R6
*
         L     R0,WORKACTV         DUPLICATE   CONNECT ???
         C     R0,F1
         BRH   CONNPREP
*
         LA    R1,CONNECT          CONNECT    FUNCTION
         LA    R2,DBSUBSYS         SUBSYSTEM  NAME
         LA    R3,DBTRMECB         DB2 STARTUP     ECB
         LA    R4,DBSTRECB         DB2 TERMINATION ECB
         LA    R9,RIBPTR           RELEASE INFORMATION BLOCK
         STM   R1,R4,CALLP01
         ST    R9,CALLP05
         OI    CALLP05,X'80'
*
         MVC   CAFFUNC,CONNECT     INDICATE CURRENT CAF FUNCTION
*
         LA    R1,CALLPARM         CALL "DSNALI" - CONNECT
         L     R15,DSNALI
         BASR  R14,R15
*
         LTR   R15,R15             SUCCESSFUL  CONNECT ???
         BRZ   CONNOPEN            YES - CONTINUE
*
         LA    R14,DB2_CONNECT_FAIL NO  - CONNECT  FAILED
         ST    R0,PACKCODE         STORE THE REASON CODE
         UNPK  REASCODE(9),PACKCODE(5)
         TR    REASCODE(8),XTAB
         BRU   RTNERROR
                        SPACE 3
**********************************************************************
* OPEN THREAD                                                        *
**********************************************************************
CONNOPEN LA    R1,OPEN             OPEN    THREAD  FUNCTION
         LA    R2,DBSUBSYS         SUBSYSTEM NAME
         LA    R3,DB2PLAN          DB2PLAN   NAME
         STM   R1,R3,CALLP01
         OI    CALLP03,X'80'
*
         MVC   CAFFUNC,OPEN        INDICATE CURRENT CAF FUNCTION
*
         LA    R1,CALLPARM         CALL "DSNALI" - OPEN THREAD
         L     R15,DSNALI
         BASR  R14,R15
*
         LA    R14,DB2_OPEN_THREAD_FAIL ASSUME OPEN FAILED
         LTR   R15,R15             SUCCESSFUL OPEN ???
         BRNZ  XLATEERR            NO  - INDICATE  ERROR
*        SNAP  DCB=SNAPDCB,PDATA=(REGS),ID=201
                        SPACE 3
**********************************************************************
* SET DEGREE FOR PARALLEL PROCESSING                                 *
**********************************************************************
*ONNDEG  MVC   CAFFUNC,SETDEG      INDICATE CURRENT CAF FUNCTION
*
*        EXEC  SQL SET CURRENT DEGREE = 'ANY'
*
*        LTR   R15,R15             SUCCESSFUL OPEN ???
*        BRZ   CONNPREP            NO  - INDICATE  ERROR
*        LA    R1,ERR#10           ASSUME SET FAILED
*        MVC   REASCODE(8),WORKDDN
*        BRU   RTNERROR            NO  - INDICATE  ERROR
                        SPACE 3
**********************************************************************
* PREPARE(BIND) THE SQL STATEMENT                                    *
**********************************************************************
CONNPREP L     R5,WORKSQLD         LOAD "SQLDA" BASE REGISTER
         USING SQLDA,R5
*
         MVC   CAFFUNC,PREPARE     INDICATE CURRENT CAF FUNCTION
*
*        LA    R15,DDNWORKL(,R7)
*        SNAP  DCB=SNAPDCB,PDATA=(REGS),ID=250,STORAGE=((R7),(R15))
*
         L     R15,WORKINDX
         L     R9,CONNPTBL(R15)
         BR    R9
*
CONNPTBL DC    A(CONNTX00)         PREPARE  SQL TEXT 00
         DC    A(CONNTX01)         PREPARE  SQL TEXT 01
         DC    A(CONNTX02)         PREPARE  SQL TEXT 02
         DC    A(CONNTX03)
         DC    A(CONNTX04)
         DC    A(CONNTX05)
         DC    A(CONNTX06)
         DC    A(CONNTX07)
         DC    A(CONNTX08)
         DC    A(CONNTX09)
         DC    A(CONNTX10)
         DC    A(CONNTX11)
         DC    A(CONNTX12)
         DC    A(CONNTX13)
         DC    A(CONNTX14)
         DC    A(CONNTX15)
         DC    A(CONNTX16)
         DC    A(CONNTX17)
         DC    A(CONNTX18)
         DC    A(CONNTX19)
         DC    A(CONNTX20)
         DC    A(CONNTX21)
         DC    A(CONNTX22)
         DC    A(CONNTX23)
         DC    A(CONNTX24)
         DC    A(CONNTX25)
         DC    A(CONNTX26)
         DC    A(CONNTX27)
         DC    A(CONNTX28)
         DC    A(CONNTX29)
         DC    A(CONNTX30)
         DC    A(CONNTX31)
         DC    A(CONNTX32)
         DC    A(CONNTX33)
         DC    A(CONNTX34)
         DC    A(CONNTX35)
         DC    A(CONNTX36)
         DC    A(CONNTX37)
         DC    A(CONNTX38)
         DC    A(CONNTX39)
         DC    A(CONNTX40)
         DC    A(CONNTX41)
         DC    A(CONNTX42)
         DC    A(CONNTX43)
         DC    A(CONNTX44)
         DC    A(CONNTX45)
         DC    A(CONNTX46)
         DC    A(CONNTX47)
         DC    A(CONNTX48)
         DC    A(CONNTX49)
         DC    A(CONNTX50)
         DC    A(CONNTX51)
         DC    A(CONNTX52)
         DC    A(CONNTX53)
         DC    A(CONNTX54)
         DC    A(CONNTX55)
         DC    A(CONNTX56)
         DC    A(CONNTX57)
         DC    A(CONNTX58)
         DC    A(CONNTX59)
         DC    A(CONNTX60)
         DC    A(CONNTX61)
         DC    A(CONNTX62)
         DC    A(CONNTX63)
         DC    A(CONNTX64)
         DC    A(CONNTX65)
         DC    A(CONNTX66)
         DC    A(CONNTX67)
         DC    A(CONNTX68)
         DC    A(CONNTX69)
         DC    A(CONNTX70)
         DC    A(CONNTX71)
         DC    A(CONNTX72)
         DC    A(CONNTX73)
         DC    A(CONNTX74)
         DC    A(CONNTX75)
         DC    A(CONNTX76)
         DC    A(CONNTX77)
         DC    A(CONNTX78)
         DC    A(CONNTX79)
         DC    A(CONNTX80)
         DC    A(CONNTX81)
         DC    A(CONNTX82)
         DC    A(CONNTX83)
         DC    A(CONNTX84)
         DC    A(CONNTX85)
         DC    A(CONNTX86)
         DC    A(CONNTX87)
         DC    A(CONNTX88)
         DC    A(CONNTX89)
         DC    A(CONNTX90)
         DC    A(CONNTX91)
         DC    A(CONNTX92)
         DC    A(CONNTX93)
         DC    A(CONNTX94)
         DC    A(CONNTX95)
         DC    A(CONNTX96)
         DC    A(CONNTX97)
         DC    A(CONNTX98)
         DC    A(CONNTX99)
*
CONNSTRC L     R15,SQLCODE         SUCCESSFUL OPEN ???
         LTR   R15,R15
         BRZ   CONNALLO            YES - CONTINUE
*
         CVD   R15,DBLWORK         STORE THE RETURN CODE (PACKED)
         MVC   REASCODE(8),SPACES
         UNPK  REASCODE(4),DBLWORK+4(4)
         OI    REASCODE+3,X'F0'    FORCE A DISPLAYABLE ZONE
         LTR   R15,R15             NEGATIVE  SQLCODE ???
         BRP   *+8                 NO  - BYPASS MOVE
         MVI   REASCODE+0,C'-'
*
         LA    R14,SQL_PREPARE_FAIL NO  - PREPARE FAILED
*
         BRU   RTNERROR            INDICATE  ERROR
                        SPACE 3
**********************************************************************
* ALLOCATE HOST VARIABLE/INDICATOR AREA                              *
**********************************************************************
*
CONNALLO LH    R3,SQLD             INITIALIZE LOOP  COUNTER
         LTR   R3,R3
         BRP   CONNINIT            YES - CONTINUE
         LA    R14,SQL_NO_COLUMNS  NO  - BAD  SQL   TEXT (NO COLUMNS)
         MVC   REASCODE(8),WORKDDN
         BRU   RTNERROR2
*
CONNINIT LA    R4,SQLVAR           INITIALIZE DSECT BASE REGISTER
         USING SQLVARN,R4
*
         LA    R0,FFMTTBLL         ALLOCATE   FIELD FORMAT  TABLE
         LR    R1,R3
         MR    R0,R0
         LR    R0,R1
         LR    R15,R0
         AY    R15,PARMMEMS      ADD TO TOTAL MEMORY USEAGE
         STY   R15,PARMMEMS      AND SAVE IT
         GETMAIN RU,LV=(0),LOC=(ANY)
         ST    R1,RFMTAFLD         SAVE COLUMN DESCRIPTION  TABLE ADDR
         ST    R3,RFMTNFLD         SAVE COLUMN COUNT
         LR    R2,R1
         USING FFMTTBL,R2
*
         XC    WORKROWL,WORKROWL   ZERO   ROW LENGTH
CONNVLP1 BRAS  R9,GETLEN           GET LENGTH OF OUTPUT DATA/INDICATORS
         CLI   SQLFORM,C'U'        UNFORMATTED OUTPUT ???
         BRNE  *+6                 NO  - BYPASS  OVERRIDE
         SR    R14,R14             YES - SET BRANCH TABLE INDEX = 0
         STH   R14,SQLIND+0        SAVE  BRNCH TABLE OFFSET (LEFT HALF)
         STH   R0,SQLIND+2         SAVE  IND  LENGTH       (RIGHT HALF)
         ST    R1,SQLDATA          SAVE  DATA LENGTH
         L     R14,WORKROWL
         AR    R14,R0              SUM INDICATOR    LENGTHS
         AR    R14,R1              SUM COLUMN DATA  LENGTHS
         ST    R14,WORKROWL        SAVE DB2 ROW SIZE
         LA    R4,SQLSIZV(,R4)     ADVANCE  TO NEXT "SQLDA" VARIABLE
         LA    R2,FFMTTBLL(,R2)    ADVANCE  TO NEXT COLUMN  DESCRIPTOR
         BRCT  R3,CONNVLP1         LOOP THROUGH ALL COLUMNS
                        SPACE 3
         L     R0,WORKROWL
         ST    R0,RECLEN          ALLOCATE  FORMATTED  RECORD AREA
         SLL   R0,1                   (GET  TWO AREAS - "EDITPROC")
         LR    R15,R0
         AY    R15,PARMMEMS      ADD TO TOTAL MEMORY USEAGE
         STY   R15,PARMMEMS      AND SAVE IT
         GETMAIN R,LV=(0),LOC=(ANY)
         ST    R1,RECADDR         SAVE FORMATTED RECORD ADDRESS
                        SPACE 1
         L     R0,RECLEN
         ST    R0,EDITOLTH
         A     R0,RECADDR
         ST    R0,EDITOPTR
*
*        L     R2,RFMTAFLD
*        L     R0,RFMTNFLD
*        LA    R1,FFMTTBLL
*        MR    R0,R0
*        LA    R9,0(R2,R1)
*        BCTR  R9,0
*        SNAP  DCB=SNAPDCB,PDATA=(REGS),ID=202,STORAGE=((R2),(R9))
*
*        L     R9,SQLDABC
*        AR    R9,R5
*        BCTR  R9,0
*        L     R15,WORKROWL
*        SNAP  DCB=SNAPDCB,ID=203,STORAGE=((R5),(R9))
                        SPACE 3
**********************************************************************
* OPEN DB2 CURSOR                                                    *
**********************************************************************
         MVC   CAFFUNC,OPEN        INDICATE CURRENT CAF FUNCTION
*
         L     R15,WORKINDX
         L     R9,CONNOTBL(R15)
         BR    R9
*
CONNOTBL DC    A(CONNCS00)        OPEN CURSOR 00
         DC    A(CONNCS01)        OPEN CURSOR 01
         DC    A(CONNCS02)        OPEN CURSOR 02
         DC    A(CONNCS03)
         DC    A(CONNCS04)
         DC    A(CONNCS05)
         DC    A(CONNCS06)
         DC    A(CONNCS07)
         DC    A(CONNCS08)
         DC    A(CONNCS09)
         DC    A(CONNCS10)
         DC    A(CONNCS11)
         DC    A(CONNCS12)
         DC    A(CONNCS13)
         DC    A(CONNCS14)
         DC    A(CONNCS15)
         DC    A(CONNCS16)
         DC    A(CONNCS17)
         DC    A(CONNCS18)
         DC    A(CONNCS19)
         DC    A(CONNCS20)
         DC    A(CONNCS21)
         DC    A(CONNCS22)
         DC    A(CONNCS23)
         DC    A(CONNCS24)
         DC    A(CONNCS25)
         DC    A(CONNCS26)
         DC    A(CONNCS27)
         DC    A(CONNCS28)
         DC    A(CONNCS29)
         DC    A(CONNCS30)
         DC    A(CONNCS31)
         DC    A(CONNCS32)
         DC    A(CONNCS33)
         DC    A(CONNCS34)
         DC    A(CONNCS35)
         DC    A(CONNCS36)
         DC    A(CONNCS37)
         DC    A(CONNCS38)
         DC    A(CONNCS39)
         DC    A(CONNCS40)
         DC    A(CONNCS41)
         DC    A(CONNCS42)
         DC    A(CONNCS43)
         DC    A(CONNCS44)
         DC    A(CONNCS45)
         DC    A(CONNCS46)
         DC    A(CONNCS47)
         DC    A(CONNCS48)
         DC    A(CONNCS49)
         DC    A(CONNCS50)
         DC    A(CONNCS51)
         DC    A(CONNCS52)
         DC    A(CONNCS53)
         DC    A(CONNCS54)
         DC    A(CONNCS55)
         DC    A(CONNCS56)
         DC    A(CONNCS57)
         DC    A(CONNCS58)
         DC    A(CONNCS59)
         DC    A(CONNCS60)
         DC    A(CONNCS61)
         DC    A(CONNCS62)
         DC    A(CONNCS63)
         DC    A(CONNCS64)
         DC    A(CONNCS65)
         DC    A(CONNCS66)
         DC    A(CONNCS67)
         DC    A(CONNCS68)
         DC    A(CONNCS69)
         DC    A(CONNCS70)
         DC    A(CONNCS71)
         DC    A(CONNCS72)
         DC    A(CONNCS73)
         DC    A(CONNCS74)
         DC    A(CONNCS75)
         DC    A(CONNCS76)
         DC    A(CONNCS77)
         DC    A(CONNCS78)
         DC    A(CONNCS79)
         DC    A(CONNCS80)
         DC    A(CONNCS81)
         DC    A(CONNCS82)
         DC    A(CONNCS83)
         DC    A(CONNCS84)
         DC    A(CONNCS85)
         DC    A(CONNCS86)
         DC    A(CONNCS87)
         DC    A(CONNCS88)
         DC    A(CONNCS89)
         DC    A(CONNCS90)
         DC    A(CONNCS91)
         DC    A(CONNCS92)
         DC    A(CONNCS93)
         DC    A(CONNCS94)
         DC    A(CONNCS95)
         DC    A(CONNCS96)
         DC    A(CONNCS97)
         DC    A(CONNCS98)
         DC    A(CONNCS99)
*
CONNCSRC DS    0H
         LTR   R15,R15             SUCCESSFUL OPEN ???
         BRZ   CONNEXIT            YES - EXIT
*
         CVD   R15,DBLWORK         STORE THE RETURN CODE (PACKED)
         MVC   REASCODE(8),SPACES
         UNPK  REASCODE(4),DBLWORK+4(4)
         OI    REASCODE+3,X'F0'    FORCE A DISPLAYABLE ZONE
         LTR   R15,R15             NEGATIVE  SQLCODE ???
         BP    *+8                 NO  - BYPASS MOVE
         MVI   REASCODE+0,C'-'
*
*        MVC   REASCODE(8),WORKDDN
         LA    R14,SQL_OPEN_FAIL   OPEN CURSOR FAILED
         BRU   RTNERROR            NO  - INDICATE ERROR
                        SPACE 3
CONNEXIT EQU   *
*        SNAP  DCB=SNAPDCB,PDATA=(REGS),ID=204
         BR    R10                RETURN
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        DETERMINE OUTPUT DATA LENGTH BASED ON COLUMN TYPE            *
*                                                                     *
*        UPON EXIT:                                                   *
*          R0 - NULL INDICATOR LENGTH (0 OR 1)                        *
*          R1 - DATA LENGTH (INCLUDING 2 BYTE LENGTH IF VARIABLE)     *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         USING SQLDA,R5
         USING SQLVARN,R4
*
GETLEN   LH    R0,SQLTYPE          GET   THE COLUMN   TYPE
         SRL   R0,1                STRIP OFF NULLABLE INDICATOR BIT
         SLL   R0,1
*
         LH    R1,SQLLEN           GET THE LENGTH FOR SOME TYPES
*
         LA    R14,CHARTYPE        LOAD TABLE ENTRY ADDRESS
         CH    R0,CHARTYPE         IF THIS IS CHARACTER DATA, WE'RE SET
         BRE   GETFIXED            JUST CHECK FOR NULLABLE COLUMNS
*
         LA    R14,VARCTYPE        LOAD TABLE ENTRY ADDRESS
         CH    R0,VARCTYPE         IF THIS IS VARCHAR  DATA, WE'RE SET
         BRE   GETVAR              ADD    TWO FOR   VARIABLE
*
         LA    R14,VARLTYPE        LOAD TABLE ENTRY ADDRESS
         CH    R0,VARLTYPE         IF THIS IS VARCHAR  DATA, WE'RE SET
         BRE   GETVAR              ADD    TWO FOR   VARIABLE
*
         LH    R1,SQLLEN           GET LENGTH AGAIN
         SLL   R1,1                DOUBLE  IT FOR GRAPHIC
*
         LA    R14,GTYPE           LOAD TABLE ENTRY ADDRESS
         CH    R0,GTYPE            IF THIS IS GRAPHIC        WE'RE SET
         BRE   GETFIXED            JUST CHECK FOR NULLABLE COLUMNS
*
         LA    R14,VARGTYPE        LOAD TABLE ENTRY ADDRESS
         CH    R0,VARGTYPE         IF THIS IS VARGRAPHIC     WE'RE SET
         BRE   GETVAR              ADD    TWO FOR   VARIABLE
*
         LA    R14,LVARGTYP        LOAD TABLE ENTRY ADDRESS
         CH    R0,LVARGTYP         IF THIS IS LONG VARGRAPHIC WE'RE SET
         BRE   GETVAR              ADD    TWO FOR   VARIABLE
*
         LA    R1,2                SET UP TWO FOR SMALL INTEGER
         LA    R14,HWTYPE          LOAD TABLE ENTRY ADDRESS
         CH    R0,HWTYPE           IF THIS IS SMALLINT DATA, WE'RE SET
         BRE   GETFIXED            JUST CHECK FOR NULLABLE COLUMNS
*
         LA    R1,4                SET UP FOUR FOR INTEGER
         LA    R14,INTTYPE         LOAD TABLE ENTRY ADDRESS
         CH    R0,INTTYPE          IF THIS IS INTEGER DATA,  WE'RE SET
         BRE   GETFIXED            JUST CHECK FOR NULLABLE COLUMNS
*
         LH    R1,SQLLEN           GET LENGTH AGAIN
         LA    R14,FLOATYPE        LOAD TABLE ENTRY ADDRESS
         CH    R0,FLOATYPE         IF THIS IS FLOAT DATA,    WE'RE SET
         BRE   GETFIXED            JUST CHECK FOR NULLABLE COLUMNS
*
         SR    R1,R1               CLEAR  THE REGISTER
         IC    R1,SQLPRCSN         GET    THE PRECISION
         LA    R1,2(,R1)           ADD    TWO (ALLOW  FOR  SIGN)
         SRL   R1,1                DIVIDE  BY TWO TO  GET  BYTES
         LA    R14,DECTYPE         LOAD TABLE ENTRY ADDRESS
         CH    R0,DECTYPE          IF THIS IS FLOAT  DATA, SET EIGHT
         BRE   GETFIXED            JUST CHECK FOR NULLABLE COLUMNS
*
         LA    R15,4               INTERNAL   LENGTH
         LH    R1,SQLLEN           GET LENGTH AGAIN (EXTERNAL)
         LA    R14,DATETYP         LOAD TABLE ENTRY ADDRESS
         CH    R0,DATETYP          IF THIS IS DATE DATA,
         BRE   GETINTRN            JUST CHECK FOR NULLABLE COLUMNS
*
         LA    R15,3               INTERNAL   LENGTH
         LH    R1,SQLLEN           GET LENGTH AGAIN (EXTERNAL)
         LA    R14,TIMETYP         LOAD TABLE ENTRY ADDRESS
         CH    R0,TIMETYP          IF THIS IS TIME DATA,
         BRE   GETINTRN            JUST CHECK FOR NULLABLE COLUMNS
*
         LA    R15,10              INTERNAL   LENGTH
         LH    R1,SQLLEN           GET LENGTH AGAIN (EXTERNAL)
         LA    R14,TIMES           LOAD TABLE ENTRY ADDRESS
         CH    R0,TIMES            IF THIS IS TIMESTAMP DATA,
         BRE   GETINTRN            JUST CHECK FOR NULLABLE COLUMNS
*
*        ERROR (UNRECOGNIZED DATA TYPE)
*
         SR    R1,R1               ZERO LENGTH
         LA    R14,UNDEFNED
         BRU   GETFIXED
                        SPACE 3
GETVAR   MVI   RFMTTYPE,X'04'      INDICATE ROW  HAS VARIABLE COLUMNS
         ST    R1,FFMTFLEN         COLUMN   LENGTH
         LA    R1,2(,R1)           ADD  TWO FOR  VARIABLE     LENGTH
         MVC   FFMTFTYP,4(R14)     MOVE    "EDITPROC" TYPE
         BRU   GETNULL
*
GETFIXED ST    R1,FFMTFLEN         COLUMN   LENGTH
         MVC   FFMTFTYP,4(R14)     MOVE    "EDITPROC" TYPE
         BRU   GETNULL
*
GETINTRN ST    R15,FFMTFLEN        COLUMN   LENGTH   (INTERNAL LENGTH)
         LR    R1,R15                                      *** DFK ***
         MVC   FFMTFTYP,4(R14)     MOVE    "EDITPROC" TYPE
*
GETNULL  MVI   FFMTNULL,X'04'      ASSUME   NOT   NULLABLE  (LEN=0)
         SR    R0,R0
         TM    SQLTYPE+1,X'01'     TEST THE NULL/INDICATOR BIT ???
         BRNO  GETSUBS             THERE IS NO INDICATOR, EXIT
         MVI   FFMTNULL,X'00'
         L     R0,FFMTFLEN         INCREMENT LENGTH
         A     R0,F1
         ST    R0,FFMTFLEN
         LA    R0,2                LOAD NULL INDICATOR LENGTH (SQL)
*
GETSUBS  LH    R14,2(,R14)         LOAD     PARSING SUBSCRIPT
*
         MVC   FFMTFNAM,SPACES     COLUMN   NAME
         LH    R15,SQLNAME
         CH    R15,H018
         BRNH  *+8
         LH    R15,H018
         LTR   R15,R15
         BRNP  GETEXIT
         BCTR  R15,0
         EX    R15,FNAMMVC
*
GETEXIT  BR    R9                  RETURN
*
FNAMMVC  MVC   FFMTFNAM(0),SQLNAME+2  * * * *  E X E C U T E D  * * * *
*
         DROP  R2
                        EJECT
**********************************************************************
* TRANSLATE ERROR CODE                                               *
**********************************************************************
XLATEERR ST    R0,PACKCODE         STORE THE REASON CODE
         CVD   R15,DBLWORK         STORE THE RETURN CODE (PACKED)
         UNPK  DBLWORK(4),DBLWORK+4(4)
         OI    DBLWORK+3,X'F0'     FORCE A DISPLAYABLE ZONE
         LTR   R15,R15             NEGATIVE RETURN CODE ???
         BRP   *+8                 NO  - BYPASS  MINUS SIGN
         MVI   DBLWORK+0,C'-'
         ST    R14,DBLWORK+8       SAVE ORIGINAL ERROR MESSAGE ADDRESS
*
         UNPK  REASCODE(9),PACKCODE(5)
         TR    REASCODE(8),XTAB
*
         LA    R1,XLATE            TRANSLATE   RETURN  CODE   FUNCTION
         LA    R2,SQLCA            SQL  COMMUNICATION  AREA
         STM   R1,R2,CALLP01
         LA    R0,DBLWORK+0
         ST    R0,CALLP03
         LA    R0,PACKCODE
         ST    R0,CALLP04
         OI    CALLP04,X'80'
*
         MVC   CAFFUNC,XLATE       INDICATE CURRENT CAF FUNCTION
*
         LA    R1,CALLPARM         CALL "DSNALI" - TRANSLATE "SQLCODE"
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
           L   R14,DBLWORK+8        LOAD ERROR MESSAGE ADDRESS
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
           L   R14,DBLWORK+8        LOAD ERROR MESSAGE ADDRESS
         endif
         BRU   RTNERROR            DISPLAY   ORIGINAL MESSAGE
*
         DROP  R4
         DROP  R5
         DROP  R6
         DROP  R7
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        D I S C O N N E C T   F R O M   D B 2                        *
*                                                                     *
*              - CLOSE                                                *
*                                                                     *
*                TERMINATE THE CURRENT THREAD USING THE               *
*                SPECIFIED TERMINATION OPTION.                        *
*                                                                     *
*              - DISCONNECT                                           *
*                                                                     *
*               TERMINATE THE CONNECTION TO DB2.                      *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
                        SPACE 3
**********************************************************************
*                                                                    *
*    CLOSE CURSOR                                                    *
*                                                                    *
**********************************************************************
*
         USING DDNWORK,R7
*
DB2CLOSE L     R6,WORKSQLW        LOAD SQL WORK AREA ADDRESS
         USING SQLDSECT,R6
*
         MVC   CAFFUNC,CLOSE       INDICATE CURRENT CAF FUNCTION
*
         L     R15,WORKINDX
         L     R9,DB2CLTBL(R15)
         BR    R9
*
DB2CLTBL DC    A(DB2CL00)         CLOSE DB2 CURSOR 00
         DC    A(DB2CL01)         CLOSE DB2 CURSOR 01
         DC    A(DB2CL02)         CLOSE DB2 CURSOR 02
         DC    A(DB2CL03)
         DC    A(DB2CL04)
         DC    A(DB2CL05)
         DC    A(DB2CL06)
         DC    A(DB2CL07)
         DC    A(DB2CL08)
         DC    A(DB2CL09)
         DC    A(DB2CL10)
         DC    A(DB2CL11)
         DC    A(DB2CL12)
         DC    A(DB2CL13)
         DC    A(DB2CL14)
         DC    A(DB2CL15)
         DC    A(DB2CL16)
         DC    A(DB2CL17)
         DC    A(DB2CL18)
         DC    A(DB2CL19)
         DC    A(DB2CL20)
         DC    A(DB2CL21)
         DC    A(DB2CL22)
         DC    A(DB2CL23)
         DC    A(DB2CL24)
         DC    A(DB2CL25)
         DC    A(DB2CL26)
         DC    A(DB2CL27)
         DC    A(DB2CL28)
         DC    A(DB2CL29)
         DC    A(DB2CL30)
         DC    A(DB2CL31)
         DC    A(DB2CL32)
         DC    A(DB2CL33)
         DC    A(DB2CL34)
         DC    A(DB2CL35)
         DC    A(DB2CL36)
         DC    A(DB2CL37)
         DC    A(DB2CL38)
         DC    A(DB2CL39)
         DC    A(DB2CL40)
         DC    A(DB2CL41)
         DC    A(DB2CL42)
         DC    A(DB2CL43)
         DC    A(DB2CL44)
         DC    A(DB2CL45)
         DC    A(DB2CL46)
         DC    A(DB2CL47)
         DC    A(DB2CL48)
         DC    A(DB2CL49)
         DC    A(DB2CL50)
         DC    A(DB2CL51)
         DC    A(DB2CL52)
         DC    A(DB2CL53)
         DC    A(DB2CL54)
         DC    A(DB2CL55)
         DC    A(DB2CL56)
         DC    A(DB2CL57)
         DC    A(DB2CL58)
         DC    A(DB2CL59)
         DC    A(DB2CL60)
         DC    A(DB2CL61)
         DC    A(DB2CL62)
         DC    A(DB2CL63)
         DC    A(DB2CL64)
         DC    A(DB2CL65)
         DC    A(DB2CL66)
         DC    A(DB2CL67)
         DC    A(DB2CL68)
         DC    A(DB2CL69)
         DC    A(DB2CL70)
         DC    A(DB2CL71)
         DC    A(DB2CL72)
         DC    A(DB2CL73)
         DC    A(DB2CL74)
         DC    A(DB2CL75)
         DC    A(DB2CL76)
         DC    A(DB2CL77)
         DC    A(DB2CL78)
         DC    A(DB2CL79)
         DC    A(DB2CL80)
         DC    A(DB2CL81)
         DC    A(DB2CL82)
         DC    A(DB2CL83)
         DC    A(DB2CL84)
         DC    A(DB2CL85)
         DC    A(DB2CL86)
         DC    A(DB2CL87)
         DC    A(DB2CL88)
         DC    A(DB2CL89)
         DC    A(DB2CL90)
         DC    A(DB2CL91)
         DC    A(DB2CL92)
         DC    A(DB2CL93)
         DC    A(DB2CL94)
         DC    A(DB2CL95)
         DC    A(DB2CL96)
         DC    A(DB2CL97)
         DC    A(DB2CL98)
         DC    A(DB2CL99)
*
DB2CLRC  EQU   *
*        SNAP  DCB=SNAPDCB,PDATA=(REGS),ID=221
*
         XC    WORKINDX,WORKINDX
*
         L     R0,WORKACTV         DECREMENT ACTIVE CURSOR COUNT
         BCTR  R0,0
         ST    R0,WORKACTV
*
         LTR   R0,R0
         BRP   DB2CLXIT
                        SPACE 3
**********************************************************************
* CLOSE DB2 THREAD                                                   *
**********************************************************************
*
         MVC   TERMOP,SYNC         SET   DB2 TERMINATION  OPTION
*
         LA    R1,CLOSE            BUILD PARAMETER  LIST
         LA    R2,TERMOP
         STM   R1,R2,CALLP01
         OI    CALLP02,X'80'
*
         LA    R1,CALLPARM         CALL "DSNALI" - CLOSE  THREAD
         L     R15,DSNALI
         BASR  R14,R15
*        SNAP  DCB=SNAPDCB,PDATA=(REGS),ID=222
                        SPACE 3
**********************************************************************
* DISCONNECT                                                         *
**********************************************************************
*
         LA    R1,DISC             BUILD PARAMETER  LIST
         ST    R1,CALLP01
         OI    CALLP01,X'80'
*
         LA    R1,CALLPARM         CALL "DSNALI" - CLOSE  THREAD
         L     R15,DSNALI
         BASR  R14,R15
*        SNAP  DCB=SNAPDCB,PDATA=(REGS),ID=223
                        SPACE 3
DB2CLXIT BR    R10                RETURN (SWITCH TO 31-BIT MODE)
*
         DROP  R6
         DROP  R7
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        "DUMMY" DSNHLI ENTRY POINT FOR "EXEC SQL" GENERATED CODE     *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         PUSH  using
         DROP  R12
         USING *,R15
DSNHLI   L     R15,DSNHLI2
         BR    R15
         DROP  R15
         POP   using
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* "ALLOCATE" - ALLOCATES MEMORY DYNAMICALLY FOR NEW DDNAME            *
*              WORK AREAS.                                            *
*                                                                     *
*  REGISTER USAGE:                                                    *
*                                                                     *
*        R10 - RETURN ADDRESS                                         *
*        R7  - CURRENT  "DDN" WORK AREA ADDRESS                       *
*        R2  - PREVIOUS "DDN" WORK AREA ADDRESS                       *
*        R1  - DDNAME    ADDRESS                                      *
*        R0  - AREA LENGTH                                            *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
ALLOCATE LA    R0,DDNWORKL         LOAD AREA LENGTH
         LR    R15,R0
         AY    R15,PARMMEMS      ADD TO TOTAL MEMORY USEAGE
         STY   R15,PARMMEMS      AND SAVE IT
         GETMAIN R,LV=(0),LOC=(ANY) GET  NEW AREA
*
         LR    R7,R1              INITIALIZE CURRENT DDN ADDRESS
         ST    R7,0(,R2)          ADD TO DDN CHAIN
         USING DDNWORK,R7
*
         LR    R0,R7              ZERO  WORK AREA
         LA    R1,DDNWORKL
         SR    R14,R14
         SR    R15,R15
         MVCL  R0,R14
*
         L     R15,WORKACTV       INCREMENT  ACTIVE CURSOR COUNT
         LA    R15,1(,R15)
         ST    R15,WORKACTV
*
         BCTR  R15,0              DECREMENT INDEX
         SLL   R15,2              MULTIPLY  BY  4
         ST    R15,WORKINDX       SAVE  BRANCH  TABLE  OFFSET
*
         MVC   WORKDDN,PARMDDN    SAVE  DDNAME
*
         ZAP   WORKCI#,P000       CONTROL INTERVAL  COUNT
         ZAP   WORKCNT,P000       EVENT   RECORD    COUNT
*
         COPY  GVBUR30P           DB2     PLAN   NAME
*
         MVC   SQLCAID,=CL8'SQLCA'
         LA    R0,SQLCALEN
         ST    R0,SQLCABC
                        SPACE 3
         L     R0,SQLDALEN        ALLOCATE  SQL  DESCRIPTOR    AREA
         LR    R15,R0
         AY    R15,PARMMEMS      ADD TO TOTAL MEMORY USEAGE
         STY   R15,PARMMEMS      AND SAVE IT
         GETMAIN RU,LV=(0),LOC=(ANY)
         ST    R1,WORKSQLD        SAVE SQL  DESCRIPTOR AREA ADDRESS
         LR    R5,R1
         USING SQLDA,R5
*
         LR    R0,R5              INITIALIZE  "SQLDA" (LOW  VALUES)
         L     R1,SQLDALEN
         SR    R14,R14
         SR    R15,R15
         MVCL  R0,R14
*
         MVC   SQLDAID,=CL8'SQLDA'
         MVC   SQLDABC,SQLDALEN
         MVC   SQLN,H750
                        SPACE 3
         L     R0,SQLDSIZE        ALLOCATE  SQL  WORK  AREA
         LR    R15,R0
         AY    R15,PARMMEMS      ADD TO TOTAL MEMORY USEAGE
         STY   R15,PARMMEMS      AND SAVE IT
         GETMAIN R,LV=(0),LOC=(ANY)
         ST    R1,WORKSQLW        SAVE WORK AREA ADDRESS
         LR    R6,R1
         USING SQLDSECT,R6
                        EJECT
**********************************************************************
* GET SQL TEXT FROM PARAMETER                                        *
**********************************************************************
         MVC   ACCMODE,PARMOPT1   ACCESS     MODE    = DB2  SQL
         MVC   SQLFORM,PARMOPT2   FORMATTING OPTION  = ANSI SQL
         MVC   DBEDITPR,SPACES    EDIT  PROC NAME
*
         L     R1,SAVEPARM        LOAD PARAMETER  LIST ADDR
*
         L     R14,PARMSUBS-PARMLIST(,R1)    DB2  SUBSYSTEM NAME
         MVC   DBSUBSYS,0(R14)
*
         L     R9,PARMTXTL-PARMLIST(,R1)     SQL  TEXT      LENGTH
         L     R9,0(,R9)
*
         L     R14,PARMTXTA-PARMLIST(,R1)    SQL  TEXT      ADDRESS
*
         LHI   R1,L'SQLTEXT       TEXT TOO LONG ???
         CR    R9,R1
         BRNH  *+6
         LR    R9,R1              YES - TRUNCATE TO  FIT
*
         LR    R15,R9             COPY  TEXT TO  DDN WORK  AREA
         LR    R1,R9
         LA    R0,SQLTEXT
         MVCL  R0,R14
*
         LR    R0,R9              INITIALIZE LOOP COUNTER
         LA    R1,SQLTEXT(R9)     INITIALIZE END-OF-STRING ADDR (+1)
*
TEXTLOOP BCTR  R1,0               BACKUP  TO PRECEEDING BYTE
         CLI   0(R1),C' '         TRAILING   BLANK  ???
         BRNE  TEXTLEN            NO  - LAST BYTE FOUND (SAVE LENGTH)
         BRCT  R0,TEXTLOOP        CONTINUE SEARCHING FOR LAST NON-BLANK
*
         BRU   INITMSG4           ERROR -  NO   TEXT PRESENT
*
TEXTLEN  STH   R0,SQLBUFFR        SAVE ACTUAL   TEXT LENGTH
*
         LR    R1,R0              COPY SQL TEXT
         LA    R0,SQLTEXT
         LA    R14,TEXTOFF(,R4)
         LR    R15,R1
         MVCL  R0,R14
*
         LA    R0,EXPLWA          INITIALIZE EDIT PROC PARAMETER LIST
         ST    R0,EDITPRM1
         LA    R0,EDITCODE
         ST    R0,EDITPRM2
         LA    R0,PROCWA
         ST    R0,EXPLWA
         LA    R0,L'PROCWA
         ST    R0,EXPLWL
         LA    R0,4
         ST    R0,EDITCODE
         LA    R0,RFMTNFLD
         ST    R0,EDITROW
                        SPACE 3
         CLC   DBEDITPR,SPACES    "EDITPROC"  DEFINED FOR TABLE ???
         BRE   INITEXIT           NO  - EXIT
*
         LA    R0,DBEDITPR        POINT TO SUBROUTINE NAME
         LOAD  EPLOC=(0),ERRET=PROCERR
         O     R0,MODE31
         ST    R0,EDITPROC
*
INITEXIT BR    R10                RETURN  TO CALLER
                        SPACE 3
PROCERR  LA    R14,EDITPROC_LOAD_FAIL LOAD ERROR MESSAGE NUMBER
         MVC   REASCODE(8),DBEDITPR  INDICATE PROGRAM NAME
         BRU   RTNERROR2          PRINT ERROR MESSAGE - STOP
                        SPACE 3
*NITERR  LA    R1,ERR#03          ASSUME I/O ERROR
*        MVC   REASCODE(8),WORKDDN
*        C     R15,F8             RECORD NOT FOUND ???
*        BRNE  RTNERROR           NO  -  INDICATE  I/O ERROR
*
*        USING IFGRPL,R3
*        CLI   RPLFDB3,X'10'
*        BRNE  RTNERROR
*
INITMSG4 LA    R14,SQL_TEXT_NF     INDICATE RECORD NOT FOUND
         MVC   REASCODE(8),WORKDDN  DDNAME
         BRU   RTNERROR2          INDICATE ERROR
*
         DROP  R5
         DROP  R6
         DROP  R7
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* "LOCDDN" - SEARCHES THE "DDN" CHAIN FOR AN ENTRY WHICH MATCHES      *
*            THE "DDNAME" IN THE PARAMETER AREA.                      *
*                                                                     *
* REGISTER USAGE:                                                     *
*                                                                     *
*        R10 - RETURN ADDRESS                                         *
*        R7  - CURRENT  "DDN"  WORK AREA ADDRESS                      *
*        R2  - PREVIOUS "DDN"  WORK AREA ADDRESS                      *
*        R1  - DDNAME          ADDRESS                                *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
LOCDDN   LA    R2,WORKDPTR          INITIALIZE PREVIOUS WORK AREA ADDR
         L     R7,WORKDPTR          INITIALIZE CURRENT  WORK AREA ADDR
*
         BRU   LOCEND               CHECK  FOR END-OF-CHAIN
                        SPACE 3
         USING DDNWORK,R7
*
LOCLOOP  LR    R2,R7
         L     R7,WORKNEXT
LOCEND   LTR   R7,R7              END-OF-CHAIN ???
         BZR   R10                YES - EXIT SUBROUTINE
*
         CLC   WORKDDN,0(R1)      MATCHING ENTRY ???
         BRNE  LOCLOOP            NO  - ADVANCE TO NEXT ENTRY ON CHAIN
*
         BR    R10                YES - EXIT SUBROUTINE
*
         DROP  R7
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        C O N S T A N T S                                            *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
static   loctr
         DS   0D
F1       DC    F'001'
F2       DC    F'002'
F3       DC    F'003'
F4       DC    F'004'
F5       DC    F'005'
F6       DC    F'006'
F8       DC    F'008'
F64K     DC    F'65536'
*
H000     DC    H'000'
H001     DC    H'001'
H002     DC    H'002'
H003     DC    H'003'
H004     DC    H'004'
H005     DC    H'005'
H006     DC    H'006'
H007     DC    H'007'
H008     DC    H'008'
H009     DC    H'009'
H010     DC    H'010'
H012     DC    H'012'
H016     DC    H'016'
H018     DC    H'018'
H020     DC    H'020'
H024     DC    H'024'
H040     DC    H'040'
*
H750     DC    H'750'
*
NULLIND  DC    H'-1'
FFFF     DC    XL4'FFFFFFFF'
*
SQLDALEN DC    F'33016'           16 + (750 COLUMNS * 44)
SQLDSIZE DC    A(SQLDLEN)         16 + (750 COLUMNS * 44)
MODE31   DS   0XL4
OPENPARM DC    XL8'8000000000000000'
*
P000     DC    PL4'0'
P001     DC    PL4'1'
*
*        DATA TYPES FOUND IN SQLTYPE, AFTER REMOVING THE NULL BIT
*
VARCTYPE DC    H'448',H'04',X'14'  NOTNULL VARCHAR TYPE
CHARTYPE DC    H'452',H'08',X'10'  NOTNULL FIXED CHAR TYPE
VARLTYPE DC    H'456',H'12',X'14'  NOTNULL LONG VARCHAR TYPE
VARGTYPE DC    H'464',H'16',X'14'  NOTNULL VARGRAPHIC TYPE
GTYPE    DC    H'468',H'20',X'10'  NOTNULL GRAPHIC TYPE
LVARGTYP DC    H'472',H'24',X'14'  NOTNULL LONG VARGRAPHIC TYPE
FLOATYPE DC    H'480',H'28',X'08'  NOTNULL FLOAT TYPE
DECTYPE  DC    H'484',H'32',X'0C'  NOTNULL DECIMAL TYPE
INTTYPE  DC    H'496',H'36',X'00'  NOTNULL INTEGER TYPE
HWTYPE   DC    H'500',H'40',X'04'  NOTNULL SMALLINT TYPE
DATETYP  DC    H'384',H'44',X'20'  NOTNULL DATE TYPE
TIMETYP  DC    H'388',H'48',X'24'  NOTNULL TIME TYPE
TIMES    DC    H'392',H'52',X'28'  NOTNULL TIMESTAMP TYPE
UNDEFNED DC    H'000',H'56',X'FF'  UNDEFINED
                        SPACE 3
SYNC     DC    CL4'SYNC'          DB2 THREAD TERMINATION OPTION
SPACES   DC    CL132' '
HEXFF    DC    256X'FF'
ZEROES   DC   8CL01'0'
REFTBLID DC    CL03'#DD'
                        SPACE 3
         ORG   *-240              TRANSLATE TABLE FOR BINARY TO HEX
XTAB     EQU   *                       (VIRTUAL ORIGIN)
         ORG   *+240
         DC    C'0123456789ABCDEF'
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        CALL  ATTACH  FUNCTION  REQUEST  NAMES                       *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
CONNECT  DC    CL12'CONNECT      ' CONNECT      TO   DB2
DISC     DC    CL12'DISCONNECT   ' DISCONNECT   FROM DB2
OPEN     DC    CL12'OPEN         ' OPEN   (CREATE    THREAD)
CLOSE    DC    CL12'CLOSE        ' CLOSE  (TERMINATE THREAD)
XLATE    DC    CL12'TRANSLATE    ' TRANSLATE    OPEN ERRORS
PREPARE  DC    CL12'PREPARE      ' PREPARE  SQL STATEMENT
FETCH    DC    CL12'FETCH        ' FETCH    DB2 ROW
SETDEG   DC    CL12'SETDEGREE    ' SET PARALLEL PROCESSING DEGREE
*
         LTORG
                        SPACE 3
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        V A R I A B L E S                                            *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        D A T A   C O N T R O L   B L O C K S                        *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         PRINT NOGEN
*
*DLWTO   WTO   TEXT=(R2),MF=L     MODEL WTO TO DISPLAY CONSOLE MESSAGES
                        SPACE 3
SNAPDCB  DCB   DSORG=PS,DDNAME=SNAPEXIT,MACRF=(W),                     X
               RECFM=VBA,LRECL=125,BLKSIZE=1632
                        SPACE 3
*
code     loctr
         USING SQLDA,R5
         USING SQLDSECT,R6
         USING DDNWORK,R7
*
FETCH00  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR00 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
FETCH01  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR01 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
FETCH02  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR02 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
FETCH03  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR03 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
FETCH04  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR04 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
FETCH05  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR05 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
FETCH06  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR06 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
FETCH07  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR07 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
FETCH08  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR08 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
FETCH09  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR09 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
FETCH10  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR10 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
FETCH11  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR11 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
FETCH12  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR12 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
FETCH13  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR13 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
FETCH14  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR14 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
FETCH15  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR15 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
FETCH16  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR16 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
FETCH17  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR17 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
FETCH18  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR18 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
FETCH19  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR19 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
FETCH20  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR20 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
FETCH21  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR21 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
FETCH22  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR22 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
FETCH23  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR23 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
FETCH24  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR24 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
static   loctr
         LTORG
code     loctr
*
         USING *,R9
FETCH25  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR25 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH26  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR26 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH27  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR27 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH28  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR28 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH29  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR29 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH30  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR30 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH31  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR31 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH32  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR32 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH33  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR33 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH34  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR34 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH35  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR35 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH36  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR36 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH37  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR37 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH38  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR38 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH39  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR39 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH40  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR40 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH41  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR41 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH42  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR42 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH43  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR43 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH44  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR44 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH45  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR45 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH46  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR46 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH47  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR47 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH48  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR48 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH49  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR49 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
static   loctr
         LTORG
code     loctr
*
         USING *,R9
FETCH50  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR50 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH51  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR51 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH52  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR52 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH53  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR53 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH54  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR54 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH55  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR55 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH56  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR56 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH57  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR57 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH58  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR58 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH59  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR59 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH60  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR60 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH61  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR61 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH62  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR62 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH63  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR63 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH64  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR64 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH65  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR65 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH66  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR66 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH67  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR67 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH68  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR68 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH69  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR69 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH70  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR70 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH71  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR71 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH72  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR72 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH73  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR73 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH74  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR74 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
static   loctr
         LTORG
code     loctr
*
         USING *,R9
FETCH75  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR75 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH76  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR76 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH77  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR77 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH78  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR78 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH79  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR79 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH80  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR80 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH81  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR81 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH82  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR82 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH83  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR83 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH84  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR84 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH85  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR85 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH86  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR86 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH87  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR87 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH88  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR88 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH89  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR89 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH90  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR90 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH91  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR91 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH92  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR92 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH93  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR93 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH94  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR94 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH95  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR95 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH96  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR96 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH97  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR97 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH98  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR98 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
         USING *,R9
FETCH99  EXEC  SQL WHENEVER NOT FOUND  GO TO NOTFOUND
         EXEC  SQL FETCH DB2CSR99 USING DESCRIPTOR :SQLDA
         BRU   FETCHRC
*
static   loctr
         LTORG
code     loctr
         DROP  R9
                        EJECT
*
         USING *,R9
CONNTX00 EXEC  SQL PREPARE SQLTXT00 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX01 EXEC  SQL PREPARE SQLTXT01 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX02 EXEC  SQL PREPARE SQLTXT02 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX03 EXEC  SQL PREPARE SQLTXT03 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX04 EXEC  SQL PREPARE SQLTXT04 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX05 EXEC  SQL PREPARE SQLTXT05 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX06 EXEC  SQL PREPARE SQLTXT06 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX07 EXEC  SQL PREPARE SQLTXT07 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX08 EXEC  SQL PREPARE SQLTXT08 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX09 EXEC  SQL PREPARE SQLTXT09 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX10 EXEC  SQL PREPARE SQLTXT10 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX11 EXEC  SQL PREPARE SQLTXT11 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX12 EXEC  SQL PREPARE SQLTXT12 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX13 EXEC  SQL PREPARE SQLTXT13 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX14 EXEC  SQL PREPARE SQLTXT14 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
static   loctr
         LTORG
code     loctr
*
         USING *,R9
CONNTX15 EXEC  SQL PREPARE SQLTXT15 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX16 EXEC  SQL PREPARE SQLTXT16 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX17 EXEC  SQL PREPARE SQLTXT17 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX18 EXEC  SQL PREPARE SQLTXT18 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX19 EXEC  SQL PREPARE SQLTXT19 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX20 EXEC  SQL PREPARE SQLTXT20 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX21 EXEC  SQL PREPARE SQLTXT21 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX22 EXEC  SQL PREPARE SQLTXT22 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX23 EXEC  SQL PREPARE SQLTXT23 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX24 EXEC  SQL PREPARE SQLTXT24 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX25 EXEC  SQL PREPARE SQLTXT25 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX26 EXEC  SQL PREPARE SQLTXT26 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX27 EXEC  SQL PREPARE SQLTXT27 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX28 EXEC  SQL PREPARE SQLTXT28 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX29 EXEC  SQL PREPARE SQLTXT29 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
static   loctr
         LTORG
code     loctr
*
         USING *,R9
CONNTX30 EXEC  SQL PREPARE SQLTXT30 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX31 EXEC  SQL PREPARE SQLTXT31 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX32 EXEC  SQL PREPARE SQLTXT32 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX33 EXEC  SQL PREPARE SQLTXT33 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX34 EXEC  SQL PREPARE SQLTXT34 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX35 EXEC  SQL PREPARE SQLTXT35 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX36 EXEC  SQL PREPARE SQLTXT36 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX37 EXEC  SQL PREPARE SQLTXT37 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX38 EXEC  SQL PREPARE SQLTXT38 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX39 EXEC  SQL PREPARE SQLTXT39 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX40 EXEC  SQL PREPARE SQLTXT40 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX41 EXEC  SQL PREPARE SQLTXT41 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX42 EXEC  SQL PREPARE SQLTXT42 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX43 EXEC  SQL PREPARE SQLTXT43 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX44 EXEC  SQL PREPARE SQLTXT44 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
static   loctr
         LTORG
code     loctr
*
         USING *,R9
CONNTX45 EXEC  SQL PREPARE SQLTXT45 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX46 EXEC  SQL PREPARE SQLTXT46 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX47 EXEC  SQL PREPARE SQLTXT47 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX48 EXEC  SQL PREPARE SQLTXT48 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX49 EXEC  SQL PREPARE SQLTXT49 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX50 EXEC  SQL PREPARE SQLTXT50 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX51 EXEC  SQL PREPARE SQLTXT51 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX52 EXEC  SQL PREPARE SQLTXT52 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX53 EXEC  SQL PREPARE SQLTXT53 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX54 EXEC  SQL PREPARE SQLTXT54 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX55 EXEC  SQL PREPARE SQLTXT55 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX56 EXEC  SQL PREPARE SQLTXT56 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX57 EXEC  SQL PREPARE SQLTXT57 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX58 EXEC  SQL PREPARE SQLTXT58 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX59 EXEC  SQL PREPARE SQLTXT59 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
static   loctr
         LTORG
code     loctr
*
         USING *,R9
CONNTX60 EXEC  SQL PREPARE SQLTXT60 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX61 EXEC  SQL PREPARE SQLTXT61 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX62 EXEC  SQL PREPARE SQLTXT62 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX63 EXEC  SQL PREPARE SQLTXT63 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX64 EXEC  SQL PREPARE SQLTXT64 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX65 EXEC  SQL PREPARE SQLTXT65 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX66 EXEC  SQL PREPARE SQLTXT66 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX67 EXEC  SQL PREPARE SQLTXT67 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX68 EXEC  SQL PREPARE SQLTXT68 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX69 EXEC  SQL PREPARE SQLTXT69 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX70 EXEC  SQL PREPARE SQLTXT70 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX71 EXEC  SQL PREPARE SQLTXT71 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX72 EXEC  SQL PREPARE SQLTXT72 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX73 EXEC  SQL PREPARE SQLTXT73 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX74 EXEC  SQL PREPARE SQLTXT74 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
static   loctr
         LTORG
code     loctr
*
         USING *,R9
CONNTX75 EXEC  SQL PREPARE SQLTXT75 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX76 EXEC  SQL PREPARE SQLTXT76 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX77 EXEC  SQL PREPARE SQLTXT77 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX78 EXEC  SQL PREPARE SQLTXT78 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX79 EXEC  SQL PREPARE SQLTXT79 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX80 EXEC  SQL PREPARE SQLTXT80 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX81 EXEC  SQL PREPARE SQLTXT81 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX82 EXEC  SQL PREPARE SQLTXT82 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX83 EXEC  SQL PREPARE SQLTXT83 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX84 EXEC  SQL PREPARE SQLTXT84 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX85 EXEC  SQL PREPARE SQLTXT85 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX86 EXEC  SQL PREPARE SQLTXT86 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX87 EXEC  SQL PREPARE SQLTXT87 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX88 EXEC  SQL PREPARE SQLTXT88 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX89 EXEC  SQL PREPARE SQLTXT89 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
static   loctr
         LTORG
code     loctr
*
         USING *,R9
CONNTX90 EXEC  SQL PREPARE SQLTXT90 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX91 EXEC  SQL PREPARE SQLTXT91 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX92 EXEC  SQL PREPARE SQLTXT92 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX93 EXEC  SQL PREPARE SQLTXT93 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX94 EXEC  SQL PREPARE SQLTXT94 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX95 EXEC  SQL PREPARE SQLTXT95 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX96 EXEC  SQL PREPARE SQLTXT96 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX97 EXEC  SQL PREPARE SQLTXT97 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX98 EXEC  SQL PREPARE SQLTXT98 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
         USING *,R9
CONNTX99 EXEC  SQL PREPARE SQLTXT99 INTO :SQLDA FROM :SQLBUFFR
         BRU   CONNSTRC
*
static   loctr
         LTORG
code     loctr
*
         DROP  R9
                        EJECT
*
         USING *,R9
CONNCS00 EXEC  SQL OPEN DB2CSR00
         BRU   CONNCSRC
*
         USING *,R9
CONNCS01 EXEC  SQL OPEN DB2CSR01
         BRU   CONNCSRC
*
         USING *,R9
CONNCS02 EXEC  SQL OPEN DB2CSR02
         BRU   CONNCSRC
*
         USING *,R9
CONNCS03 EXEC  SQL OPEN DB2CSR03
         BRU   CONNCSRC
*
         USING *,R9
CONNCS04 EXEC  SQL OPEN DB2CSR04
         BRU   CONNCSRC
*
         USING *,R9
CONNCS05 EXEC  SQL OPEN DB2CSR05
         BRU   CONNCSRC
*
         USING *,R9
CONNCS06 EXEC  SQL OPEN DB2CSR06
         BRU   CONNCSRC
*
         USING *,R9
CONNCS07 EXEC  SQL OPEN DB2CSR07
         BRU   CONNCSRC
*
         USING *,R9
CONNCS08 EXEC  SQL OPEN DB2CSR08
         BRU   CONNCSRC
*
         USING *,R9
CONNCS09 EXEC  SQL OPEN DB2CSR09
         BRU   CONNCSRC
*
         USING *,R9
CONNCS10 EXEC  SQL OPEN DB2CSR10
         BRU   CONNCSRC
*
         USING *,R9
CONNCS11 EXEC  SQL OPEN DB2CSR11
         BRU   CONNCSRC
*
         USING *,R9
CONNCS12 EXEC  SQL OPEN DB2CSR12
         BRU   CONNCSRC
*
         USING *,R9
CONNCS13 EXEC  SQL OPEN DB2CSR13
         BRU   CONNCSRC
*
         USING *,R9
CONNCS14 EXEC  SQL OPEN DB2CSR14
         BRU   CONNCSRC
*
         USING *,R9
CONNCS15 EXEC  SQL OPEN DB2CSR15
         BRU   CONNCSRC
*
         USING *,R9
CONNCS16 EXEC  SQL OPEN DB2CSR16
         BRU   CONNCSRC
*
         USING *,R9
CONNCS17 EXEC  SQL OPEN DB2CSR17
         BRU   CONNCSRC
*
         USING *,R9
CONNCS18 EXEC  SQL OPEN DB2CSR18
         BRU   CONNCSRC
*
         USING *,R9
CONNCS19 EXEC  SQL OPEN DB2CSR19
         BRU   CONNCSRC
*
static   loctr
         LTORG
code     loctr
*
         USING *,R9
CONNCS20 EXEC  SQL OPEN DB2CSR20
         BRU   CONNCSRC
*
         USING *,R9
CONNCS21 EXEC  SQL OPEN DB2CSR21
         BRU   CONNCSRC
*
         USING *,R9
CONNCS22 EXEC  SQL OPEN DB2CSR22
         BRU   CONNCSRC
*
         USING *,R9
CONNCS23 EXEC  SQL OPEN DB2CSR23
         BRU   CONNCSRC
*
         USING *,R9
CONNCS24 EXEC  SQL OPEN DB2CSR24
         BRU   CONNCSRC
*
         USING *,R9
CONNCS25 EXEC  SQL OPEN DB2CSR25
         BRU   CONNCSRC
*
         USING *,R9
CONNCS26 EXEC  SQL OPEN DB2CSR26
         BRU   CONNCSRC
*
         USING *,R9
CONNCS27 EXEC  SQL OPEN DB2CSR27
         BRU   CONNCSRC
*
         USING *,R9
CONNCS28 EXEC  SQL OPEN DB2CSR28
         BRU   CONNCSRC
*
         USING *,R9
CONNCS29 EXEC  SQL OPEN DB2CSR29
         BRU   CONNCSRC
*
         USING *,R9
CONNCS30 EXEC  SQL OPEN DB2CSR30
         BRU   CONNCSRC
*
         USING *,R9
CONNCS31 EXEC  SQL OPEN DB2CSR31
         BRU   CONNCSRC
*
         USING *,R9
CONNCS32 EXEC  SQL OPEN DB2CSR32
         BRU   CONNCSRC
*
         USING *,R9
CONNCS33 EXEC  SQL OPEN DB2CSR33
         BRU   CONNCSRC
*
         USING *,R9
CONNCS34 EXEC  SQL OPEN DB2CSR34
         BRU   CONNCSRC
*
         USING *,R9
CONNCS35 EXEC  SQL OPEN DB2CSR35
         BRU   CONNCSRC
*
         USING *,R9
CONNCS36 EXEC  SQL OPEN DB2CSR36
         BRU   CONNCSRC
*
         USING *,R9
CONNCS37 EXEC  SQL OPEN DB2CSR37
         BRU   CONNCSRC
*
         USING *,R9
CONNCS38 EXEC  SQL OPEN DB2CSR38
         BRU   CONNCSRC
*
         USING *,R9
CONNCS39 EXEC  SQL OPEN DB2CSR39
         BRU   CONNCSRC
*
static   loctr
         LTORG
code     loctr
*
         USING *,R9
CONNCS40 EXEC  SQL OPEN DB2CSR40
         BRU   CONNCSRC
*
         USING *,R9
CONNCS41 EXEC  SQL OPEN DB2CSR41
         BRU   CONNCSRC
*
         USING *,R9
CONNCS42 EXEC  SQL OPEN DB2CSR42
         BRU   CONNCSRC
*
         USING *,R9
CONNCS43 EXEC  SQL OPEN DB2CSR43
         BRU   CONNCSRC
*
         USING *,R9
CONNCS44 EXEC  SQL OPEN DB2CSR44
         BRU   CONNCSRC
*
         USING *,R9
CONNCS45 EXEC  SQL OPEN DB2CSR45
         BRU   CONNCSRC
*
         USING *,R9
CONNCS46 EXEC  SQL OPEN DB2CSR46
         BRU   CONNCSRC
*
         USING *,R9
CONNCS47 EXEC  SQL OPEN DB2CSR47
         BRU   CONNCSRC
*
         USING *,R9
CONNCS48 EXEC  SQL OPEN DB2CSR48
         BRU   CONNCSRC
*
         USING *,R9
CONNCS49 EXEC  SQL OPEN DB2CSR49
         BRU   CONNCSRC
*
         USING *,R9
CONNCS50 EXEC  SQL OPEN DB2CSR50
         BRU   CONNCSRC
*
         USING *,R9
CONNCS51 EXEC  SQL OPEN DB2CSR51
         BRU   CONNCSRC
*
         USING *,R9
CONNCS52 EXEC  SQL OPEN DB2CSR52
         BRU   CONNCSRC
*
         USING *,R9
CONNCS53 EXEC  SQL OPEN DB2CSR53
         BRU   CONNCSRC
*
         USING *,R9
CONNCS54 EXEC  SQL OPEN DB2CSR54
         BRU   CONNCSRC
*
         USING *,R9
CONNCS55 EXEC  SQL OPEN DB2CSR55
         BRU   CONNCSRC
*
         USING *,R9
CONNCS56 EXEC  SQL OPEN DB2CSR56
         BRU   CONNCSRC
*
         USING *,R9
CONNCS57 EXEC  SQL OPEN DB2CSR57
         BRU   CONNCSRC
*
         USING *,R9
CONNCS58 EXEC  SQL OPEN DB2CSR58
         BRU   CONNCSRC
*
         USING *,R9
CONNCS59 EXEC  SQL OPEN DB2CSR59
         BRU   CONNCSRC
*
static   loctr
         LTORG
code     loctr
*
         USING *,R9
CONNCS60 EXEC  SQL OPEN DB2CSR60
         BRU   CONNCSRC
*
         USING *,R9
CONNCS61 EXEC  SQL OPEN DB2CSR61
         BRU   CONNCSRC
*
         USING *,R9
CONNCS62 EXEC  SQL OPEN DB2CSR62
         BRU   CONNCSRC
*
         USING *,R9
CONNCS63 EXEC  SQL OPEN DB2CSR63
         BRU   CONNCSRC
*
         USING *,R9
CONNCS64 EXEC  SQL OPEN DB2CSR64
         BRU   CONNCSRC
*
         USING *,R9
CONNCS65 EXEC  SQL OPEN DB2CSR65
         BRU   CONNCSRC
*
         USING *,R9
CONNCS66 EXEC  SQL OPEN DB2CSR66
         BRU   CONNCSRC
*
         USING *,R9
CONNCS67 EXEC  SQL OPEN DB2CSR67
         BRU   CONNCSRC
*
         USING *,R9
CONNCS68 EXEC  SQL OPEN DB2CSR68
         BRU   CONNCSRC
*
         USING *,R9
CONNCS69 EXEC  SQL OPEN DB2CSR69
         BRU   CONNCSRC
*
         USING *,R9
CONNCS70 EXEC  SQL OPEN DB2CSR70
         BRU   CONNCSRC
*
         USING *,R9
CONNCS71 EXEC  SQL OPEN DB2CSR71
         BRU   CONNCSRC
*
         USING *,R9
CONNCS72 EXEC  SQL OPEN DB2CSR72
         BRU   CONNCSRC
*
         USING *,R9
CONNCS73 EXEC  SQL OPEN DB2CSR73
         BRU   CONNCSRC
*
         USING *,R9
CONNCS74 EXEC  SQL OPEN DB2CSR74
         BRU   CONNCSRC
*
         USING *,R9
CONNCS75 EXEC  SQL OPEN DB2CSR75
         BRU   CONNCSRC
*
         USING *,R9
CONNCS76 EXEC  SQL OPEN DB2CSR76
         BRU   CONNCSRC
*
         USING *,R9
CONNCS77 EXEC  SQL OPEN DB2CSR77
         BRU   CONNCSRC
*
         USING *,R9
CONNCS78 EXEC  SQL OPEN DB2CSR78
         BRU   CONNCSRC
*
         USING *,R9
CONNCS79 EXEC  SQL OPEN DB2CSR79
         BRU   CONNCSRC
*
static   loctr
         LTORG
code     loctr
*
         USING *,R9
CONNCS80 EXEC  SQL OPEN DB2CSR80
         BRU   CONNCSRC
*
         USING *,R9
CONNCS81 EXEC  SQL OPEN DB2CSR81
         BRU   CONNCSRC
*
         USING *,R9
CONNCS82 EXEC  SQL OPEN DB2CSR82
         BRU   CONNCSRC
*
         USING *,R9
CONNCS83 EXEC  SQL OPEN DB2CSR83
         BRU   CONNCSRC
*
         USING *,R9
CONNCS84 EXEC  SQL OPEN DB2CSR84
         BRU   CONNCSRC
*
         USING *,R9
CONNCS85 EXEC  SQL OPEN DB2CSR85
         BRU   CONNCSRC
*
         USING *,R9
CONNCS86 EXEC  SQL OPEN DB2CSR86
         BRU   CONNCSRC
*
         USING *,R9
CONNCS87 EXEC  SQL OPEN DB2CSR87
         BRU   CONNCSRC
*
         USING *,R9
CONNCS88 EXEC  SQL OPEN DB2CSR88
         BRU   CONNCSRC
*
         USING *,R9
CONNCS89 EXEC  SQL OPEN DB2CSR89
         BRU   CONNCSRC
*
         USING *,R9
CONNCS90 EXEC  SQL OPEN DB2CSR90
         BRU   CONNCSRC
*
         USING *,R9
CONNCS91 EXEC  SQL OPEN DB2CSR91
         BRU   CONNCSRC
*
         USING *,R9
CONNCS92 EXEC  SQL OPEN DB2CSR92
         BRU   CONNCSRC
*
         USING *,R9
CONNCS93 EXEC  SQL OPEN DB2CSR93
         BRU   CONNCSRC
*
         USING *,R9
CONNCS94 EXEC  SQL OPEN DB2CSR94
         BRU   CONNCSRC
*
         USING *,R9
CONNCS95 EXEC  SQL OPEN DB2CSR95
         BRU   CONNCSRC
*
         USING *,R9
CONNCS96 EXEC  SQL OPEN DB2CSR96
         BRU   CONNCSRC
*
         USING *,R9
CONNCS97 EXEC  SQL OPEN DB2CSR97
         BRU   CONNCSRC
*
         USING *,R9
CONNCS98 EXEC  SQL OPEN DB2CSR98
         BRU   CONNCSRC
*
         USING *,R9
CONNCS99 EXEC  SQL OPEN DB2CSR99
         BRU   CONNCSRC
*
static   loctr
         LTORG
code     loctr
*
         DROP  R9
                        EJECT
*
         USING *,R9
DB2CL00  EXEC  SQL CLOSE DB2CSR00
         BRU   DB2CLRC
*
         USING *,R9
DB2CL01  EXEC  SQL CLOSE DB2CSR01
         BRU   DB2CLRC
*
         USING *,R9
DB2CL02  EXEC  SQL CLOSE DB2CSR02
         BRU   DB2CLRC
*
         USING *,R9
DB2CL03  EXEC  SQL CLOSE DB2CSR03
         BRU   DB2CLRC
*
         USING *,R9
DB2CL04  EXEC  SQL CLOSE DB2CSR04
         BRU   DB2CLRC
*
         USING *,R9
DB2CL05  EXEC  SQL CLOSE DB2CSR05
         BRU   DB2CLRC
*
         USING *,R9
DB2CL06  EXEC  SQL CLOSE DB2CSR06
         BRU   DB2CLRC
*
         USING *,R9
DB2CL07  EXEC  SQL CLOSE DB2CSR07
         BRU   DB2CLRC
*
         USING *,R9
DB2CL08  EXEC  SQL CLOSE DB2CSR08
         BRU   DB2CLRC
*
         USING *,R9
DB2CL09  EXEC  SQL CLOSE DB2CSR09
         BRU   DB2CLRC
*
         USING *,R9
DB2CL10  EXEC  SQL CLOSE DB2CSR10
         BRU   DB2CLRC
*
         USING *,R9
DB2CL11  EXEC  SQL CLOSE DB2CSR11
         BRU   DB2CLRC
*
         USING *,R9
DB2CL12  EXEC  SQL CLOSE DB2CSR12
         BRU   DB2CLRC
*
         USING *,R9
DB2CL13  EXEC  SQL CLOSE DB2CSR13
         BRU   DB2CLRC
*
         USING *,R9
DB2CL14  EXEC  SQL CLOSE DB2CSR14
         BRU   DB2CLRC
*
         USING *,R9
DB2CL15  EXEC  SQL CLOSE DB2CSR15
         BRU   DB2CLRC
*
         USING *,R9
DB2CL16  EXEC  SQL CLOSE DB2CSR16
         BRU   DB2CLRC
*
         USING *,R9
DB2CL17  EXEC  SQL CLOSE DB2CSR17
         BRU   DB2CLRC
*
         USING *,R9
DB2CL18  EXEC  SQL CLOSE DB2CSR18
         BRU   DB2CLRC
*
         USING *,R9
DB2CL19  EXEC  SQL CLOSE DB2CSR19
         BRU   DB2CLRC
*
static   loctr
         LTORG
code     loctr
*
         USING *,R9
DB2CL20  EXEC  SQL CLOSE DB2CSR20
         BRU   DB2CLRC
*
         USING *,R9
DB2CL21  EXEC  SQL CLOSE DB2CSR21
         BRU   DB2CLRC
*
         USING *,R9
DB2CL22  EXEC  SQL CLOSE DB2CSR22
         BRU   DB2CLRC
*
         USING *,R9
DB2CL23  EXEC  SQL CLOSE DB2CSR23
         BRU   DB2CLRC
*
         USING *,R9
DB2CL24  EXEC  SQL CLOSE DB2CSR24
         BRU   DB2CLRC
*
         USING *,R9
DB2CL25  EXEC  SQL CLOSE DB2CSR25
         BRU   DB2CLRC
*
         USING *,R9
DB2CL26  EXEC  SQL CLOSE DB2CSR26
         BRU   DB2CLRC
*
         USING *,R9
DB2CL27  EXEC  SQL CLOSE DB2CSR27
         BRU   DB2CLRC
*
         USING *,R9
DB2CL28  EXEC  SQL CLOSE DB2CSR28
         BRU   DB2CLRC
*
         USING *,R9
DB2CL29  EXEC  SQL CLOSE DB2CSR29
         BRU   DB2CLRC
*
         USING *,R9
DB2CL30  EXEC  SQL CLOSE DB2CSR30
         BRU   DB2CLRC
*
         USING *,R9
DB2CL31  EXEC  SQL CLOSE DB2CSR31
         BRU   DB2CLRC
*
         USING *,R9
DB2CL32  EXEC  SQL CLOSE DB2CSR32
         BRU   DB2CLRC
*
         USING *,R9
DB2CL33  EXEC  SQL CLOSE DB2CSR33
         BRU   DB2CLRC
*
         USING *,R9
DB2CL34  EXEC  SQL CLOSE DB2CSR34
         BRU   DB2CLRC
*
         USING *,R9
DB2CL35  EXEC  SQL CLOSE DB2CSR35
         BRU   DB2CLRC
*
         USING *,R9
DB2CL36  EXEC  SQL CLOSE DB2CSR36
         BRU   DB2CLRC
*
         USING *,R9
DB2CL37  EXEC  SQL CLOSE DB2CSR37
         BRU   DB2CLRC
*
         USING *,R9
DB2CL38  EXEC  SQL CLOSE DB2CSR38
         BRU   DB2CLRC
*
         USING *,R9
DB2CL39  EXEC  SQL CLOSE DB2CSR39
         BRU   DB2CLRC
*
static   loctr
         LTORG
code     loctr
*
         USING *,R9
DB2CL40  EXEC  SQL CLOSE DB2CSR40
         BRU   DB2CLRC
*
         USING *,R9
DB2CL41  EXEC  SQL CLOSE DB2CSR41
         BRU   DB2CLRC
*
         USING *,R9
DB2CL42  EXEC  SQL CLOSE DB2CSR42
         BRU   DB2CLRC
*
         USING *,R9
DB2CL43  EXEC  SQL CLOSE DB2CSR43
         BRU   DB2CLRC
*
         USING *,R9
DB2CL44  EXEC  SQL CLOSE DB2CSR44
         BRU   DB2CLRC
*
         USING *,R9
DB2CL45  EXEC  SQL CLOSE DB2CSR45
         BRU   DB2CLRC
*
         USING *,R9
DB2CL46  EXEC  SQL CLOSE DB2CSR46
         BRU   DB2CLRC
*
         USING *,R9
DB2CL47  EXEC  SQL CLOSE DB2CSR47
         BRU   DB2CLRC
*
         USING *,R9
DB2CL48  EXEC  SQL CLOSE DB2CSR48
         BRU   DB2CLRC
*
         USING *,R9
DB2CL49  EXEC  SQL CLOSE DB2CSR49
         BRU   DB2CLRC
*
         USING *,R9
DB2CL50  EXEC  SQL CLOSE DB2CSR50
         BRU   DB2CLRC
*
         USING *,R9
DB2CL51  EXEC  SQL CLOSE DB2CSR51
         BRU   DB2CLRC
*
         USING *,R9
DB2CL52  EXEC  SQL CLOSE DB2CSR52
         BRU   DB2CLRC
*
         USING *,R9
DB2CL53  EXEC  SQL CLOSE DB2CSR53
         BRU   DB2CLRC
*
         USING *,R9
DB2CL54  EXEC  SQL CLOSE DB2CSR54
         BRU   DB2CLRC
*
         USING *,R9
DB2CL55  EXEC  SQL CLOSE DB2CSR55
         BRU   DB2CLRC
*
         USING *,R9
DB2CL56  EXEC  SQL CLOSE DB2CSR56
         BRU   DB2CLRC
*
         USING *,R9
DB2CL57  EXEC  SQL CLOSE DB2CSR57
         BRU   DB2CLRC
*
         USING *,R9
DB2CL58  EXEC  SQL CLOSE DB2CSR58
         BRU   DB2CLRC
*
         USING *,R9
DB2CL59  EXEC  SQL CLOSE DB2CSR59
         BRU   DB2CLRC
*
static   loctr
         LTORG
code     loctr
*
         USING *,R9
DB2CL60  EXEC  SQL CLOSE DB2CSR60
         BRU   DB2CLRC
*
         USING *,R9
DB2CL61  EXEC  SQL CLOSE DB2CSR61
         BRU   DB2CLRC
*
         USING *,R9
DB2CL62  EXEC  SQL CLOSE DB2CSR62
         BRU   DB2CLRC
*
         USING *,R9
DB2CL63  EXEC  SQL CLOSE DB2CSR63
         BRU   DB2CLRC
*
         USING *,R9
DB2CL64  EXEC  SQL CLOSE DB2CSR64
         BRU   DB2CLRC
*
         USING *,R9
DB2CL65  EXEC  SQL CLOSE DB2CSR65
         BRU   DB2CLRC
*
         USING *,R9
DB2CL66  EXEC  SQL CLOSE DB2CSR66
         BRU   DB2CLRC
*
         USING *,R9
DB2CL67  EXEC  SQL CLOSE DB2CSR67
         BRU   DB2CLRC
*
         USING *,R9
DB2CL68  EXEC  SQL CLOSE DB2CSR68
         BRU   DB2CLRC
*
         USING *,R9
DB2CL69  EXEC  SQL CLOSE DB2CSR69
         BRU   DB2CLRC
*
         USING *,R9
DB2CL70  EXEC  SQL CLOSE DB2CSR70
         BRU   DB2CLRC
*
         USING *,R9
DB2CL71  EXEC  SQL CLOSE DB2CSR71
         BRU   DB2CLRC
*
         USING *,R9
DB2CL72  EXEC  SQL CLOSE DB2CSR72
         BRU   DB2CLRC
*
         USING *,R9
DB2CL73  EXEC  SQL CLOSE DB2CSR73
         BRU   DB2CLRC
*
         USING *,R9
DB2CL74  EXEC  SQL CLOSE DB2CSR74
         BRU   DB2CLRC
*
         USING *,R9
DB2CL75  EXEC  SQL CLOSE DB2CSR75
         BRU   DB2CLRC
*
         USING *,R9
DB2CL76  EXEC  SQL CLOSE DB2CSR76
         BRU   DB2CLRC
*
         USING *,R9
DB2CL77  EXEC  SQL CLOSE DB2CSR77
         BRU   DB2CLRC
*
         USING *,R9
DB2CL78  EXEC  SQL CLOSE DB2CSR78
         BRU   DB2CLRC
*
         USING *,R9
DB2CL79  EXEC  SQL CLOSE DB2CSR79
         BRU   DB2CLRC
*
static   loctr
         LTORG
code     loctr
*
         USING *,R9
DB2CL80  EXEC  SQL CLOSE DB2CSR80
         BRU   DB2CLRC
*
         USING *,R9
DB2CL81  EXEC  SQL CLOSE DB2CSR81
         BRU   DB2CLRC
*
         USING *,R9
DB2CL82  EXEC  SQL CLOSE DB2CSR82
         BRU   DB2CLRC
*
         USING *,R9
DB2CL83  EXEC  SQL CLOSE DB2CSR83
         BRU   DB2CLRC
*
         USING *,R9
DB2CL84  EXEC  SQL CLOSE DB2CSR84
         BRU   DB2CLRC
*
         USING *,R9
DB2CL85  EXEC  SQL CLOSE DB2CSR85
         BRU   DB2CLRC
*
         USING *,R9
DB2CL86  EXEC  SQL CLOSE DB2CSR86
         BRU   DB2CLRC
*
         USING *,R9
DB2CL87  EXEC  SQL CLOSE DB2CSR87
         BRU   DB2CLRC
*
         USING *,R9
DB2CL88  EXEC  SQL CLOSE DB2CSR88
         BRU   DB2CLRC
*
         USING *,R9
DB2CL89  EXEC  SQL CLOSE DB2CSR89
         BRU   DB2CLRC
*
         USING *,R9
DB2CL90  EXEC  SQL CLOSE DB2CSR90
         BRU   DB2CLRC
*
         USING *,R9
DB2CL91  EXEC  SQL CLOSE DB2CSR91
         BRU   DB2CLRC
*
         USING *,R9
DB2CL92  EXEC  SQL CLOSE DB2CSR92
         BRU   DB2CLRC
*
         USING *,R9
DB2CL93  EXEC  SQL CLOSE DB2CSR93
         BRU   DB2CLRC
*
         USING *,R9
DB2CL94  EXEC  SQL CLOSE DB2CSR94
         BRU   DB2CLRC
*
         USING *,R9
DB2CL95  EXEC  SQL CLOSE DB2CSR95
         BRU   DB2CLRC
*
         USING *,R9
DB2CL96  EXEC  SQL CLOSE DB2CSR96
         BRU   DB2CLRC
*
         USING *,R9
DB2CL97  EXEC  SQL CLOSE DB2CSR97
         BRU   DB2CLRC
*
         USING *,R9
DB2CL98  EXEC  SQL CLOSE DB2CSR98
         BRU   DB2CLRC
*
         USING *,R9
DB2CL99  EXEC  SQL CLOSE DB2CSR99
         BRU   DB2CLRC
*
static   loctr
         LTORG
*
         DROP  R9
*
         DROP  R5
         DROP  R6
         DROP  R7
                        EJECT
         DCBD  DSORG=PS
                        SPACE 3
GVBUR30  CSECT
*
         EXEC  SQL INCLUDE SQLDA
                        SPACE 3
GVBUR30  CSECT
*
         END
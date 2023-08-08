         TITLE 'GVBUR40 - HARDWARE COMPRESSION/EXPANSION UTILITY'
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
*
*   GVBUR40 - COMPRESS/DECOMPRESS A SPECIFIED RECORD.
*
*   INPUT:   - PARAMETER LIST.
*   OUTPUT:  - PARAMETER LIST.
*
*   RETURN:
*    NORMAL  - REGISTER FIFTEEN SET TO ZERO.
*    ERROR   - R15=08 - BAD FUNCTION CODE.
*            - R15=12 - BAD EXPANDED LENGTH.
*            - R15=16 - COMPRESSION ERROR.
*            - R15=20 - EXPANSION ERROR.
*            - R15=24 - DICTIONARY NOT LOADED.
*            - R15=28 - DICTIONARY NOT DELETED.
*
*   REGISTER USAGE:
*        R15 - STANDARD LINKAGE/TEMPORARY WORK
*        R14 - STANDARD LINKAGE/TEMPORARY WORK
*        R13 - STANDARD LINKAGE
*        R12 - PROGRAM BASE REGISTER
*        R11 - RESERVED FOR 2ND PROGRAM BASE
*        R10 - 1ST LEVEL INTERNAL SUBROUTINE LINKAGE
*        R9  - CALLING INTERFACE
*        R8  - NOT USED
*        R7  - NOT USED
*        R6  - NOT USED
*        R5  - USED BY CMPSC
*        R4  - USED BY CMPSC
*        R3  - USED BY CMPSC
*        R2  - USED BY CMPSC
*        R1  - USED BY CMPSC/STANDARD LINKAGE
*        R0  - USED BY CMPSC/TEMPORARY WORK
*
*---------------------------------------------------------------------*
*        DYNAMIC STORAGE
*---------------------------------------------------------------------*
WORKSTOR DSECT                         *------------------------------*
*                                      * WORKING STORAGE
*                                      *------------------------------*
SAVEAREA DS  18F                       REGISTER SAVE AREA
*
WKTOKNRC DS    A                       NAME/TOKEN  SERVICES RETN CODE
WKTOKNAM DS    XL16                    TOKEN NAME
WKTOKN   DS   0XL16                    TOKEN
CMPR_PTR DS    A                       COMPRESSION DICTIONARY
EXPD_PTR DS    A                       EXPANSION   DICTIONARY
         DS   2A                       (RESERVED)
*
         DS   0D
WORKAREA DS    CL128                   GENERALIZED WORK AREA
WKREENT  DS    XL128                   RE-ENTRANT  PARAMETER  LIST
DICTDCB  DS    (DICTLEN)C              DCB DATA
WORKSLEN EQU   (*-WORKSTOR)            DYNAMIC STORAGE LENGTH
                        EJECT
*---------------------------------------------------------------------*
*        CALLING INTERFACE
*---------------------------------------------------------------------*
PARMAREA DSECT (R9)                    PARAMETER AREA
PARMANCH DS    A                        - REENTRANT STORAGE ANCHOR
PARMCMPA DS    A                        - EXPANDED RECORD AREA PTR
PARMEXPA DS    A                        - COMPRESSED RECORD AREA PTR
PARMDFMT DS    F                        - DICTIONARY FORMAT
PARMDNME DS    CL08                     - DICTIONARY NAME
PARMCMPL DS    H                        - EXPANDED RECORD LENGTH
PARMEXPL DS    H                        - COMPRESSED RECORD LENGTH
PARMRC   DS    H                        - RETURN CODE
PARMFCN  DS    CL01                     - FUNCTION CODE ('C' 'D' 'T')
PARMLEN  EQU   (*-PARMAREA)
                        EJECT
*---------------------------------------------------------------------*
*        EQUATED VALUES
*---------------------------------------------------------------------*
         YREGS                         REGISTER EQUATES
*
RSABP    EQU   04,4                    SAVE AREA OFFSETS
RSAFP    EQU   08,4
RSA14    EQU   12,4
RSA15    EQU   16,4
RSA0     EQU   20,4
RSA1     EQU   24,4
                        EJECT
         PRINT GEN
*                                      GLOBAL REGISTER ASSIGNMENTS
         USING WORKSTOR,R13             - DYNAMIC STORAGE
         USING GVBUR40,R12               - PROGRAM BASE(S)
         USING PARMAREA,R9              - CALLING INTERFACE
*
GVBUR40  CSECT
GVBUR40  AMODE 31
GVBUR40  RMODE ANY
         J     START
UR40EYE  GVBEYE GVBUR40
*
START    EQU   *
         STM   R14,R12,RSA14(R13)      SAVE CALLER'S  REGS
         LA    R12,0(,R15)             SET  PROGRAM   BASE
*
         LR    R8,R1                   SAVE PARAMETER LIST
         L     R9,0(,R8)               -->  PARAMETER AREA
         L     R1,PARMANCH             -->  STORAGE ANCHOR
         LTR   R1,R1                   FIRST ENTRY?
         BNZ   CONTINUE                N - CONTINUE
*
         L     R0,=A(WORKSLEN)         OBTAIN DYNAMIC STORAGE
         GETMAIN R,LV=(0),LOC=BELOW
*
         ST    R1,PARMANCH             ANCHOR THE WORK AREA
         LR    R2,R1                   CLEAR GOTTEN STORAGE
         L     R3,=A(WORKSLEN)
         SLL   R4,32
         SLL   R5,32
         MVCL  R2,R4
*
         ST    R1,RSAFP(,R13)          CHAIN THE SAVE AREAS
         ST    R13,RSABP(,R1)
         LR    R13,R1                  --> DYNAMIC STORAGE
         BAS   R10,INIT0100            DO THE INITIALIZATION
         B     MAIN0100
*
CONTINUE EQU   *
         ST    R1,RSAFP(,R13)          CHAIN THE SAVE AREAS
         ST    R13,RSABP(,R1)
         LR    R13,R1                  --> DYNAMIC STORAGE
                        EJECT
MAIN0100 EQU   *
         CLI   PARMFCN,C'C'            COMPRESSION REQUESTED?
         BE    MAIN0300                Y - CONTINUE
         CLI   PARMFCN,C'D'            EXPANSION REQUESTED?
         BNE   TERM0100                N - CHECK FOR TERMINATION
*
MAIN0200 EQU   *
         L     R4,PARMCMPA             --> RECORD TO EXPAND
         LH    R5,PARMCMPL             LENGTH OF RECORD
         L     R2,PARMEXPA             --> RECORD RETURN AREA
         LH    R3,=H'32760'            MAX EXPANSION LENGTH
         L     R0,PARMDFMT             DICTIONARY FORMAT
         O     R0,=X'00000100'         INDICATE EXPANSION
         L     R1,EXPD_PTR             --> DICTIONARY
*
MAIN0220 EQU   *
         CMPSC R2,R4                   ACTIVATE EXPANSION
         BC    8,MAIN0240               - CONTINUE IF SUCCESSFUL
         BC    1,MAIN0220               - REPEAT IF CPU INTERRUPTED
         B     ERROR#03
*
MAIN0240 EQU   *
         L     R1,PARMEXPA             CALC THE RECORD LENGTH
         SR    R2,R1
         CH    R2,=H'32760'            VALID EXPANDED LENGTH?
         BH    ERROR#04                N - ERROR
         STH   R2,PARMEXPL             RETURN EXPANDED LENGTH
         B     MAIN0900
*
MAIN0300 EQU   *
         L     R4,PARMEXPA             --> RECORD TO COMPRESS
         LH    R5,PARMEXPL             LENGTH OF RECORD
         L     R2,PARMCMPA             --> RECORD RETURN AREA
         LH    R3,=H'32760'            MAX COMPRESSION LENGTH
         L     R0,PARMDFMT             DICTIONARY FORMAT
         N     R0,=X'FFFFFEFF'         INDICATE COMPRESSION
         L     R1,CMPR_PTR             --> DICTIONARY
*
MAIN0320 EQU   *
         CMPSC R2,R4                   ACTIVATE COMPRESSION
         BC    8,MAIN0340               - CONTINUE IF SUCCESSFUL
         BC    1,MAIN0320               - REPEAT IF CPU INTERRUPTED
         B     ERROR#02
*
MAIN0340 EQU   *
         L     R1,PARMCMPA             CALC THE RECORD LENGTH
         LA    R2,1(,R2)
         SR    R2,R1
         STH   R2,PARMCMPL             COMPRESSED RECORD LENGTH
                        EJECT
MAIN0900 EQU   *
         SLL   R15,32                  PROBABLY A GOOD RETURN
*
RETURN   EQU   *
         STH   R15,PARMRC              SAVE THE RETURN CODE
         L     R13,RSABP(,R13)         --> CALLER'S SAVE AREA
         L     R14,RSA14(,R13)         RESTORE CALLER'S REGS
         LM    R0,R12,RSA0(R13)
         BSM   0,R14
*
GOBACK   EQU   *
         STH   R15,PARMRC              SAVE THE RETURN CODE
         LR    R1,R13                  CURRENT SAVE AREA
         LR    R2,R15                  SAVE THE RETURN CODE
         L     R13,RSABP(,R13)         --> CALLER'S SAVE AREA
*
         L     R0,=A(WORKSLEN)         RELEASE DYNAMIC STORAGE
         FREEMAIN R,LV=(0),A=(1)
         XC    RSAFP(L'RSAFP,R13),RSAFP(R13)
*
         LR    R15,R2                  SET THE RETURN CODE
         L     R14,RSA14(,R13)         RESTORE CALLER'S REGS
         LM    R0,R12,RSA0(R13)
         BSM   0,R14
                        EJECT
*---------------------------------------------------------------------*
*        PERFORM THE TERMINATION PROCESSING
*---------------------------------------------------------------------*
TERM0100 EQU   *
         CLI   PARMFCN,C'T'            TERMINATION REQUEST?
         BNE   ERROR#01                N - ERROR
*
         L     R2,CMPR_PTR             --> DICTIONARY
         DELETE EPLOC=(R2)             REMOVE FROM VS
         LTR   R15,R15                 SUCCESSFULL DELETE?
         BNZ   ERROR#06                N - ERROR
*
         B     GOBACK                  RETURN TO CALLER
                        EJECT
*---------------------------------------------------------------------*
*        PERFORM THE INTIALIZATION PROCESSING
*---------------------------------------------------------------------*
INIT0100 EQU   *
         MVC   WKTOKNAM+0(8),GENEVA
         MVC   WKTOKNAM+8(8),PARMDNME  DICTIONARY NAME
*
         CALL  IEANTRT,(TOKNLVL2,WKTOKNAM,WKTOKN,WKTOKNRC),            X
               MF=(E,WKREENT)
         L     R15,WKTOKNRC            RETRIEVE SUCCESSFUL ???
         LTR   R15,R15
         BZ    INITEXIT                Y - EXIT
*
         ENQ   (GENEVA,PGMNAME,E,,STEP),RNL=NO
         LTR   R15,R15
         BNZ   ERROR#10
                        SPACE 3
         CALL  IEANTRT,(TOKNLVL2,WKTOKNAM,WKTOKN,WKTOKNRC),            X
               MF=(E,WKREENT)
         L     R15,WKTOKNRC            RETRY SUCCESSFUL ???
         LTR   R15,R15
         BZ    INITDEQ                 Y - EXIT
*
         LA    R2,PARMDNME             --> DICTIONARY TO USE
         LTR   R9,R9                   CICS PARAMETER PRESENT  ???
         BM    INIT0300                Y -  LOAD FROM "DFHRPL"
*
         L     R14,4(,R8)              LOAD CICS PARM ADDRESS
         CLI   0(R14),C'Y'
         BNE   INIT0300
*
INIT0200 MVC   WKREENT(8),OPENPARM
         MVC   DICTDCB(DICTLEN),DICTLIB
         OPEN  (DICTDCB,(INPUT)),MF=(E,WKREENT),MODE=31
         TM    DICTDCB+48,X'10'        SUCCESSFUL ???
         BNO   ERROR#07
*
         LOAD  EPLOC=(R2),DCB=DICTDCB  LOAD THE DICTIONARY
         LTR   R15,R15                 DICTIONARY LOCATED?
         BNZ   ERROR#05                N - ERROR
*
         LR    R2,R0
         LR    R3,R1
         CLOSE (DICTDCB),MF=(E,WKREENT),MODE=31
         LR    R0,R2
         LR    R1,R3
         B     INIT0400
*
INIT0300 LOAD  EPLOC=(R2)              LOAD THE DICTIONARY
         LTR   R15,R15                 DICTIONARY LOCATED?
         BNZ   ERROR#05                N - ERROR
*
INIT0400 ST    R0,CMPR_PTR             COMPRESSION DICTIONARY
         SLL   R1,16                   CALC OFFSET TO EXP DICT
         SRL   R1,14
         AR    R0,R1
         ST    R0,EXPD_PTR             EXPANSION DICTIONARY
         SPACE 3
         CALL  IEANTCR,(TOKNLVL2,WKTOKNAM,WKTOKN,TOKNPERS,WKTOKNRC),   X
               MF=(E,WKREENT)
         L     R15,WKTOKNRC            NAME/TOKEN CREATE SUCCESSFUL ???
         LTR   R15,R15
         BNZ   ERROR#09                N - ERROR
*
INITDEQ  DEQ   (GENEVA,PGMNAME,,STEP),RNL=NO
*
INITEXIT BR    R10
                        EJECT
ERROR#01 EQU   *
         LA    R15,8                   BAD FUNCTION CODE
         B     GOBACK
ERROR#02 EQU   *
         LA    R15,16                  COMPRESSION FAILURE
         B     GOBACK
ERROR#03 EQU   *
         LA    R15,20                  EXPANSION FAILURE
         B     GOBACK
ERROR#04 EQU   *
         LA    R15,12                  BAD EXPANDED LENGTH
         B     GOBACK
ERROR#05 EQU   *
         DEQ   (GENEVA,PGMNAME,,STEP),RNL=NO
         LA    R15,24                  DICTIONARY NOT FOUND
         B     GOBACK
ERROR#06 EQU   *
         LA    R15,28                  DICTIONARY NOT DELETED
         B     GOBACK
ERROR#07 EQU   *
         DEQ   (GENEVA,PGMNAME,,STEP),RNL=NO
         LA    R15,32                  CICS LOADLIB NOT OPENED
         B     GOBACK
ERROR#08 EQU   *
         LA    R15,36                  NAME/TOKEN RETRIEVE FAILED
         B     GOBACK
ERROR#09 EQU   *
         DEQ   (GENEVA,PGMNAME,,STEP),RNL=NO
         LA    R15,40                  NAME/TOKEN CREATE   FAILED
         B     GOBACK
ERROR#10 EQU   *
         LA    R15,44                  ENQUEUE    FAILED
         B     GOBACK
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*        C O N S T A N T S
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
DICTLIB  DCB   DDNAME=DFHRPL,DSORG=PO,MACRF=R
DICTLEN  EQU   (*-DICTLIB)             DYNAMIC STORAGE LENGTH
OPENPARM DC    XL8'8000000000000000'
*
TOKNLVL2 DC    A(2)                    NAME/TOKEN  AVAILABILITY  LEVEL
TOKNPERS DC    F'0'                 TOKEN PERSISTENCE
GENEVA   DC    CL8'GENEVA'                  TOKEN  NAME
PGMNAME  DC    CL8'GVBUR40'            MINOR  ENQ  NAME
*
         LTORG
*
UR40VERS CSECT
UR40VERS AMODE 31
UR40VERS RMODE ANY
*
         END

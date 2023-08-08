         TITLE 'GVBUR66 - COBOL ENQ/DEQ INTERFACE'
**********************************************************************
*
* (C) COPYRIGHT IBM CORPORATION 2003, 2021.
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
*
*   GVBUR66 - GENERALIZED ENQ/DEQ INTERFACE FOR COBOL PROGRAMS.
*
*   PARAMETERS: STANDARD LINKAGE WITH REGISTER ONE POINTING TO THE
*               UR66AREA CALLING INTERFACE.  REFER TO THE GVBCUR66
*               COBOL COPYBOOK FOR MORE DOCUMENTATION.
*
*
*   RETURN:
*    NORMAL  - RETURN CODE (R15) = 0 --> SUCCESSFUL REQUEST.
*    ERROR   - A MESSAGE IS WRITTEN TO THE JOB LOG AND A USER
*              ABEND (66) IS ISSUED WHEN A SPECIFICATION ERROR
*              HAS BEEN DETECTED.
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*   R E G I S T E R  U S A G E
*
*   R15   -  STANDARD LINKAGE/WORK REGISTER
*   R14   -  STANDARD LINKAGE/WORK REGISTER
*   R13   -  STANDARD LINKAGE/DYNAMIC STORAGE BASE
*   R12   -  1ST PROGRAM BASE
*   R11   -  RESERVED
*   R10   -  INTERNAL LINKAGE
*   R9    -  UR66AREA CALLING INTERFACE PTR
*   R8    -  WORK REGISTER
*   R7    -  WORK REGISTER
*   R6    -  WORK REGISTER
*   R5    -  WORK REGISTER
*   R4    -  MINOR NAME PTR
*   R3    -  MAJOR NAME PTR
*   R2    -  STANDARD USAGE/WORK REGISTER
*   R1    -  STANDARD LINKAGE/WORK REGISTER
*   R0    -  STANDARD LINKAGE/WORK REGISTER
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
                        EJECT
         YREGS
*
RSABP    EQU   04,4
RSAFP    EQU   08,4
RSA14    EQU   12,4
RSA15    EQU   16,4
RSA0     EQU   20,4
RSA1     EQU   24,4
*
         COPY  GVBUTEQU
*
         USING WORKSTOR,R13             - DYNAMIC STORAGE
         USING GVBUR66,R12              - PROGRAM BASE(S)
         USING UR66AREA,R9              - CALLING INTERFACE
*
                        EJECT
GVBUR66  CSECT
GVBUR66  AMODE 31
GVBUR66  RMODE ANY
         J     START
UR66EYE  GVBEYE GVBUR66
*
START    EQU   *
         STM   R14,R12,RSA14(R13)      SAVE CALLER'S REGS
         LA    R12,0(,R15)             SET PROGRAM BASE
*
         L     R9,0(,R1)               --> PARAMETER LIST
*
         LHI   R0,WORKSLEN             OBTAIN DYNAMIC STORAGE
         GETMAIN R,LV=(0),LOC=BELOW
*
         LR    R2,R1                   CLEAR GOTTEN STORAGE
         LHI   R3,WORKSLEN
         SLL   R4,32
         SLL   R5,32
         MVCL  R2,R4
*
         ST    R1,RSAFP(,R13)          CHAIN THE SAVE AREAS
         ST    R13,RSABP(,R1)
         LR    R13,R1                  --> DYNAMIC STORAGE
                        EJECT
MAIN0100 EQU   *
         CLI   UR66RNAM,X'40'          RNAME SPECIFIED?
         BNH   ERROR#03                N - ERROR
         CLI   UR66QNAM,X'40'          QNAME SPECIFIED?
         BNH   ERROR#04                N - ERROR
         LA    R3,UR66QNAM             --> MAJOR NAME
         LA    R4,UR66RNAM             --> MINOR NAME
         CLC   =C'ENQ',UR66RTYP        ENQ REQUEST?
         BNE   MAIN0200                N - SEE IF DEQ
         MVC   WORKAREA(ENQM1LEN),ENQMODL1
         CLI   UR66CNTR,C'E'           EXCLUSIVE REQUEST?
         BE    MAIN0120                Y - CHECK OTHER OPTIONS
         CLI   UR66CNTR,C'S'           SHARED REQUEST?
         BNE   ERROR#02                N - ERROR
         OI    WORKAREA+2,X'80'        INDICATE SHARED
*
MAIN0120 EQU   *
         CLI   UR66SCOP,C'1'           SCOPE OF 'STEP'?
         BE    MAIN0190                Y - ALREADY SPECIFIED
         OI    WORKAREA+2,X'40'        INDICATE SYSTEM SCOPE
         CLI   UR66SCOP,C'2'           SCOPE OF 'SYSTEM'?
         BE    MAIN0190                Y - DO THE ENQUEUE
         OI    WORKAREA+2,X'08'        INDICATE SYSTEMS SCOPE
         CLI   UR66SCOP,C'3'           SCOPE OF 'SYSTEMS'?
         BNE   ERROR#05                N - ERROR
*
MAIN0190 EQU   *
         ENQ   ((R3),(R4),,,,),MF=(E,WORKAREA)
*
         B     MAIN0900                RETURN TO THE CALLER
*
MAIN0200 EQU   *
         CLC   =C'DEQ',UR66RTYP        DEQ REQUEST?
         BNE   ERROR#01                N - ERROR
         MVC   WORKAREA(DEQM1LEN),DEQMODL1
*
MAIN0220 EQU   *
         CLI   UR66SCOP,C'1'           SCOPE OF 'STEP'?
         BE    MAIN0290                Y - ALREADY SPECIFIED
         OI    WORKAREA+2,X'40'        INDICATE SYSTEM SCOPE
         CLI   UR66SCOP,C'2'           SCOPE OF 'SYSTEM'?
         BE    MAIN0290                Y - DO THE DEQUEUE
         OI    WORKAREA+2,X'08'        INDICATE SYSTEMS SCOPE
         CLI   UR66SCOP,C'3'           SCOPE OF 'SYSTEMS'?
         BNE   ERROR#05                N - ERROR
*
MAIN0290 EQU   *
         DEQ   ((R3),(R4),,,),MF=(E,WORKAREA)
*
MAIN0900 EQU   *
         SLL   R15,32                  PROBABLY A GOOD REQUEST
*
RETURN   EQU   *
         LR    R1,R13                  CURRENT SAVE AREA
         LR    R2,R15                  SAVE THE RETURN CODE
         L     R13,RSABP(,R13)         --> CALLER'S SAVE AREA
*
         LHI   R0,WORKSLEN             RELEASE DYNAMIC STORAGE
         FREEMAIN R,LV=(0),A=(1)
         XC    RSAFP(L'RSAFP,R13),RSAFP(R13)
*
         LR    R15,R2                  SET THE RETURN CODE
         L     R14,RSA14(,R13)         RESTORE CALLER'S REGS
         LM    R0,R12,RSA0(R13)
         BSM   0,R14
                        EJECT
*-------------------------------------------------------------------*
*        ISSUE AN ERROR MESSAGE AND TERMINATE
*-------------------------------------------------------------------*
EROR0100 EQU   *
*
         GVBMSG WTO,MSGNO=(r14),SUBNO=1,                               +
               SUB1=(modname,8),                                       +
               MF=(E,MSG_AREA)
         B     ABEND066
*
EROR0102 EQU   *
* message with 2 parameters
         GVBMSG WTO,MSGNO=(r14),SUBNO=2,                               +
               SUB1=(modname,8),                                       +
               SUB2=(UR66RTYP,L'UR66RTYP),                             +
               MF=(E,MSG_AREA)
*
ABEND066 DS    0H
         ABEND 66,DUMP                 REGURGITATE
*
*
ERROR#01 EQU   *
         LA    R14,UR66_REQ_ERR1       MESSAGE NUMBER
         B     EROR0100                ISSUE AN ERROR
ERROR#02 EQU   *
         LA    R14,UR66_REQ_ERR2       MESSAGE NUMBER
         B     EROR0100                ISSUE AN ERROR
ERROR#03 EQU   *
         LA    R14,UR66_REQ_ERR3       MESSAGE NUMBER
         B     EROR0102                ISSUE AN ERROR
ERROR#04 EQU   *
         LA    R14,UR66_REQ_ERR4       MESSAGE NUMBER
         B     EROR0102                ISSUE AN ERROR
ERROR#05 EQU   *
         LA    R14,UR66_REQ_ERR5       MESSAGE NUMBER
         B     EROR0102                ISSUE AN ERROR
                        EJECT
STATSTOR DS    0D                      *----------------------------*
*                                      * STATIC STORAGE
         DC    CL8'GVBUR66'            *----------------------------*
*
ENQMODL1 DS    0F                      MODEL FOR 1ST ENQ
         ENQ   (,,E,L'UR66RNAM,STEP),RNL=NO,MF=L
ENQM1LEN EQU   (*-ENQMODL1)
*
DEQMODL1 DS    0F                      MODEL FOR 1ST DEQ
         DEQ   (,,L'UR66RNAM,STEP),RNL=NO,MF=L
DEQM1LEN EQU   (*-DEQMODL1)
*
         LTORG
                        EJECT
WORKSTOR DSECT                         *----------------------------*
*                                      * WORKING STORAGE
*                                      *----------------------------*
*
SAVEAREA DS    18F                     REGISTER SAVE AREA
*
MSG_AREA GVBMSG MF=L
*
         DS    0D
WORKAREA DS    CL128                   GENERALIZED WORK AREA
WORKSLEN EQU   (*-WORKSTOR)            DYNAMIC STORAGE LENGTH
                        EJECT
         GVBAUR66 DSECT=YES            UR66 CALLING INTERFACE
*
         END
